use rand::Rng;
use std::{cell::RefCell, collections::HashMap, path::Path, rc::Rc, str::FromStr, time::Duration};

use crate::proto::{
    common::{
        components::{BatteryType, ComponentCategory, EvChargerType, InverterType},
        metrics::{
            electrical::{ac::AcPhase, Ac, Dc},
            Bounds, Metric, MetricAggregation,
        },
    },
    microgrid::{
        battery, component, component_data, ev_charger, grid, inverter, meter, Component,
        ComponentData, ComponentList, Connection, ConnectionList, MicrogridMetadata, Location,
    },
};
use notify::{RecommendedWatcher, Watcher};
use prost_types::Timestamp;
use tulisp::{destruct_bind, intern, list, Error, ErrorKind, TulispContext, TulispObject};

type CompDataMaker =
    fn(&mut TulispContext, &TulispObject, &Symbols) -> Result<ComponentData, Error>;

intern! {
    #[derive(Clone)]
    pub(crate) struct Symbols {
        id: "id",
        soc: "soc",
        name: "name",
        data: "data",
        type_: "type",
        power: "power",
        stream: "stream",
        voltage: "voltage",
        current: "current",
        category: "category",
        interval: "interval",
        capacity: "capacity",
        location: "location",
        metadata: "metadata",
        soc_lower: "soc-lower",
        soc_upper: "soc-upper",
        relay_state: "relay-state",
        cable_state: "cable-state",
        socket_addr: "socket-addr",
        ac_frequency: "ac-frequency",
        microgrid_id: "microgrid-id",
        inclusion_lower: "inclusion-lower",
        inclusion_upper: "inclusion-upper",
        exclusion_lower: "exclusion-lower",
        exclusion_upper: "exclusion-upper",
        per_phase_power: "per-phase-power",
        component_state: "component-state",
        components_alist: "components-alist",
        set_power_active: "set-power-active",
        connections_alist: "connections-alist",
        rated_fuse_current: "rated-fuse-current",
        state_update_functions: "state-update-functions",
        state_update_interval_ms: "state-update-interval-ms",
        retain_requests_duration_ms: "retain-requests-duration-ms",
    }
}

#[derive(Clone)]
pub struct Config {
    filename: String,

    pub(crate) ctx: Rc<RefCell<tulisp::TulispContext>>,

    /// Component ID -> (Component's Data Method, Interval, To ComponentData Method)
    stream_methods: Rc<RefCell<HashMap<u64, (TulispObject, u64, CompDataMaker)>>>,

    /// Component ID -> last power update time.
    last_formula_update_time: Rc<RefCell<std::time::Instant>>,

    symbols: Symbols,
}

// Tokio is configured to use the current_thread runtime, so it is not unsafe to
// make `Config` Send and Sync.
unsafe impl Send for Config {}
unsafe impl Sync for Config {}

macro_rules! alist_get_as {
    ($ctx: expr, $rest:expr, $key:expr, $as_fn:ident) => {{
        alist_get_as!($ctx, $rest, $key).and_then(|x| x.$as_fn())
    }};
    ($ctx: expr, $rest:expr, $key:expr, eval++$as_fn:ident) => {{
        let out = alist_get_as!($ctx, $rest, $key);
        out.and_then(|x| $ctx.eval_and_then(&x, |x| x.$as_fn()))
    }};
    ($ctx: expr, $rest:expr, $key:expr) => {{
        tulisp::lists::alist_get($ctx, $key, $rest, None, None, None)
    }};
}

macro_rules! alist_get_f32 {
    ($ctx: expr, $rest:expr, $key:expr) => {
        alist_get_as!($ctx, $rest, $key, eval ++ try_float).unwrap_or_default() as f32
    };
}

macro_rules! alist_get_u32 {
    ($ctx: expr, $rest:expr, $key:expr) => {
        alist_get_as!($ctx, $rest, $key, eval ++ try_int).unwrap_or_default() as u32
    };
}

macro_rules! alist_get_3_phase {
    ($ctx: expr, $rest:expr, $key:expr) => {{
        let expr = alist_get_as!($ctx, $rest, $key).unwrap_or_default();
        let items = if expr.consp() && expr.car_and_then(|x| Ok(x.numberp()))? {
            expr
        } else {
            $ctx.eval(&expr)?
        };
        (
            items
                .car()
                .and_then(|x| $ctx.eval_and_then(&x, |x| x.as_float()))
                .unwrap_or_default() as f32,
            items
                .cadr()
                .and_then(|x| $ctx.eval_and_then(&x, |x| x.as_float()))
                .unwrap_or_default() as f32,
            items
                .caddr()
                .and_then(|x| $ctx.eval_and_then(&x, |x| x.as_float()))
                .unwrap_or_default() as f32,
        )
    }};
}

fn enum_from_alist<T: FromStr + Default>(
    ctx: &mut TulispContext,
    alist: &TulispObject,
    key: &TulispObject,
    eval: bool,
) -> Option<T> {
    let val = if eval {
        alist_get_as!(ctx, alist, key, eval ++ as_symbol).ok()?
    } else {
        alist_get_as!(ctx, alist, key, as_symbol).ok()?
    };
    match val.parse::<T>() {
        Ok(x) => Some(x),
        Err(_) => {
            log::error!("Invalid value for {}: {}", key, val);
            None
        }
    }
}

fn make_component_from_alist(
    ctx: &mut TulispContext,
    alist: &TulispObject,
    symbols: &Symbols,
) -> Result<Component, Error> {
    let id = alist_get_as!(ctx, alist, &symbols.id, as_int)? as u64;
    let name = alist_get_as!(ctx, alist, &symbols.name, as_string).unwrap_or_default();
    let Some(category) = enum_from_alist::<ComponentCategory>(ctx, alist, &symbols.category, false)
    else {
        return Err(Error::new(
            tulisp::ErrorKind::Uninitialized,
            format!("Invalid component category for component {}", id),
        ));
    };

    let metadata = match category {
        ComponentCategory::Inverter => {
            enum_from_alist::<InverterType>(ctx, alist, &symbols.type_, false)
                .map(|typ| component::Metadata::Inverter(inverter::Metadata { r#type: typ as i32 }))
        }
        ComponentCategory::Battery => {
            enum_from_alist::<BatteryType>(ctx, alist, &symbols.type_, false)
                .map(|typ| component::Metadata::Battery(battery::Metadata { r#type: typ as i32 }))
        }
        ComponentCategory::EvCharger => {
            enum_from_alist::<EvChargerType>(ctx, alist, &symbols.type_, false).map(|typ| {
                component::Metadata::EvCharger(ev_charger::Metadata { r#type: typ as i32 })
            })
        }
        ComponentCategory::Grid => Some(component::Metadata::Grid(grid::Metadata {
            rated_fuse_current: alist_get_u32!(ctx, alist, &symbols.rated_fuse_current),
        })),
        _ => None,
    };

    let comp = Component {
        id,
        name,
        category: category as i32,
        metadata,

        ..Default::default()
    };
    Ok(comp)
}

impl Config {
    pub fn new(filename: &str) -> Self {
        let mut ctx = tulisp::TulispContext::new();
        add_functions(&mut ctx);

        let _ = ctx.eval_file(filename).map_err(|e| {
            log::error!("Tulisp error:\n{}", e.format(&ctx));
            e
        });
        let now = std::time::Instant::now();
        let symbols = Symbols::new(&mut ctx);
        Self {
            filename: filename.to_string(),
            ctx: Rc::new(RefCell::new(ctx)),
            stream_methods: Rc::new(RefCell::new(HashMap::new())),
            last_formula_update_time: Rc::new(RefCell::new(now)),
            symbols,
        }
    }

    pub fn reload(&self) {
        let start = std::time::Instant::now();
        let mut ctx = self.ctx.borrow_mut();
        if ctx
            .eval_file(&self.filename)
            .map_err(|e| {
                log::error!("Tulisp error:\n{}", e.format(&ctx));
                e
            })
            .is_err()
        {
            return;
        }
        let duration = start.elapsed();
        log::info!(
            "Reloaded config file in {}ms",
            duration.as_nanos() as f64 / 1e6
        );
        *self.stream_methods.borrow_mut() = HashMap::new();
    }

    pub async fn start(self) {
        self.start_state_updates();
        self.start_watching().await;
    }

    async fn start_watching(self) {
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);

        let mut watcher = RecommendedWatcher::new(
            move |res| {
                futures::executor::block_on(async {
                    tx.send(res).await.unwrap();
                });
            },
            notify::Config::default(),
        )
        .unwrap();
        watcher
            .watch(
                &Path::new(&self.filename),
                notify::RecursiveMode::NonRecursive,
            )
            .unwrap();

        while let Some(res) = rx.recv().await {
            match res {
                Ok(event) => {
                    if let notify::EventKind::Modify(_) = event.kind {
                        tokio::time::sleep(Duration::from_millis(50)).await;
                        self.reload();
                    }
                }
                Err(e) => {
                    log::error!("watch error: {:?}", e);
                    return;
                }
            }
        }
    }

    fn start_state_updates(&self) {
        let config = self.clone();
        tokio::spawn(async move {
            loop {
                config.update_state();
                let update_interval = config
                    .symbols
                    .state_update_interval_ms
                    .get()
                    .and_then(|x| x.as_int())
                    .unwrap_or(2000) as u64;
                tokio::time::sleep(Duration::from_millis(update_interval)).await;
            }
        });
    }

    fn update_state(&self) {
        let exprs_alist = self
            .symbols
            .state_update_functions
            .get()
            .map_err(|e| {
                log::error!("Tulisp error:\n{}", e.format(&self.ctx.borrow()));
                panic!("Update state function failed");
            })
            .unwrap();
        let last_update_time = self.last_formula_update_time.borrow();
        let now = std::time::Instant::now();

        for func in exprs_alist.base_iter() {
            let res = self.ctx.borrow_mut().funcall(
                &func,
                &list![(now.duration_since(*last_update_time).as_millis() as i64).into()].unwrap(),
            );
            res.map_err(|e| {
                log::error!("Tulisp error:\n{}", e.format(&self.ctx.borrow()));
                panic!("Update state function failed");
            })
            .unwrap();
        }
        drop(last_update_time);
        *self.last_formula_update_time.borrow_mut() = now;
    }

    pub fn socket_addr(&self) -> String {
        let addr = self.symbols.socket_addr.get().and_then(|x| x.as_string());

        match addr {
            Ok(vv) => vv,
            Err(err) => {
                panic!(
                    r#"{}

Invalid socket-addr.  Add a config line in this format:
	(setq socket-addr "[::1]:8080")
"#,
                    err.format(&self.ctx.borrow())
                )
            }
        }
    }

    pub fn retain_requests_duration(&self) -> Duration {
        let dur_ms = self
            .symbols
            .retain_requests_duration_ms
            .get()
            .and_then(|x| x.as_int())
            .unwrap_or(5000);

        Duration::from_millis(dur_ms as u64)
    }

    pub fn metadata(&self) -> Result<MicrogridMetadata, Error> {
        let alist = self.symbols.metadata.get().unwrap_or_else(|_|TulispObject::nil());

        let microgrid_id = alist_get_as!(
            &mut self.ctx.borrow_mut(),
            &alist,
            &self.symbols.microgrid_id,
            as_int
        ).unwrap_or_default() as u64;
        let location = alist_get_as!(
            &mut self.ctx.borrow_mut(),
            &alist,
            &self.symbols.location
        ).unwrap_or_default();

        let latitude = location.car()?.as_float().map(|x| Some(x)).unwrap_or(None);
        let longitude = location.cadr()?.as_float().map(|x| Some(x)).unwrap_or(None);

        Ok(MicrogridMetadata {
            microgrid_id,
            location: if latitude.is_some() || longitude.is_some() {
                Some(Location {
                    latitude: latitude.unwrap_or_default() as f32,
                    longitude: longitude.unwrap_or_default() as f32,
                })
            } else {
                None
            },
        })
    }

    pub fn components(&self) -> Result<ComponentList, Error> {
        let alists = self.symbols.components_alist.get()?;
        Ok(ComponentList {
            components: alists
                .base_iter()
                .map(|x| {
                    make_component_from_alist(&mut self.ctx.borrow_mut(), &x, &self.symbols)
                        .unwrap()
                })
                .collect(),
        })
    }

    pub fn connections(&self) -> Result<ConnectionList, Error> {
        let alist = self.symbols.connections_alist.get()?;
        Ok(ConnectionList {
            connections: alist
                .base_iter()
                .map(|x| Connection {
                    start: x.car().and_then(|x| x.as_int()).unwrap() as u64,
                    end: x.cdr().and_then(|x| x.as_int()).unwrap() as u64,
                    ..Default::default()
                })
                .collect(),
        })
    }

    pub fn set_power_active(&self, component_id: u64, power: f32) -> Result<(), Error> {
        let res = self.ctx.borrow_mut().funcall(
            &self.symbols.set_power_active,
            &list![(component_id as i64).into(), (power as f64).into()]?,
        )?;

        if !res.null() {
            return Err(Error::new(tulisp::ErrorKind::Undefined, res.as_string()?).with_trace(res));
        }
        Ok(())
    }

    fn get_conv_function(&self, component_id: u64, comp: &TulispObject) -> CompDataMaker {
        match make_component_from_alist(&mut self.ctx.borrow_mut(), &comp, &self.symbols)
            .unwrap()
            .category()
        {
            ComponentCategory::Battery => Self::battery_data,
            ComponentCategory::Inverter => Self::inverter_data,
            ComponentCategory::Meter => Self::meter_data,
            ComponentCategory::EvCharger => Self::ev_charger_data,
            _ => Err(Error::new(
                tulisp::ErrorKind::Uninitialized,
                format!("Invalid component category for component {}", component_id),
            ))
            .unwrap(),
        }
    }

    pub fn get_component_data(&self, component_id: u64) -> Result<(ComponentData, u64), Error> {
        let mut stream_methods = self.stream_methods.borrow_mut();
        let (data_method, interval, conv_function) =
            if let Some((data_method, interval, conv_function)) = stream_methods.get(&component_id)
            {
                (data_method.clone(), *interval, *conv_function)
            } else {
                let alists = self.symbols.components_alist.get()?;
                let comp = alists
                    .base_iter()
                    .find(|x| {
                        alist_get_as!(&mut self.ctx.borrow_mut(), &x, &self.symbols.id, as_int)
                            .unwrap() as u64
                            == component_id
                    })
                    .expect(&format!("Component id {component_id} not found"));

                let stream =
                    alist_get_as!(&mut self.ctx.borrow_mut(), &comp, &self.symbols.stream).unwrap();

                let interval = alist_get_as!(
                    &mut self.ctx.borrow_mut(),
                    &stream,
                    &self.symbols.interval,
                    as_int
                )
                .unwrap();
                let data_method =
                    alist_get_as!(&mut self.ctx.borrow_mut(), &stream, &self.symbols.data).unwrap();

                let conv_function = self.get_conv_function(component_id, &comp);

                stream_methods.insert(
                    component_id,
                    (data_method.clone(), interval as u64, conv_function),
                );

                (data_method, interval as u64, conv_function)
            };

        let tulisp_data = self
            .ctx
            .borrow_mut()
            .funcall(&data_method, &list!((component_id as i64).into())?);
        let tulisp_data = tulisp_data.map_err(|e| {
            log::error!("Tulisp error:\n{}", e.format(&self.ctx.borrow()));
            panic!();
        })?;

        let comp_data = conv_function(&mut self.ctx.borrow_mut(), &tulisp_data, &self.symbols);
        let comp_data = comp_data.map_err(|e| {
            log::error!("Tulisp error:\n{}", e.format(&self.ctx.borrow()));
            panic!();
        })?;

        Ok((comp_data, interval as u64))
    }
}

/// ComponentData methods
impl Config {
    fn battery_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ComponentData, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;
        let capacity = alist_get_f32!(ctx, &alist, &symbols.capacity);

        let soc_avg = alist_get_f32!(ctx, &alist, &symbols.soc);
        let soc_lower = alist_get_f32!(ctx, &alist, &symbols.soc_lower);
        let soc_upper = alist_get_f32!(ctx, &alist, &symbols.soc_upper);

        let voltage = alist_get_f32!(ctx, &alist, &symbols.voltage);
        let current = alist_get_f32!(ctx, &alist, &symbols.current);
        let power = alist_get_f32!(ctx, &alist, &symbols.power);

        let inclusion_lower = alist_get_f32!(ctx, &alist, &symbols.inclusion_lower);
        let inclusion_upper = alist_get_f32!(ctx, &alist, &symbols.inclusion_upper);
        let exclusion_lower = alist_get_f32!(ctx, &alist, &symbols.exclusion_lower);
        let exclusion_upper = alist_get_f32!(ctx, &alist, &symbols.exclusion_upper);

        let component_state =
            enum_from_alist::<battery::ComponentState>(ctx, &alist, &symbols.component_state, true)
                .unwrap_or_default() as i32;
        let relay_state =
            enum_from_alist::<battery::RelayState>(ctx, &alist, &symbols.relay_state, true)
                .unwrap_or_default() as i32;

        return Ok(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id,
            data: Some(component_data::Data::Battery(battery::Battery {
                properties: Some(battery::Properties {
                    capacity,
                    ..Default::default()
                }),
                state: Some(battery::State {
                    component_state,
                    relay_state,
                }),
                errors: vec![],
                data: Some(battery::Data {
                    soc: Some(MetricAggregation {
                        avg: soc_avg,
                        system_inclusion_bounds: Some(Bounds {
                            lower: soc_lower,
                            upper: soc_upper,
                        }),
                        ..Default::default()
                    }),
                    dc: Some(Dc {
                        voltage: Some(Metric {
                            value: voltage,
                            ..Default::default()
                        }),
                        current: Some(Metric {
                            value: current,
                            ..Default::default()
                        }),
                        power: Some(Metric {
                            value: power,
                            system_inclusion_bounds: Some(Bounds {
                                lower: inclusion_lower,
                                upper: inclusion_upper,
                            }),
                            system_exclusion_bounds: Some(Bounds {
                                lower: exclusion_lower,
                                upper: exclusion_upper,
                            }),
                            ..Default::default()
                        }),
                        ..Default::default()
                    }),
                    ..Default::default()
                }),
            })),
        });
    }

    fn ac_from_alist(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<Ac, Error> {
        let frequency = symbols
            .ac_frequency
            .get()
            .and_then(|x| x.as_float())
            .unwrap_or_default() as f32;
        let current = alist_get_3_phase!(ctx, &alist, &symbols.current);
        let voltage = alist_get_3_phase!(ctx, &alist, &symbols.voltage);
        let per_phase_power = alist_get_3_phase!(ctx, &alist, &symbols.per_phase_power);

        let power = alist_get_f32!(ctx, &alist, &symbols.power);

        let inclusion_lower = alist_get_f32!(ctx, &alist, &symbols.inclusion_lower);
        let inclusion_upper = alist_get_f32!(ctx, &alist, &symbols.inclusion_upper);
        let exclusion_lower = alist_get_f32!(ctx, &alist, &symbols.exclusion_lower);
        let exclusion_upper = alist_get_f32!(ctx, &alist, &symbols.exclusion_upper);

        Ok(Ac {
            frequency: Some(Metric {
                value: frequency,
                ..Default::default()
            }),
            current: Some(Metric {
                value: current.0 + current.1 + current.2,
                ..Default::default()
            }),
            power_active: Some(Metric {
                value: power,
                system_inclusion_bounds: Some(Bounds {
                    lower: inclusion_lower,
                    upper: inclusion_upper,
                }),
                system_exclusion_bounds: Some(Bounds {
                    lower: exclusion_lower,
                    upper: exclusion_upper,
                }),
                ..Default::default()
            }),
            phase_1: Some(AcPhase {
                voltage: Some(Metric {
                    value: voltage.0,
                    ..Default::default()
                }),
                current: Some(Metric {
                    value: current.0,
                    ..Default::default()
                }),
                power_active: Some(Metric {
                    value: per_phase_power.0,
                    ..Default::default()
                }),
                ..Default::default()
            }),
            phase_2: Some(AcPhase {
                voltage: Some(Metric {
                    value: voltage.1,
                    ..Default::default()
                }),
                current: Some(Metric {
                    value: current.1,
                    ..Default::default()
                }),
                power_active: Some(Metric {
                    value: per_phase_power.1,
                    ..Default::default()
                }),
                ..Default::default()
            }),
            phase_3: Some(AcPhase {
                voltage: Some(Metric {
                    value: voltage.2,
                    ..Default::default()
                }),
                current: Some(Metric {
                    value: current.2,
                    ..Default::default()
                }),
                power_active: Some(Metric {
                    value: per_phase_power.2,
                    ..Default::default()
                }),
                ..Default::default()
            }),
            ..Default::default()
        })
    }

    fn inverter_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ComponentData, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;

        let component_state = enum_from_alist::<inverter::ComponentState>(
            ctx,
            &alist,
            &symbols.component_state,
            true,
        )
        .unwrap_or_default() as i32;

        return Ok(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id,
            data: Some(component_data::Data::Inverter(inverter::Inverter {
                state: Some(inverter::State { component_state }),
                data: Some(inverter::Data {
                    ac: Some(Self::ac_from_alist(ctx, &alist, symbols)?),
                    ..Default::default()
                }),
                ..Default::default()
            })),
        });
    }

    fn meter_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ComponentData, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;

        return Ok(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id,
            data: Some(component_data::Data::Meter(meter::Meter {
                state: Some(meter::State {
                    component_state: enum_from_alist::<meter::ComponentState>(
                        ctx,
                        &alist,
                        &symbols.component_state,
                        true,
                    )
                    .unwrap_or_default() as i32,
                }),
                data: Some(meter::Data {
                    ac: Some(Self::ac_from_alist(ctx, &alist, symbols)?),
                    ..Default::default()
                }),
                ..Default::default()
            })),
        });
    }

    fn ev_charger_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ComponentData, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;

        let component_state = enum_from_alist::<ev_charger::ComponentState>(
            ctx,
            &alist,
            &symbols.component_state,
            true,
        )
        .unwrap_or_default() as i32;

        let cable_state =
            enum_from_alist::<ev_charger::CableState>(ctx, &alist, &symbols.cable_state, true)
                .unwrap_or_default() as i32;

        return Ok(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id,
            data: Some(component_data::Data::EvCharger(ev_charger::EvCharger {
                state: Some(ev_charger::State {
                    component_state,
                    cable_state,
                }),
                data: Some(ev_charger::Data {
                    ac: Some(Self::ac_from_alist(ctx, &alist, symbols)?),
                    ..Default::default()
                }),
                ..Default::default()
            })),
        });
    }
}

fn add_functions(ctx: &mut TulispContext) {
    macro_rules! log_impl {
        ($level:ident) => {
            |ctx, args| {
                destruct_bind!((msg) = args);
                log::$level!("{}", ctx.eval(&msg)?.as_string()?);
                Ok(TulispObject::nil())
            }
        };
    }

    ctx.add_special_form("log.info", log_impl!(info));
    ctx.add_special_form("log.warn", log_impl!(warn));
    ctx.add_special_form("log.error", log_impl!(error));
    ctx.add_special_form("log.debug", log_impl!(debug));
    ctx.add_special_form("log.trace", log_impl!(trace));

    ctx.add_special_form("random", |ctx, args| {
        destruct_bind!((&optional limit) = args);
        let rnd = if limit.null() {
            rand::thread_rng().gen()
        } else {
            let limit = ctx.eval(&limit)?.try_into()?;
            rand::thread_rng().gen_range(0..limit)
        };
        Ok(rnd.into())
    });
}
