use std::{any::Any, cell::RefCell, collections::HashMap, rc::Rc, str::FromStr, time::Duration};

use crate::proto::{
    common::{
        components::{BatteryType, ComponentCategory, EvChargerType, InverterType},
        metrics::{
            electrical::{ac::AcPhase, Ac, Dc},
            Bounds, Metric, MetricAggregation,
        },
    },
    microgrid::{
        battery, component, component_data, ev_charger, inverter, meter, Component, ComponentData,
        ComponentList, Connection, ConnectionList,
    },
};
use prost_types::Timestamp;
use tokio_stream::StreamExt;
use tulisp::{list, tulisp_fn, tulisp_fn_no_eval, Error, TulispContext, TulispObject};

#[derive(Default, Clone)]
pub struct Config {
    filename: String,

    pub(crate) ctx: Rc<RefCell<tulisp::TulispContext>>,

    /// Component ID -> (ComponentData Method, Interval)
    stream_methods: Rc<RefCell<HashMap<u64, (TulispObject, u64)>>>,
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
        let key = $ctx.intern($key);
        tulisp::lists::alist_get($ctx, &key, $rest, None, None, None)
    }};
}

macro_rules! alist_get_f32 {
    ($ctx: expr, $rest:expr, $key:expr) => {
        alist_get_as!($ctx, $rest, $key, eval ++ try_float).unwrap_or_default() as f32
    };
}

macro_rules! alist_get_3_phase {
    ($ctx: expr, $rest:expr, $key:expr) => {{
        let items = alist_get_as!($ctx, $rest, $key).unwrap_or_default();
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
    key: &str,
) -> Option<T> {
    let val = alist_get_as!(ctx, alist, key, as_symbol).ok()?;
    match val.parse::<T>() {
        Ok(x) => Some(x),
        Err(_) => {
            println!("Invalid value for {}: {}", key, val);
            None
        }
    }
}

fn make_component_from_alist(
    ctx: &mut TulispContext,
    alist: &TulispObject,
) -> Result<Component, Error> {
    let id = alist_get_as!(ctx, alist, "id", as_int)? as u64;
    let name = alist_get_as!(ctx, alist, "name", as_string).unwrap_or_default();
    let Some(category) = enum_from_alist::<ComponentCategory>(ctx, alist, "category") else {
        return Err(Error::new(
            tulisp::ErrorKind::Uninitialized,
            format!("Invalid component category for component {}", id),
        ));
    };

    let metadata = match category {
        ComponentCategory::Inverter => enum_from_alist::<InverterType>(ctx, alist, "type")
            .map(|typ| component::Metadata::Inverter(inverter::Metadata { r#type: typ as i32 })),
        ComponentCategory::Battery => enum_from_alist::<BatteryType>(ctx, alist, "type")
            .map(|typ| component::Metadata::Battery(battery::Metadata { r#type: typ as i32 })),
        ComponentCategory::EvCharger => enum_from_alist::<EvChargerType>(ctx, alist, "type")
            .map(|typ| component::Metadata::EvCharger(ev_charger::Metadata { r#type: typ as i32 })),
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
            println!("Tulisp error:\n{}", e.format(&ctx));
            e
        });

        Self {
            filename: filename.to_string(),
            ctx: Rc::new(RefCell::new(ctx)),
            stream_methods: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn reload(&self) {
        let start = std::time::Instant::now();
        let mut ctx = self.ctx.borrow_mut();
        if ctx
            .eval_file(&self.filename)
            .map_err(|e| {
                println!("Tulisp error:\n{}", e.format(&ctx));
                e
            })
            .is_err()
        {
            return;
        }
        let duration = start.elapsed();
        println!(
            "Reloaded config file in {}ms",
            duration.as_nanos() as f64 / 1e6
        );
        *self.stream_methods.borrow_mut() = HashMap::new();
    }

    pub fn start_watching(&self) {
        let config = self.clone();

        tokio::spawn(async move {
            let inotify = inotify::Inotify::init().unwrap();
            inotify
                .watches()
                .add(config.filename.clone(), inotify::WatchMask::MODIFY)
                .unwrap();

            let mut buffer = [0; 1024];
            let mut inotify_stream = inotify.into_event_stream(&mut buffer).unwrap();

            while let Some(_) = inotify_stream.next().await {
                config.reload();
            }
        });
    }

    pub fn socket_addr(&self) -> String {
        let addr = self
            .ctx
            .borrow_mut()
            .intern("socket-addr")
            .get()
            .and_then(|x| x.as_string());

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
            .ctx
            .borrow_mut()
            .intern("retain-requests-duration-ms")
            .get()
            .and_then(|x| x.as_int())
            .unwrap_or(5000);

        Duration::from_millis(dur_ms as u64)
    }

    pub fn components(&self) -> Result<ComponentList, Error> {
        let alists = self.ctx.borrow_mut().intern("components-alist").get()?;
        Ok(ComponentList {
            components: alists
                .base_iter()
                .map(|x| make_component_from_alist(&mut self.ctx.borrow_mut(), &x).unwrap())
                .collect(),
        })
    }

    pub fn connections(&self) -> Result<ConnectionList, Error> {
        let alist = self.ctx.borrow_mut().intern("connections-alist").get()?;
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
        let func = self.ctx.borrow_mut().intern("set-power-active");
        self.ctx.borrow_mut().funcall(
            &func,
            &list![(component_id as i64).into(), (power as f64).into()]?,
        )?;

        Ok(())
    }

    pub fn get_component_data(&self, component_id: u64) -> Result<(ComponentData, u64), Error> {
        let mut stream_methods = self.stream_methods.borrow_mut();
        let (data_method, interval) = if let Some((data_method, interval)) =
            stream_methods.get(&component_id)
        {
            (data_method.clone(), *interval)
        } else {
            let alists = self.ctx.borrow_mut().intern("components-alist").get()?;
            let comp = alists
                .base_iter()
                .find(|x| {
                    alist_get_as!(&mut self.ctx.borrow_mut(), &x, "id", as_int).unwrap() as u64
                        == component_id
                })
                .expect(&format!("Component id {component_id} not found"));

            let stream = alist_get_as!(&mut self.ctx.borrow_mut(), &comp, "stream").unwrap();

            let interval =
                alist_get_as!(&mut self.ctx.borrow_mut(), &stream, "interval", as_int).unwrap();
            let data_method = alist_get_as!(&mut self.ctx.borrow_mut(), &stream, "data").unwrap();

            stream_methods.insert(component_id, (data_method.clone(), interval as u64));

            (data_method, interval as u64)
        };

        let comp_data = self
            .ctx
            .borrow_mut()
            .funcall(&data_method, &list!((component_id as i64).into())?)?
            .as_any()?
            .downcast_ref::<ComponentData>()
            .unwrap()
            .clone();

        Ok((comp_data, interval as u64))
    }
}

fn add_functions(ctx: &mut TulispContext) {
    #[tulisp_fn_no_eval(add_func = "ctx", name = "battery-data")]
    fn battery_data(ctx: &mut TulispContext, alist: TulispObject) -> Result<Rc<dyn Any>, Error> {
        let id = alist_get_as!(ctx, &alist, "id", eval ++ as_int)? as u64;
        let capacity = alist_get_f32!(ctx, &alist, "capacity");

        let soc_avg = alist_get_f32!(ctx, &alist, "soc");
        let soc_lower = alist_get_f32!(ctx, &alist, "soc-lower");
        let soc_upper = alist_get_f32!(ctx, &alist, "soc-upper");

        let voltage = alist_get_f32!(ctx, &alist, "voltage");
        let current = alist_get_f32!(ctx, &alist, "current");
        let power = alist_get_f32!(ctx, &alist, "power");

        let inclusion_lower = alist_get_f32!(ctx, &alist, "inclusion-lower");
        let inclusion_upper = alist_get_f32!(ctx, &alist, "inclusion-upper");
        let exclusion_lower = alist_get_f32!(ctx, &alist, "exclusion-lower");
        let exclusion_upper = alist_get_f32!(ctx, &alist, "exclusion-upper");

        let component_state =
            enum_from_alist::<battery::ComponentState>(ctx, &alist, "component-state")
                .unwrap_or_default() as i32;
        let relay_state = enum_from_alist::<battery::RelayState>(ctx, &alist, "relay-state")
            .unwrap_or_default() as i32;

        return Ok(Rc::new(ComponentData {
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
        }));
    }

    fn ac_from_alist(ctx: &mut TulispContext, alist: &TulispObject) -> Result<Ac, Error> {
        let frequency = ctx
            .intern("ac-frequency")
            .get()
            .and_then(|x| x.as_float())
            .unwrap_or_default() as f32;
        let current = alist_get_3_phase!(ctx, &alist, "current");
        let voltage = alist_get_3_phase!(ctx, &alist, "voltage");
        let power = alist_get_f32!(ctx, &alist, "power");

        let inclusion_lower = alist_get_f32!(ctx, &alist, "inclusion-lower");
        let inclusion_upper = alist_get_f32!(ctx, &alist, "inclusion-upper");
        let exclusion_lower = alist_get_f32!(ctx, &alist, "exclusion-lower");
        let exclusion_upper = alist_get_f32!(ctx, &alist, "exclusion-upper");

        Ok(Ac {
            frequency: Some(Metric {
                value: frequency,
                ..Default::default()
            }),
            current: Some(Metric {
                // TODO: what is a 3 phase current?  is this the sum of all 3 phases?
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
                ..Default::default()
            }),
            ..Default::default()
        })
    }

    #[tulisp_fn_no_eval(add_func = "ctx", name = "inverter-data")]
    fn inverter_data(ctx: &mut TulispContext, alist: TulispObject) -> Result<Rc<dyn Any>, Error> {
        let id = alist_get_as!(ctx, &alist, "id", eval ++ as_int)? as u64;

        let component_state =
            enum_from_alist::<inverter::ComponentState>(ctx, &alist, "component-state")
                .unwrap_or_default() as i32;

        return Ok(Rc::new(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id,
            data: Some(component_data::Data::Inverter(inverter::Inverter {
                state: Some(inverter::State { component_state }),
                data: Some(inverter::Data {
                    ac: Some(ac_from_alist(ctx, &alist)?),
                    ..Default::default()
                }),
                ..Default::default()
            })),
        }));
    }

    #[tulisp_fn_no_eval(add_func = "ctx", name = "meter-data")]
    fn meter_data(ctx: &mut TulispContext, alist: TulispObject) -> Result<Rc<dyn Any>, Error> {
        let id = alist_get_as!(ctx, &alist, "id", eval ++ as_int)? as u64;

        return Ok(Rc::new(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id,
            data: Some(component_data::Data::Meter(meter::Meter {
                data: Some(meter::Data {
                    ac: Some(ac_from_alist(ctx, &alist)?),
                    ..Default::default()
                }),
                ..Default::default()
            })),
        }));
    }

    #[tulisp_fn_no_eval(add_func = "ctx", name = "ev-charger-data")]
    fn ev_charger_data(ctx: &mut TulispContext, alist: TulispObject) -> Result<Rc<dyn Any>, Error> {
        let id = alist_get_as!(ctx, &alist, "id", eval ++ as_int)? as u64;

        let component_state =
            enum_from_alist::<ev_charger::ComponentState>(ctx, &alist, "component-state")
                .unwrap_or_default() as i32;

        let cable_state = enum_from_alist::<ev_charger::CableState>(ctx, &alist, "cable-state")
            .unwrap_or_default() as i32;

        return Ok(Rc::new(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id,
            data: Some(component_data::Data::EvCharger(ev_charger::EvCharger {
                state: Some(ev_charger::State {
                    component_state,
                    cable_state,
                }),
                data: Some(ev_charger::Data {
                    ac: Some(ac_from_alist(ctx, &alist)?),
                    ..Default::default()
                }),
                ..Default::default()
            })),
        }));
    }

    #[tulisp_fn(add_func = "ctx", name = "log.info")]
    fn log_info(msg: String) {
        log::info!("{}", msg);
    }

    #[tulisp_fn(add_func = "ctx", name = "log.warn")]
    fn log_warn(msg: String) {
        log::warn!("{}", msg);
    }

    #[tulisp_fn(add_func = "ctx", name = "log.error")]
    fn log_error(msg: String) {
        log::error!("{}", msg);
    }

    #[tulisp_fn(add_func = "ctx", name = "log.debug")]
    fn log_debug(msg: String) {
        log::debug!("{}", msg);
    }
}
