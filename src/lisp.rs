use std::{any::Any, collections::HashMap, rc::Rc};

use prost_types::Timestamp;
use tulisp::{tulisp_fn, Error, TulispContext, TulispObject};

use crate::proto::{
    common::{
        components::ComponentCategory,
        metrics::{electrical::Dc, Bounds, Metric, MetricAggregation},
    },
    microgrid::{
        battery, component_data, Component, ComponentData, ComponentList, Connection,
        ConnectionList,
    },
};

#[derive(Default)]
pub struct Config {
    ctx: tulisp::TulispContext,

    /// Component ID -> (ComponentData Method, Interval)
    stream_methods: HashMap<u64, (TulispObject, u64)>,
}

// Tokio is configured to use the current_thread runtime, so it is not unsafe to
// make `Config` Send.
unsafe impl Send for Config {}

macro_rules! alist_get_as {
    ($ctx: expr, $rest:expr, $key:expr, $as_fn:ident) => {{
        let key = $ctx.intern($key);
        tulisp::lists::alist_get($ctx, &key, $rest, None, None, None).and_then(|x| x.$as_fn())
    }};
    ($ctx: expr, $rest:expr, $key:expr) => {{
        let key = $ctx.intern($key);
        tulisp::lists::alist_get($ctx, &key, $rest, None, None, None)
    }};
}

fn make_component_from_alist(
    ctx: &mut TulispContext,
    alist: &TulispObject,
) -> Result<Component, Error> {
    let comp = Component {
        id: alist_get_as!(ctx, alist, "id", as_int)? as u64,
        name: alist_get_as!(ctx, alist, "name", as_string).unwrap_or_default(),
        category: ComponentCategory::from_str_name(&alist_get_as!(
            ctx, alist, "category", as_string
        )?)
        .unwrap_or_default() as i32,

        ..Default::default()
    };
    Ok(comp)
}

impl Config {
    pub fn new(filename: &str) -> Self {
        let mut ctx = tulisp::TulispContext::new();
        add_functions(&mut ctx);
        ctx.eval_file(filename).unwrap();
        Self {
            ctx,
            ..Default::default()
        }
    }

    pub fn socket_addr(&mut self) -> String {
        self.ctx
            .eval_string("socket-addr")
            .and_then(|x| x.as_string())
            .unwrap()
    }

    pub fn components(&mut self) -> Result<ComponentList, Error> {
        let alists = self.ctx.eval_string("components-alist")?;
        Ok(ComponentList {
            components: alists
                .base_iter()
                .map(|x| make_component_from_alist(&mut self.ctx, &x).unwrap())
                .collect(),
        })
    }

    pub fn connections(&mut self) -> Result<ConnectionList, Error> {
        let alist = self.ctx.eval_string("connections-alist")?;
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

    pub fn get_component_data(&mut self, component_id: u64) -> Result<(ComponentData, u64), Error> {
        let (data_method, interval) = if let Some((data_method, interval)) =
            self.stream_methods.get(&component_id)
        {
            (data_method.clone(), *interval)
        } else {
            let alists = self.ctx.eval_string("components-alist")?;
            let comp = alists
                .base_iter()
                .find(|x| {
                    alist_get_as!(&mut self.ctx, &x, "id", as_int).unwrap() as u64 == component_id
                })
                .expect(&format!("Component id {component_id} not found"));

            let stream = alist_get_as!(&mut self.ctx, &comp, "stream").unwrap();

            let interval = alist_get_as!(&mut self.ctx, &stream, "interval", as_int).unwrap();
            let data_method = alist_get_as!(&mut self.ctx, &stream, "data").unwrap();

            self.stream_methods
                .insert(component_id, (data_method.clone(), interval as u64));

            (data_method, interval as u64)
        };

        let comp_data = self
            .ctx
            .funcall(&data_method, &TulispObject::nil())?
            .as_any()?
            .downcast_ref::<ComponentData>()
            .unwrap()
            .clone();

        Ok((comp_data, interval as u64))
    }
}

fn add_functions(ctx: &mut TulispContext) {
    #[tulisp_fn(add_func = "ctx", name = "battery-data")]
    fn battery_data(ctx: &mut TulispContext, alist: TulispObject) -> Result<Rc<dyn Any>, Error> {
        return Ok(Rc::new(ComponentData {
            ts: Some(Timestamp::from(std::time::SystemTime::now())),
            id: 0,
            data: Some(component_data::Data::Battery(battery::Battery {
                properties: Some(battery::Properties {
                    capacity: alist_get_as!(ctx, &alist, "capacity", as_float).unwrap() as f32,
                    ..Default::default()
                }),
                state: None,
                errors: vec![],
                data: Some(battery::Data {
                    soc: Some(MetricAggregation {
                        avg: alist_get_as!(ctx, &alist, "soc", as_float).unwrap_or_default() as f32,
                        system_inclusion_bounds: Some(Bounds {
                            lower: alist_get_as!(ctx, &alist, "soc-lower", as_float)
                                .unwrap_or_default() as f32,
                            upper: alist_get_as!(ctx, &alist, "soc-upper", as_float)
                                .unwrap_or_default() as f32,
                        }),
                        ..Default::default()
                    }),
                    dc: Some(Dc {
                        voltage: Some(Metric {
                            value: alist_get_as!(ctx, &alist, "voltage", as_float)
                                .unwrap_or_default() as f32,
                            ..Default::default()
                        }),
                        current: Some(Metric {
                            value: alist_get_as!(ctx, &alist, "current", as_float)
                                .unwrap_or_default() as f32,
                            ..Default::default()
                        }),
                        power: Some(Metric {
                            value: alist_get_as!(ctx, &alist, "power", as_float).unwrap_or_default()
                                as f32,
                            system_inclusion_bounds: Some(Bounds {
                                lower: alist_get_as!(ctx, &alist, "inclusion-lower", as_float)
                                    .unwrap_or_default()
                                    as f32,
                                upper: alist_get_as!(ctx, &alist, "inclusion-upper", as_float)
                                    .unwrap_or_default()
                                    as f32,
                            }),
                            system_exclusion_bounds: Some(Bounds {
                                lower: alist_get_as!(ctx, &alist, "exclusion-lower", as_float)
                                    .unwrap_or_default()
                                    as f32,
                                upper: alist_get_as!(ctx, &alist, "exclusion-upper", as_float)
                                    .unwrap_or_default()
                                    as f32,
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
}
