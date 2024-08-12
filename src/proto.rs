use self::{
    common::components::{BatteryType, ComponentCategory, EvChargerType, InverterType},
    microgrid::{battery, ev_charger, inverter, meter},
};

pub mod common {
    pub mod components {
        tonic::include_proto!("frequenz.api.common.components");
    }

    pub mod metrics {
        tonic::include_proto!("frequenz.api.common.metrics");

        pub mod electrical {
            tonic::include_proto!("frequenz.api.common.metrics.electrical");
        }
    }
}

pub mod microgrid {
    tonic::include_proto!("frequenz.api.microgrid");

    pub mod common {
        tonic::include_proto!("frequenz.api.microgrid.common");
    }

    pub mod grid {
        tonic::include_proto!("frequenz.api.microgrid.grid");
    }

    pub mod inverter {
        tonic::include_proto!("frequenz.api.microgrid.inverter");
    }

    pub mod battery {
        tonic::include_proto!("frequenz.api.microgrid.battery");
    }

    pub mod ev_charger {
        tonic::include_proto!("frequenz.api.microgrid.ev_charger");
    }

    pub mod meter {
        tonic::include_proto!("frequenz.api.microgrid.meter");
    }

    pub mod sensor {
        tonic::include_proto!("frequenz.api.microgrid.sensor");
    }
}

macro_rules! impl_enum_from_str {
    ($(($t:ty, $p:literal),)+) => {
        $(
            impl std::str::FromStr for $t {
                type Err = ();

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    let s = s.replace("-", "_");
                    match <$t>::from_str_name(($p.to_string() + &s).to_uppercase().as_str()) {
                        Some(x) => Ok(x),
                        None => Err(()),
                    }
                }
            }
        )+
    };
}

impl_enum_from_str!(
    (ComponentCategory, "COMPONENT_CATEGORY_"),
    (BatteryType, "BATTERY_TYPE_"),
    (InverterType, "INVERTER_TYPE_"),
    (EvChargerType, "EV_CHARGER_TYPE_"),
    (meter::ComponentState, "COMPONENT_STATE_"),
    (battery::ComponentState, "COMPONENT_STATE_"),
    (battery::RelayState, "RELAY_STATE_"),
    (inverter::ComponentState, "COMPONENT_STATE_"),
    (ev_charger::ComponentState, "COMPONENT_STATE_"),
    (ev_charger::CableState, "CABLE_STATE_"),
);
