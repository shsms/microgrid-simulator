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
