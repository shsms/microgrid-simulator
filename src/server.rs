use std::pin::Pin;
use std::time::{Duration, SystemTime};

use tokio_stream::wrappers::ReceiverStream;
use tokio_stream::Stream;

use crate::lisp::Config;
use crate::proto::microgrid::microgrid_server::Microgrid;
use crate::proto::microgrid::{
    ComponentData, ComponentFilter, ComponentIdParam, ComponentList, ConnectionFilter,
    ConnectionList, MicrogridMetadata, SetBoundsParam, SetPowerActiveParam, SetPowerReactiveParam,
};

pub struct MicrogridServer {
    pub config: Config,
}

#[tonic::async_trait]
impl Microgrid for MicrogridServer {
    async fn get_microgrid_metadata(
        &self,
        _request: tonic::Request<()>,
    ) -> std::result::Result<tonic::Response<MicrogridMetadata>, tonic::Status> {
        todo!()
    }

    async fn list_components(
        &self,
        _request: tonic::Request<ComponentFilter>,
    ) -> std::result::Result<tonic::Response<ComponentList>, tonic::Status> {
        let components = self.config.components().unwrap();
        Ok(tonic::Response::new(components))
    }
    async fn list_connections(
        &self,
        _request: tonic::Request<ConnectionFilter>,
    ) -> std::result::Result<tonic::Response<ConnectionList>, tonic::Status> {
        let connections = self.config.connections().unwrap();
        Ok(tonic::Response::new(connections))
    }

    async fn set_power_active(
        &self,
        _request: tonic::Request<SetPowerActiveParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        let request = _request.into_inner();
        self.config
            .set_power_active(request.component_id, request.power)
            .map_err(|e| {
                println!("Tulisp error:\n{}", e.format(&self.config.ctx.borrow()));
                e
            })
            .unwrap();
        Ok(tonic::Response::new(()))
    }

    type StreamComponentDataStream =
        Pin<Box<dyn Stream<Item = Result<ComponentData, tonic::Status>> + Send>>;

    async fn stream_component_data(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<Self::StreamComponentDataStream>, tonic::Status> {
        let id = request.into_inner().id;

        let (tx, rx) = tokio::sync::mpsc::channel(128);
        let config = self.config.clone();

        tokio::spawn(async move {
            let mut last_msg_ts = SystemTime::now();
            loop {
                let (data, interval) = config.get_component_data(id as u64).unwrap();

                if let Err(err) = tx.send(Result::<_, tonic::Status>::Ok(data)).await {
                    println!("stream_component_data(component_id={id}): {err}");
                    break;
                }

                let now = SystemTime::now();
                let tgt_ts = last_msg_ts + Duration::from_millis(interval as u64);
                let dur =
                    Duration::from_millis(tgt_ts.duration_since(now).unwrap().as_millis() as u64);
                tokio::time::sleep(dur).await;
                last_msg_ts = tgt_ts;
            }
        });

        let output_stream = ReceiverStream::new(rx);
        Ok(tonic::Response::new(
            Box::pin(output_stream) as Self::StreamComponentDataStream
        ))
    }

    //
    //
    // Unused methods
    //
    //
    async fn can_stream_data(
        &self,
        _request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<bool>, tonic::Status> {
        todo!()
    }
    async fn add_exclusion_bounds(
        &self,
        _request: tonic::Request<SetBoundsParam>,
    ) -> std::result::Result<tonic::Response<::prost_types::Timestamp>, tonic::Status> {
        todo!()
    }
    async fn add_inclusion_bounds(
        &self,
        _request: tonic::Request<SetBoundsParam>,
    ) -> std::result::Result<tonic::Response<::prost_types::Timestamp>, tonic::Status> {
        todo!()
    }
    async fn set_power_reactive(
        &self,
        _request: tonic::Request<SetPowerReactiveParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn start(
        &self,
        _request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn hot_standby(
        &self,
        _request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn cold_standby(
        &self,
        _request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn stop(
        &self,
        _request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn error_ack(
        &self,
        _request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
}
