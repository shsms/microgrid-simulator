use crate::proto::microgrid::microgrid_server::Microgrid;
use crate::proto::microgrid::{
    ComponentData, ComponentFilter, ComponentIdParam, ComponentList, ConnectionFilter,
    ConnectionList, MicrogridMetadata, SetBoundsParam, SetPowerActiveParam, SetPowerReactiveParam,
};

pub struct MicrogridServer;

#[tonic::async_trait]
impl Microgrid for MicrogridServer {
    async fn get_microgrid_metadata(
        &self,
        request: tonic::Request<()>,
    ) -> std::result::Result<tonic::Response<MicrogridMetadata>, tonic::Status> {
        todo!()
    }

    async fn list_components(
        &self,
        request: tonic::Request<ComponentFilter>,
    ) -> std::result::Result<tonic::Response<ComponentList>, tonic::Status> {
        todo!()
    }
    async fn list_connections(
        &self,
        request: tonic::Request<ConnectionFilter>,
    ) -> std::result::Result<tonic::Response<ConnectionList>, tonic::Status> {
        todo!()
    }

    type StreamComponentDataStream = tonic::codec::Streaming<ComponentData>;

    async fn stream_component_data(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<Self::StreamComponentDataStream>, tonic::Status> {
        todo!()
    }
    async fn can_stream_data(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<bool>, tonic::Status> {
        todo!()
    }

    // Unused methods
    async fn add_exclusion_bounds(
        &self,
        request: tonic::Request<SetBoundsParam>,
    ) -> std::result::Result<tonic::Response<::prost_types::Timestamp>, tonic::Status> {
        todo!()
    }
    async fn add_inclusion_bounds(
        &self,
        request: tonic::Request<SetBoundsParam>,
    ) -> std::result::Result<tonic::Response<::prost_types::Timestamp>, tonic::Status> {
        todo!()
    }
    async fn set_power_active(
        &self,
        request: tonic::Request<SetPowerActiveParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn set_power_reactive(
        &self,
        request: tonic::Request<SetPowerReactiveParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn start(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn hot_standby(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn cold_standby(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn stop(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn error_ack(
        &self,
        request: tonic::Request<ComponentIdParam>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
}
