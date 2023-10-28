mod proto;
mod server;
use tonic::transport::Server;

#[tokio::main]
async fn main() {
    let server = server::MicrogridServer {};
    Server::builder()
        .add_service(proto::microgrid::microgrid_server::MicrogridServer::new(
            server,
        ))
        .serve("[::1]:50051".parse().unwrap())
        .await
        .unwrap();
}
