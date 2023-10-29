mod lisp;
mod proto;
mod server;
use tonic::transport::Server;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let mut config = lisp::Config::new("config.lisp");

    let socket_addr = config.socket_addr();
    println!("Server listening on {}", socket_addr);

    let server = server::MicrogridServer { config };
    Server::builder()
        .add_service(proto::microgrid::microgrid_server::MicrogridServer::new(
            server,
        ))
        .serve(socket_addr.parse().unwrap())
        .await
        .unwrap();
}
