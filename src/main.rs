mod lisp;
mod proto;
mod server;
mod timeout_tracker;

use tonic::transport::Server;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    simplelog::SimpleLogger::init(simplelog::LevelFilter::Debug, simplelog::Config::default())
        .unwrap();

    let config = lisp::Config::new("config.lisp");
    tokio::spawn(config.clone().start());
    let socket_addr = config.socket_addr();
    log::info!("Server listening on {}", socket_addr);

    let server = server::MicrogridServer::new(config);
    Server::builder()
        .add_service(proto::microgrid::microgrid_server::MicrogridServer::new(
            server,
        ))
        .serve(socket_addr.parse().unwrap())
        .await
        .unwrap();
}
