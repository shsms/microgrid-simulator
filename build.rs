fn main() -> Result<(), std::io::Error> {
    let config = prost_build::Config::new();

    tonic_build::configure()
        .compile_with_config(
            config,
            &["submodules/frequenz-api-microgrid/proto/frequenz/api/microgrid/microgrid.proto"],
            &[
                "submodules/frequenz-api-microgrid/proto",
                "submodules/frequenz-api-microgrid/submodules/frequenz-api-common/proto",
                "submodules/frequenz-api-microgrid/submodules/api-common-protos",
            ],
        )
        .map_err(|e| {
            eprintln!("Could not compile protobuf files. Error: {:?}", e);
            e
        })
}
