use std::{error::Error, io};

use lsp_server::{Connection, Message};
use lsp_types::{InitializeParams, ServerCapabilities};
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{EnvFilter, layer::SubscriberExt as _, util::SubscriberInitExt as _};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().with_writer(io::stderr))
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();
    tracing::trace!("logging initialized");

    let (connection, io_threads, initialization_params) = tracing::info_span!(
        "starting generic LSP server"
    )
    .in_scope(|| -> Result<_, Box<dyn Error + Sync + Send>> {
        tracing::trace!("initializing I/O…");

        let (connection, io_threads) = Connection::stdio();

        tracing::trace!("generating server capabilities…");

        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            ..Default::default()
        })
        .unwrap();

        tracing::debug!("awaiting init from client…");

        let initialization_params = match connection.initialize(server_capabilities) {
            Ok(it) => it,
            Err(e) => {
                if e.channel_is_disconnected() {
                    io_threads.join()?;
                }
                return Err(e.into());
            }
        };

        tracing::debug!("init from client: {initialization_params:#?}");

        let initialization_params: InitializeParams =
            serde_json::from_value(initialization_params).unwrap();

        Ok((connection, io_threads, initialization_params))
    })?;

    tracing::info_span!("main loop").in_scope(|| {
        main_loop(connection, initialization_params)?;
        tracing::info!("shutting down server");
        io_threads.join()?;
        Ok(())
    })
}

fn main_loop(
    connection: Connection,
    _params: InitializeParams,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    tracing::trace!("entering message receive loop");
    for msg in &connection.receiver {
        tracing::trace!("processing message {msg:?}");

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                tracing::trace!("got request: {req:?}");
                #[expect(clippy::match_single_binding)]
                match &*req.method {
                    _ => {
                        panic!("bruh IDK what this is: {req:?}")
                    }
                };
            }
            Message::Response(resp) => {
                tracing::info!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                tracing::info!("got notification: {not:?}");
            }
        }
    }
    Ok(())
}
