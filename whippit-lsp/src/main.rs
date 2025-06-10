use std::{error::Error, fs, io};

use lsp_server::{Connection, Message, Response};
use lsp_types::{
    DocumentFormattingParams, FormattingOptions, InitializeParams, OneOf, Position,
    ServerCapabilities, TextDocumentIdentifier, TextEdit, WorkDoneProgressParams,
    request::{Formatting, Request as _},
};
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{EnvFilter, layer::SubscriberExt as _, util::SubscriberInitExt as _};
use whippit::{metadata::properties::unstructured::UnstructuredFile, reexport::chumsky::Parser};

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
            document_formatting_provider: Some(OneOf::Left(true)),
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
                match &*req.method {
                    Formatting::METHOD => {
                        let (id, params) = req.extract(Formatting::METHOD).unwrap();

                        let DocumentFormattingParams {
                            text_document,
                            options,
                            work_done_progress_params,
                        } = params;

                        let TextDocumentIdentifier { uri } = text_document;

                        let FormattingOptions { .. } = options;
                        // TODO: validate that options match what we'll impose, otherwise warn.

                        let WorkDoneProgressParams { work_done_token } = work_done_progress_params;
                        assert!(work_done_token.is_none());

                        // TODO: Break this logic out into its own file!
                        let response = uri
                            .scheme()
                            .is_some_and(|s| s.eq_lowercase("file"))
                            .then(|| uri.path())
                            .and_then(|path| {
                                // TODO: don't `unwrap` this plz
                                let file_contents = fs::read_to_string(path.as_str()).unwrap();
                                let file = dbg!(
                                    whippit::metadata::file_parser::<'_, UnstructuredFile<'_>>()
                                        .parse(&file_contents)
                                )
                                .into_output()?;
                                let file_contents =
                                    whippit::metadata::properties::unstructured::format(&file)
                                        .to_string();
                                Some(vec![TextEdit {
                                    range: lsp_types::Range {
                                        start: Position {
                                            line: 0,
                                            character: 0,
                                        },
                                        // TODO: Is this okay? Do we actually need to compute the end
                                        // indices?
                                        end: Position {
                                            line: u32::MAX,
                                            character: u32::MAX,
                                        },
                                    },
                                    new_text: file_contents,
                                }])
                            });

                        let _ = connection
                            .sender
                            .send(Message::Response(Response::new_ok(id, response)));
                    }
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
