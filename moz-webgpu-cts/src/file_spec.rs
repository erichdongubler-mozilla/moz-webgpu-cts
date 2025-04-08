use std::{fmt::Display, path::PathBuf};

use miette::Report;
use wax::Glob;

use crate::AlreadyReportedToCommandline;

pub(crate) struct FileSpec {
    pub paths: Vec<PathBuf>,
    pub globs: Vec<String>,
}

impl FileSpec {
    pub(crate) fn into_paths(
        self,
        what: impl Display,
    ) -> Result<Vec<PathBuf>, AlreadyReportedToCommandline> {
        let Self { paths, globs } = self;

        let globs = {
            let mut found_glob_parse_err = false;
            let globs = globs
                .into_iter()
                .filter_map(|glob| match Glob::diagnosed(&glob) {
                    Ok((glob, _diagnostics)) => Some(glob.into_owned().partition()),
                    Err(diagnostics) => {
                        found_glob_parse_err = true;
                        let error_reports = diagnostics
                            .into_iter()
                            .filter(|diag| {
                                // N.B.: There should be at least one of these!
                                diag.severity()
                                    .is_none_or(|sev| sev == miette::Severity::Error)
                            })
                            .map(Report::new_boxed);
                        for report in error_reports {
                            eprintln!("{report:?}");
                        }
                        None
                    }
                })
                .collect::<Vec<_>>();

            if found_glob_parse_err {
                log::error!("failed to parse one or more globs for {what}; bailing");
                return Err(AlreadyReportedToCommandline);
            }

            globs
        };

        let paths_from_globs = {
            let mut found_glob_walk_err = false;
            let files = globs
                .iter()
                .flat_map(|(base_path, glob)| {
                    glob.walk(base_path)
                        .filter_map(|entry| match entry {
                            Ok(entry) => Some(entry.into_path()),
                            Err(e) => {
                                found_glob_walk_err = true;
                                let ctx_msg = if let Some(path) = e.path() {
                                    format!(
                                        "failed to enumerate {what} from glob `{}` at path {}",
                                        glob,
                                        path.display()
                                    )
                                } else {
                                    format!("failed to enumerate {what} from glob `{glob}`")
                                };
                                let e = Report::msg(e).wrap_err(ctx_msg);
                                eprintln!("{e:?}");
                                None
                            }
                        })
                        .collect::<Vec<_>>() // OPT: Can we get rid of this somehow?
                })
                .collect::<Vec<_>>();

            if found_glob_walk_err {
                log::error!(
                    concat!(
                        "failed to enumerate {} from globs, ",
                        "see above for more details"
                    ),
                    what
                );
                return Err(AlreadyReportedToCommandline);
            }

            files
        };

        if paths_from_globs.is_empty() && !globs.is_empty() {
            if paths.is_empty() {
                log::error!(
                    concat!(
                        "{} were specified exclusively via glob search, ",
                        "but none were found; bailing"
                    ),
                    what
                );
                return Err(AlreadyReportedToCommandline);
            } else {
                log::warn!(
                    concat!(
                        "{} were specified via path and glob search, ",
                        "but none were found via glob; ",
                        "continuing with direct paths"
                    ),
                    what
                )
            }
        }

        let exec_report_paths = paths
            .into_iter()
            .chain(paths_from_globs)
            .collect::<Vec<_>>();

        log::trace!("working with the following {what}: {exec_report_paths:#?}");
        log::info!("working with {} {what}", exec_report_paths.len());

        Ok(exec_report_paths)
    }
}
