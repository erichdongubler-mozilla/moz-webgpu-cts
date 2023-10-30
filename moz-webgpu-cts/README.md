# `moz-webgpu-cts`

A (WIP) tool for analyzing and adjusting [Web Platform Tests] data in
a checkout of [`mozilla-central`]. Currently tailored exclusively towards the
WebGPU team and its use cases.

[Web Platform Tests]: https://web-platform-tests.org/
[`mozilla-central`]: https://hg.mozilla.org/mozilla-central/

## Usage

All of `moz-webgpu-cts`' usage documentation is embedded into its `--help` flag.
@ErichDonGubler isn't going to try to impress you here; if you need it, see
[installation](#Installation) and try things out yourself; if not, ask him
directly whether this tool is interesting for you.

## Installation

You can install `moz-webgpu-cts` via Cargo's `install` subcommand with the
`--git` option pointing at this repo:

```sh
cargo install --git https://github.com/ErichDonGubler/moz-webgpu-cts
```
