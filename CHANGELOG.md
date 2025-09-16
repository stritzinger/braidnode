# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.1] - 2025-09-16
### Added
* ExDoc configuration.
* `links` metadata pointing to GitHub and the Changelog.

### Changed
* Move Orchestrator API doc to `docs/orchestrator-api.md` (from `doc/`).

## [1.0.0] - 2025-09-09
### Added
* `auto_connect` option for connection behavior.

### Changed
* Verification hardening: support a limited allowed-device list in TLS verify fun.

### Fixed
* Typo fixes in `sys.config`.

### Security
* Rotated to a new CA for TLS.

> Note: No breaking changes since 0.2.0; promoting the API to stable.

## [0.2.0] - 2025-02-14
### Added
* Ping neighbours in loop to improve liveness signals.

### Changed
* Replace `jiffy` with native OTP JSON support.
* Drop custom OTP image.
* Switch Docker base to official `erlang:27-*` images.
* Adjust TLS distribution options for OTP 27.
* Change braidnet API port.
* Updated rebar app template and developer guidance.
* Update signing API and related crypto handling.
* Increase RPC timeout to 60_000.
* Remove `git_url_rewrites` from `rebar3_docker` default settings.

### Fixed
* Configuration typos and minor robustness improvements.
* Fix match in ping loop.

### Security
* Use TLS for internal braidnode-braidnet connection.
* Set `fail_if_no_peer_cert = true` for server-side SSL dist.
* Print a warning if `epmd` is not started when attempting registration.

## [0.1.0] - 2023-07-01
### Added
* Initial release.

[unreleased]: https://github.com/stritzinger/braidnode/compare/1.0.1...HEAD
[1.0.1]: https://github.com/stritzinger/braidnode/compare/1.0.0...1.0.1
[1.0.0]: https://github.com/stritzinger/braidnode/compare/0.2.0...1.0.0
[0.2.0]: https://github.com/stritzinger/braidnode/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/stritzinger/braidnode/releases/tag/0.1.0
