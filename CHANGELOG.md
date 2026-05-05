# Changelog

All notable changes to the `louter` Haskell library will be documented in this file.

## 0.1.1.2 - 2026-04-30

### Added
- `newClientWithTimeout` entry point for creating a `Client` with a configurable HTTP response timeout (in seconds).
- `Show` instances for `Backend` to aid debugging. API keys are redacted in the rendered output.

### Changed
- OpenAI request serialization now omits empty optional fields (`tools`, `temperature`, `max_tokens`) instead of emitting `null`, improving compatibility with backends that reject null-valued fields.
- Single-text message content is now serialized as a plain string (`content: "..."`) instead of the typed-array form (`content: [{"type":"text","text":"..."}]`) when submitting requests.

### Acknowledgments
- Many thanks to [@julialongtin](https://github.com/julialongtin) (Julia Longtin) for contributing all of the changes in this release (PRs #1, #2, #3).

## 0.1.1.1 - 2025-12-25

Previous release.
