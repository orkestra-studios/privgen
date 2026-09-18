# Changelog

## 2.0.0 — 2026-09-19

A rewrite. The tool now generates documents from a spec per game instead of
serving one hardcoded policy, and produces a terms of service alongside the
privacy policy.

### Added

- **Terms of service** at `/terms/<slug>`, covering virtual items and currency,
  in-app purchases and the EU/UK/TR withdrawal right, advertising, Apple's
  minimum EULA terms, consumer-rights carve-outs on the liability limits, and
  optional US arbitration (off by default).
- **Per-game YAML specs** in `games/`, replacing the two-line text format.
  Unknown fields are rejected, so a typo cannot silently drop a disclosure.
- **Regulation-aware clauses** for GDPR/UK GDPR, CCPA/CPRA and other US state
  laws, COPPA as amended in 2025, and KVKK including the 2024 Art. 9 transfer
  regime. The previous policy named no regulation at all.
- **An SDK catalog** replacing the `Publisher` enum. One entry drives the GDPR
  recipients list, the CCPA sale/share disclosure, the COPPA third-party check,
  and the KVKK transfer clause.
- **An audit view** (`/audit`, and golden files) showing which clauses fired and
  why, for legal review.
- **Tests** — clause selection, corpus lint, slug safety, and a check that the
  `#DataDeletion` anchor survives every scenario.
- `/healthz`, and `PRIVGEN_SPECS` / `PRIVGEN_PORT` / `PRIVGEN_AUDIT`.

### Fixed

- **Path traversal.** `findGame` concatenated an unvalidated URL capture into a
  file path. Slugs are now validated at parse time, and specs are loaded once at
  boot, so no request touches the filesystem.
- **404s returned HTTP 200.** A missing game served a "not found" page with a
  200 status, which reads to a store crawler as a live policy URL.
- **Four of six publishers rendered nothing.** `partners _ = Html.p ""` emitted
  an empty paragraph while the data-deletion section still told readers to use
  the partner contacts "provided above". A section with no applicable clauses is
  now dropped, and cannot be constructed empty.
- **The shipped game data produced a broken policy.** Both files in `games/`
  were one line, but `loadGame` required two, so both fell through to
  `Game "" Self` — an empty game name and no partner disclosure.
- **A partial `read`** on the publisher line.
- **"This Html is used to inform visitors"** and two more occurrences, from a
  bad find/replace in `fe7c79b`.
- **A dangling reference** to "our Terms and Conditions, which is accessible at
  <game name>", which pointed at nothing. It is now a link to `/terms/<slug>`.
- **No effective date** on generated documents.
- `- Wall` in the test `ghc-options`, which GHC read as an input filename; the
  test suite had never built. `-Wall` now applies to the library too.
- The release workflow shipped a bare binary without `games/` or `static/`,
  both of which are resolved relative to the working directory.

### Changed

- Resolver LTS 20.11 (GHC 9.2.5) → LTS 23.28 (GHC 9.8.4), by name rather than
  raw snapshot URL. All dependencies now carry version bounds.
- CI builds and tests on every push and pull request, not only on release.
- `games/` is version-controlled. It was gitignored, which is the wrong
  treatment for the source of truth of published legal documents.
- `privgen.cabal` is no longer committed; `package.yaml` is the source of truth.
- Removed `src/Html.txt`, a template engine for a `web/` directory that has
  never existed.
