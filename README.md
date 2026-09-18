# privgen

Generates a privacy policy and a terms of service document for each of our
games, from a spec file per game, and serves them at stable URLs.

```
GET /privacy/<slug>     the privacy policy
GET /terms/<slug>       the terms of service
GET /healthz            how many games are loaded
GET /audit/<kind>/<slug>  why each clause was included (PRIVGEN_AUDIT=1 only)
```

> **This produces a draft, not legal advice.** The clause corpus was written
> against the regulations named below, but only counsel can tell you whether it
> is right for a particular game in a particular market. Nothing here is
> published until it has been reviewed.

## How it works

A game is described by a YAML file in `games/`, named after its slug:

```yaml
slug: donut-rush-3d
name: Donut Rush 3D
audience: general
regions: [eea, uk, us-ca, us-other, turkey, rest-of-world]
sdks: [applovin-max, gameanalytics, adjust]
monetization: [banner-ads, rewarded-ads, iap-consumable]
```

A spec states **facts about the game** and nothing else. It never names a
clause, and it never restates what an SDK does — "AdMob receives an advertising
ID, and that counts as a sale under the CCPA" is a fact about AdMob, so it
lives in `src/Privgen/Catalog.hs`. If specs restated it, they would drift and
the documents would start lying.

Each clause in the corpus carries a **condition** — a value, not a function —
describing when it applies:

```haskell
clause "privacy.rights.us.sale-share" (AnySdkWith SellsOrShares) $ \env -> ...
  `citing` ["CCPA § 1798.120"]
```

Because conditions are data, the selection can be tested without rendering
anything, analysed for gaps across the whole corpus, and explained to a human.

## Running it

```sh
stack build
stack exec privgen-exe
```

| Variable | Default | Meaning |
|---|---|---|
| `PRIVGEN_SPECS` | `games` | Directory of spec files |
| `PRIVGEN_PORT`  | `8080`  | Port to bind |
| `PRIVGEN_AUDIT` | unset   | `1` exposes `/audit` |

`games/` and `static/` are resolved relative to the working directory, and both
are shipped in the release tarball.

**The server refuses to start if anything is wrong** — an unparseable spec, an
unknown SDK id, a duplicate clause id, a child-directed game shipping a
behavioural ad SDK. Every document is also rendered once at boot. A broken
deploy is far better than a broken policy at a URL an app store points at.

Findings that are gaps in the *business* rather than in the *file* — a missing
Art. 27 representative, for instance — print an advisory on every boot but do
not block startup. Taking a live policy URL offline to protest a paperwork
omission would help nobody.

## Adding a game

1. Write `games/<slug>.yaml`. The filename must match the `slug:` field.
2. `stack test` — the loader validates it and the goldens will show the diff.
3. Commit it. **Spec files are version-controlled on purpose**: they are the
   source of truth for published legal documents, and the history is the record
   of what was published when.

If a spec names an SDK that isn't in the catalog, the load fails and lists the
valid ids. Add the SDK to `src/Privgen/Catalog.hs` — `sdkEntry` is a total
function, so the compiler will not let you add one without supplying every
legal fact about it.

## What the corpus covers

- **GDPR / UK GDPR** — lawful bases, retention, recipients, international
  transfers, Arts. 15–22 rights, Art. 27 representatives, the right to complain
  to a supervisory authority.
- **CCPA / CPRA and other US state laws** — notice at collection, Do Not Sell
  or Share, sensitive-data limits, universal opt-out signals, appeals,
  non-discrimination.
- **COPPA, as amended** — the 2025 amendments took full effect on 22 April
  2026: separate verifiable parental consent before third-party disclosure, a
  retention limit for children's data, contextual-only advertising.
- **KVKK** — Art. 11 rights and the 30-day response, and the Art. 9 transfer
  regime as amended in 2024 (standard contracts notified to the Authority
  within five business days).
- **App store requirements** — the `#DataDeletion` anchor that Google Play
  deep-links, and Apple's minimum EULA terms including Apple as a third-party
  beneficiary.

## Reviewing a change

`renderAudit` produces a plain-text view of which clauses fired and why:

```
## rights-us
  privacy.rights.us.sale-share
    cites: CCPA § 1798.120; CPRA § 1798.121
    [x] AnySdkWith SellsOrShares  -- SellsOrShares matched by applovin-max
```

This, not the HTML, is what a reviewer should read. It is also captured as a
golden file, so every wording or selection change arrives as a reviewable diff
on a file named after the game:

```sh
PRIVGEN_GOLDEN_UPDATE=1 stack test   # capture or refresh
git diff test/golden/                # then read what changed
```

Until they have been captured once and committed, the golden tests report as
pending rather than failing.

## Layout

| Path | What |
|---|---|
| `src/Privgen/Types.hs` | Shared vocabulary — regions, data categories, purposes |
| `src/Privgen/Catalog.hs` | Third-party SDK facts |
| `src/Privgen/Spec.hs` | Spec types and decoding |
| `src/Privgen/Validate.hs` | Semantic checks; `ValidSpec` |
| `src/Privgen/Condition.hs` | The condition ADT and its explanation trace |
| `src/Privgen/Document.hs` | Clauses, sections, selection, corpus lint |
| `src/Privgen/Corpus/` | The actual text |
| `src/Privgen/Render.hs` | Blaze rendering |
