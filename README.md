# cloud-itonami-lei-549300w9jlpw15xifm52

> **Independent third-party archive/analysis. Not affiliated with, endorsed by, or sponsored by Telefonaktiebolaget LM Ericsson.**

This repository archives the publicly published legal notice of **Telefonaktiebolaget LM Ericsson** (SE), with source-url and retrieval-date provenance, per
ADR-2607110300 (`cloud-itonami-lei-corporate-tos-catalog`, `com-junkawasaki/root`).
Read-only reference/archive repository — not a governed Advisor/Governor actor.

- LEI: `549300W9JLPW15XIFM52` (GLEIF entity status ACTIVE, registration ISSUED)
- Source: https://www.ericsson.com/en/legal
- Retrieved: 2026-07-25T05:18:09Z
- SHA-256 of archived text: `d48532643ca1e4b8bd9a8f7a89c3ae93d444ed5d490b1e7fd667e5b1e07ecf1b`

## Files

| File | What it holds |
|---|---|
| `blueprint.edn` | The entity's identity: legal name, LEI, jurisdiction, website. |
| `facts.edn` | 35 verified registry facts with per-fact provenance. **Generated** — see below. |
| `80-data/public/tos.journal.edn` | The archived legal-notice text, as an `[e a v tx op]` journal. |
| `scripts/verify-facts.cljs` | Re-fetches every source `facts.edn` cites and fails if the live record disagrees. |

## Verifying the record

The LEI claims above used to be assertions with nothing in the repository behind
them. `facts.edn` now carries them as data, and every value in it was read out of
a public registry response whose URL and retrieval time sit next to the value:

```
nbb scripts/verify-facts.cljs           # check the recorded facts against the live sources
nbb scripts/verify-facts.cljs --write   # re-fetch and rewrite facts.edn
```

Eight GLEIF endpoints back the file — the LEI record, its ISINs, its managing
LOU, both pages of its 29 direct children, the ultimate-parent reporting
exception, registration authority `RA000544` (Bolagsverket), and ISO 20275 legal
form `XJHM` (Aktiebolag). All eight answered `200` when the file was written.

The checker exits **0** when every citation resolves and every recorded fact
still matches, **1** when a citation breaks or a fact drifts, and **3** when it
could not perform the check at all — an absent `facts.edn`, or every request
failing at the transport level. A check that could not run must not be
indistinguishable from a check that ran and found nothing, so it refuses to
report a pass rather than exiting 0.

### One source is not machine-retrievable

`https://www.ericsson.com/en/legal`, the origin of the archived text above,
returns **HTTP 403** to automated retrieval (measured 2026-08-20). The archived
copy and its SHA-256 stand as recorded on 2026-07-25, but they cannot presently
be re-derived from source by a script, and no attempt is made to work around the
publisher's bot protection. That URL is therefore deliberately **not** cited in
`facts.edn`, which holds only sources that answer.

## Joining

`:company/lei` is the join key, and it is present on all 35 entities — including
each of the 29 `:fact/kind :direct-child` records, whose LEIs are the group's
subsidiaries and resolve to their own GLEIF records.

`facts.edn` is not on the shared query plane yet: `manifest/edn-query.cljs` in
`com-junkawasaki/root` has loaders for `blueprint.edn` and `tos.journal.edn` and
none for this file, so these datoms are readable here but not yet joinable from
`edn-query`.

Acquired by `scripts/lei-acquire.cljs` as part of the worldwide-broadening
continuation that followed the 2026-07-25 coverage audit, which found the
catalog's real reach was 27 countries with the United States at 55%.
