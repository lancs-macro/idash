# idash

International housing observatory Shiny dashboard (single `app.R`). Deployed to shinyapps.io via `rsconnect` (`manifest.json`, `rsconnect/`); `content/` and `www/` hold supporting content and static assets, `R/` holds helper modules sourced by the app.

## Environment

- R version: managed with `rig` (currently 4.6.1 - `rig default 4.6.1`)
- Dependencies: managed with `rv` (`rproject.toml` + lockfile), not renv
  - Setup: `rv sync`
  - Add a dependency: `rv add <pkg>`
- Format: `air format .` (check only: `air format --check .`)
- Lint: `jarl check .` (autofix: `jarl check . --fix`)

The `rproject.toml`/`rv.lock` were migrated from a `renv.lock` that had been seeded from the deployed `rsconnect` manifest, so versions should still track deployed versions closely - re-verify `rv sync` against the deployed shinyapps.io manifest the next time this app is deployed.

### Rules
- Never use `install.packages()` or `renv::*` - all dependency changes go through `rv add`/`rv remove`.
- Run `air format .` then `jarl check .` before committing.
- No `# nolint` comments - jarl uses `# jarl-ignore <rule>: <reason>` on the line before the flagged code instead.

## Commands

- Run locally: `Rscript -e 'shiny::runApp()'`
- Deploy: `Rscript -e 'rsconnect::deployApp()'`

<!-- knowledge:start -->
## Knowledge wiki
This repo's wiki lives in `./wiki` (committed with the code; overview: `wiki/idash.md`). It is linked into the
shared vault at `./kb` (-> C:\Users\User\knowledge), where `kb/CLAUDE.md` holds the rules (frontmatter, wikilinks, INGEST/QUERY/LINT).
- Session start: read `kb/inbox/idash.md`, act on items, move them under `## Done`.
- Write the wiki to the repo wiki standard: `kb/REPO-WIKI.md` (layout, page types, frontmatter, links). Scaffold pages with `python kb/scripts/new_page.py idash architecture|decision|runbook|integration|concept "<Title>"`.
- Update the wiki page that describes a behaviour in the same change that alters it; then run `python kb/scripts/reindex.py` and `python kb/scripts/lint.py`.
- Find things with `python kb/scripts/search.py "<terms>"` before browsing `kb/wiki/INDEX.md`. Never edit `kb/raw/`.
<!-- knowledge:end -->
