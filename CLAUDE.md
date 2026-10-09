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
