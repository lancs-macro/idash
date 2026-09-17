# idash

International housing observatory Shiny dashboard (single `app.R`). Deployed to shinyapps.io via `rsconnect` (`manifest.json`, `rsconnect/`); `content/` and `www/` hold supporting content and static assets, `R/` holds helper modules sourced by the app.

## Environment

Dependencies are pinned with `renv` (installer backend: `pak`, enabled via `.Rprofile`). The lockfile was seeded from the deployed `rsconnect` manifest, so it should track deployed versions closely.

- Restore: `Rscript -e 'renv::restore()'`
- Add a dependency: `Rscript -e 'renv::install("pkg"); renv::snapshot()'`
- After changing dependencies: `Rscript -e 'renv::snapshot()'`

## Commands

- Run locally: `Rscript -e 'shiny::runApp()'`
- Deploy: `Rscript -e 'rsconnect::deployApp()'`
