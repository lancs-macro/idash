---
title: idash
type: project
status: active
created: 2026-10-27
updated: 2026-11-04
sources: []
scope: housing-observatory
tags: [project, frontend]
verified_commit: bc20578ca5f8165db468659db54fc56f09504af9
files: [app.R, R, content, rproject.toml, manifest.json]
---
# idash

Shiny dashboard of the International Housing Observatory: real house price indices and exuberance (bubble) statistics by country.

Part of [[concepts/housing-observatory]]. Embedded in [[projects/housing-site-int/housing-site-int]].

## Stack
R Shiny (`shiny`, `shinydashboard`, `shinydashboardPlus`, `DT`, `shinyWidgets`, `tidyverse`) and the `exuber` package. R 4.6.1 via `rig`; dependencies with `rv` (`rproject.toml` + `rv.lock`), never `install.packages()` or `renv`; format with `air format .`, lint with `jarl check .` before committing (repo `CLAUDE.md`).

## Data
`app.R` reads the data API ([[projects/housing-api/housing-api]]): it takes the newest release from `api.housing-observatory.com/datasets/int/index.json`, then loads `<release>/rhpi.json` and `<release>/rhpi-radf.json`.

## Run and test
`Rscript -e 'shiny::runApp()'`. No automated tests.

## Layout
- `app.R` single-file app
- `R/` helper modules (`00-functions-src.R`, `021-update-exuberance-fundamentals.R`, `ui-exuberance-fundamentals.R`)
- `content/` home and methodology HTML, footer, analytics snippet, `style.css`; `www/` static assets
- `manifest.json`, `rsconnect/` deploy metadata

## Unknowns
- Host: `CLAUDE.md` says shinyapps.io (`rsconnect::deployApp()`), but the site links `kvasilopoulos-idash.share.connect.posit.cloud`. Treat Posit Connect Cloud as current (unverified) and fix whichever is stale.
- `CLAUDE.md` asks to re-verify `rv sync` against the deployed manifest at the next deploy.

Related: [[concepts/housing-observatory]], [[conventions]], [[inbox/idash]].

## Analytics
GA4 tag `G-ZHNETED6TG` (property "Housing Observatory - GA4"), loaded from `idash/wiki/idash.md/google-analytics.html` in the app head. The ID lives once, in `scripts/set_ga.py` (`GA_ID`); `python scripts/set_ga.py` rewrites it, `--check` verifies. Replaced the dead Universal Analytics tag (idash) and added the missing tag (ukdash) on 2026-11-04. After a republish, hits show under the `kvasilopoulos-content.share.connect.posit.cloud` hostname.

## Analytics
GA4 tag `G-ZHNETED6TG` (property "Housing Observatory - GA4"), loaded from `idash/google-analytics.html` in the app head. The ID lives once, in `scripts/set_ga.py` (`GA_ID`); `python scripts/set_ga.py` rewrites it, `--check` verifies. Replaced the dead Universal Analytics tag (idash) and added the missing tag (ukdash) on 2026-11-04. After a republish, hits show under the `kvasilopoulos-.share.connect.posit.cloud` hostname.
