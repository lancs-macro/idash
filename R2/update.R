library(shiny)
source("R2/00-functions-src.R")
source("R2/01-Manipulation.R")
source("R2/02-Analysis.R")

options(
  repos = structure(
    c(CRAN = 'https://cran.rstudio.com/'), RStudio = TRUE), 
  download.file.method = 'wininet')
options(rsconnect.check.certificate = TRUE)

appDir <- usethis::proj_path()

manifestLines <- rsconnect:::bundleFiles(appDir)
exclude <- grep("Manipulation|Analysis|update", manifestLines)
appLines <- manifestLines[-exclude]

rsconnect::deployApp(
  appDir = appDir,      
  appFiles = appLines,
  appTitle = "International Housing Observatory Dashboard",
  account = "lancs-macro", 
  server = "shinyapps.io", 
  appName = "idash",
  appId = 197422, 
  launch.browser = function(url) {         
    message("Deployment completed: ", url)     
    }, 
  lint = FALSE, 
  forceUpdate = TRUE,
  metadata = list(asMultiple = FALSE, asStatic = FALSE),      
  logLevel = "verbose") 



# push to github ----------------------------------------------------------

library(gert)

# library(git2r)

# repo <- git2r::repository(appDir)
# 
# disp_vers <- price[nrow(price), 1][[1]] %>% 
#   zoo::as.yearqtr()
# 
# add(repo, ".")
# commit(repo, message = glue::glue("update to version {disp_vers}"))
# push(repo, credentials = cred_token()) ##ssh path
