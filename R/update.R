source("R/01-Manipulation.R")
source("R/02-Analysis.R")

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
  account = "lancs-macro", 
  server = "shinyapps.io", 
  appName = "international-housing-observatory",      
  appId = 705554, 
  launch.browser = function(url) {         
    message("Deployment completed: ", url)     
    }, 
  lint = FALSE, 
  forceUpdate = TRUE,
  metadata = list(asMultiple = FALSE, asStatic = FALSE),      
  logLevel = "verbose") 


library(git2r)

repo <- git2r::repository(appDir)
add(repo, ".")
commit("update version")
