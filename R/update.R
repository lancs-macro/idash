library(shiny)
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
  appName = "international-housing-observatory-dashboard",      
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

disp_vers <- price[nrow(price), 1][[1]] %>% 
  zoo::as.yearqtr()

add(repo, ".")
commit(repo, message = glue::glue("update to version {disp_vers}"))
push(repo, credentials = cred_token()) ##ssh path
