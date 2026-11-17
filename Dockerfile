FROM rocker/shiny-verse:4.5.1
RUN install2.r --error --skipinstalled shinydashboard shinydashboardPlus shinyWidgets DT exuber glue jsonlite zoo rmarkdown readxl here
WORKDIR /app
COPY . /app
ENV PORT=3838 MALLOC_ARENA_MAX=2 MALLOC_TRIM_THRESHOLD_=131072
CMD ["sh", "-c", "R -e \"shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))\""]
