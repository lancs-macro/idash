library(shiny)
library(shinydashboard)
library(dashboardthemes)

library(DT)
library(tidyverse)
library(glue)


# Set Options -------------------------------------------------------------

src <- "primary" 
load <- TRUE


# Source ------------------------------------------------------------------

if (src == "primary") {
  suppressMessages(
    list.files(c("Rscripts"), full.names = TRUE, pattern = "-src.R") %>% 
      map(source)
  )
}else {
  suppressMessages(
    list.files(c("Rscripts"), full.names = TRUE, pattern = ".R") %>% 
      map(source)
  )
}

# Load everything ---------------------------------------------------------

if (load) {
  store <- c((items <- c("price", "income")),
             c("cnames"),
             paste0("summary_", items),
             paste0("datestamp_", items),
             paste0("plot_", items),
             paste0("autoplot_", items),
             paste0("autoplot_datestamp_", items),
             paste0("estimation_", items),
            "cv_seq","cv_table")
  
  path_store <- paste0("data/RDS/", store, ".rds")
  
  for (i in seq_along(path_store)) assign(store[i], readRDS(file = path_store[i]))
}

# Header ------------------------------------------------------------------


header <- dashboardHeader(

  title =  "International Housing Observatory",
  titleWidth = 400

  # Return to original website
  # tags$li(
  #   a(href = "https://www.dallasfed.org/institute/houseprice",
  #     icon("power-off"),
  #     title = "Back to Fed's Website"),
  #   class = "dropdown"
  # )
)

# Sidebar -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home"),
                         selected = TRUE),
                menuItem("Overview", tabName = "overview", 
                         icon = icon("globe",  lib = "glyphicon")),
                menuItem("Analysis", tabName = "analysis", icon = icon("table")),
                menuItem("Data Sources & Methodology", tabName = "methodology",
                         icon = icon("chalkboard-teacher")),
                menuItem("Download Data", icon = icon("download"), 
                         menuSubItem("Raw Data", tabName = "download_raw",
                                     icon = icon("angle-right")),
                         menuSubItem("Exuberance", tabName = "download_exuberance",
                                     icon = icon("angle-right"))),
                hr()
    )
  )
)



# Body --------------------------------------------------------------------


body <- dashboardBody(
 
  # Make theme "html/theme.R"
  theme_boe_website,
  
  ######## Customization #################
  
  tags$head(
    
    # tags$link(rel = "icon", type = "image/jpg", href = "logo.jpg"),
    
    includeHTML("content/google-analytics.html"),
    
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  
  # Customize the red to red_lanc
  tags$style(
    type = 'text/css', 
    '.bg-blue {background-color: rgb(49, 70, 88)!important; }'
  ),
  # Move datatable button to the left
  tags$style(
    type = "text/css",
    'div.dt-buttons {float: right;}'
  ),
  
  
  
  # Overview ----------------------------------------------------------------
  
  
  tabItems(
    tabItem(tabName = "home",
            includeCSS("content/style.css"),
            includeHTML("content/home.html"),
            includeHTML("content/footer.html")
            
    ),
    
    tabItem(tabName = "overview",
            fluidPage(
                      
              h2("Overview",
                 style = "padding:1em 0 0 20px;"),
                      
              h3("Chronology of exuberance in international housing markets.", 
                 style = "padding:0 0 0 20px;"),
                      
              p("The below figures show the periods during which real house prices 
                and house-price-to-income ratios displayed explosive dynamics (i.e., 
                the periods during which the estimated BSADF statistics exceeded the 
                corresponding 95% critical values). Most prominently, they show the
                synchronization of exuberance across markets in the 2000s. ", 
                style = "padding:1em 0 2em 1.5em;"),
              
              fluidRow(
                box(title = "Real House Prices",
                    plotOutput("autoplot_datestamp_price")
                ),
                box(title = "Real Price to Disposable Income Ratio ",
                    plotOutput("autoplot_datestamp_income")
                )
              )
            ),
            includeHTML("content/footer.html")
    ),
    
    # Analysis --------------------------------------------------------------------
   
    
    tabItem(tabName = "analysis",
            fluidPage(style = "padding:0;",
              
                h2("Analysis",
                   style = "padding: 1em 0 0 1em;"),
              
              div(class = "row", 
                  style = "text-align:left;padding:2em;",
                  
                  column(6, 
                         p(
                           "This page provides figures for real house prices and
                             house price to disposable income ratios (housing affordability) 
                            starting in 1975, 
                             exuberance statistics, as well as date-stamping of the 
                            specific periods of exubernace.")
                  ),
                  column(6,
                         style = "padding-left:5em;",
                         selectInput("country", "Select Country:", cnames)
                  )
                  
              ),
              
              
              fluidRow(
                box(
                  title = "Real House Prices",
                  plotOutput("plot1"),
                  width = 6
                ),
                box(
                  title = "Real Price to Disposable Income Ratio", 
                  plotOutput("plot3"),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  background = "blue",
                  p("Exuberance Statistics", style = "font-size:22px;text-align:center;")
                )
              ),
              
              fluidRow(
                box(
                  title = "Real House Prices",
                  tableOutput("table1")
                ),
                box(
                  title = "Real Price to Disposable Income Ratio", 
                  tableOutput("table2")
                )
              ),
              
              
              fluidRow(
                box(
                  width = 12,
                  background = "blue",
                  p("Date Stamping Periods of Exuberance", 
                    style = "font-size:22px;text-align:center;")
                )
              ),
              
              fluidRow(
                box(
                  title = "Real House Prices",
                  plotOutput("plot2"),
                  width = 6),
                box(
                  title = "Real Price to Disposable Income Ratio", 
                  plotOutput("plot4"),
                  width = 6),
                p("There is exuberance when the statistic (blue line) exceeds the critical value 
                  (red line).", style = "text-align:center;")
              ),
              
              fluidRow(
                box(
                  width = 12,
                  background = "blue",
                  p("Date Stamping Periods of Exuberance Table", 
                    style = "font-size:22px;text-align:center;")
                )
              ),
              
              
              fluidRow(
                box(
                  title = "Real House Prices",
                  dataTableOutput("table3")
                ),
                box(
                  title = "Real Price to Disposable Income Ratio", 
                  dataTableOutput("table4")
                )
              )
            ),
            includeHTML("content/footer.html")
    ),
    
    
    
    # Data --------------------------------------------------------------------
    tabItem(tabName = "download_raw",
            
            fluidPage(
              h2("Download Raw Data"),
              br(),
              fluidRow(
                tabBox(width = 12,
                       side = "left",
                       tabPanel(dataTableOutput("data_price"), 
                                title = "Real House Prices"),
                       tabPanel(dataTableOutput("data_income"), 
                                title = "Real House Price to Income")
                )
              )
            ),
            includeHTML("content/footer.html")
    ),
    
    tabItem(tabName = "download_exuberance",
            
            fluidPage(
              h2("Download Exuberance Statistics"),
              br(),
              fluidRow(
                tabBox(width = 12, 
                       side = "left",
                       tabPanel(dataTableOutput("estimation_price"), 
                                title = "Real House Price Exuberance Statistics"),
                       tabPanel(dataTableOutput("estimation_income"), 
                                title = "Real House Price to Income Exuberance Statistics"),
                       tabPanel(dataTableOutput("cv_seq"), 
                                title = "BSADF Critical Value Sequence Statistics"),
                       tabPanel(dataTableOutput("cv_table"), 
                                title = "GSADF Statistics & Critical Values")
                )
              )
            ),
            includeHTML("content/footer.html")
    ),
    
    tabItem(tabName = "methodology",
            includeHTML("content/methodology.html"),
            includeHTML("content/footer.html")
    )
  )
)


server <- function(input, output, session) {
  
  
  # overview ----------------------------------------------------------------
  
  output$autoplot_datestamp_price <- 
    renderPlot({
      autoplot_datestamp_price
    })
  output$autoplot_datestamp_income <- 
    renderPlot({
      autoplot_datestamp_income
    })
  
  
  # Analysis ----------------------------------------------------------------
  
  
  output$table1 <- renderTable({
    summary_price[[input$country]]},
    striped = TRUE, bordered = TRUE,  
    width = '100%', rownames = TRUE,
    align = 'ccccc')
  
  output$table2 <- renderTable({
    summary_income[[input$country]]},
    striped = TRUE, bordered = TRUE,  
    width = '100%', rownames = TRUE,
    align = 'ccccc')
  
  output$table3 <- renderDataTable({
    datestamp_price[[input$country]]},
    options = list(searching = FALSE, 
                   ordering = FALSE,
                   dom = "t"))
  
  output$table4 <- renderDataTable({
    datestamp_income[[input$country]]},
    options = list(searching = FALSE, 
                   ordering = FALSE,
                   dom = "t"))
  
  
  # Plots -------------------------------------------------------------------
  
  
  
  output$plot1 <- renderPlot({plot_price[[input$country]]})
  
  output$plot2 <- renderPlot({autoplot_price[[input$country]]})
  
  output$plot3 <- renderPlot({plot_income[[input$country]]})
  
  output$plot4 <- renderPlot({autoplot_income[[input$country]]})
  
  
  # Data --------------------------------------------------------------------
  
  
  ### Data section
  citation_data <- glue::glue(
    "We would appreciate that anyone wishing to use this dataset, modified or 
otherwise, acknowledge the source of the data publicly available through this 
website with a citation of the working paper: for example, including a statement 
such as, 'The authors acknowledge use of the dataset described in Mack and Martínez-García (2011).'")
  citation_estimation <- glue::glue(
    "We would appreciate that anyone wishing to use this dataset, modified or 
  otherwise, acknowledge the source of the data publicly available through this 
  website with a citation of the working paper: for example, including a statement 
  such as, 'The authors acknowledge use of the dataset described in Pavlidis et al. (2016).'")
  
  # Raw
  
  output$data_price <- DT::renderDataTable({
    make_DT(price, "price", citation_data)
  })
  
  
  output$data_income <- renderDataTable({
    make_DT(income, "income", citation_data)
  })
  
  
  ### Estimation Statistics and Critical Values
  
  output$cv_table <- renderDataTable({
    make_DT_general(cv_table, "cv_table")
  })
  
  output$estimation_price <- renderDataTable({
    make_DT(estimation_price, "estimation_price", citation_estimation)
  })
  
  output$estimation_income <- renderDataTable({
    make_DT(estimation_income, "estimation_price", citation_estimation)
  })
  
  output$cv_seq <- renderDataTable({
    make_DT_general(cv_seq, "cv_sequence")
  })
  
  
  
}


# Launch ------------------------------------------------------------------

shinyApp(ui = dashboardPage(header, sidebar, body), server)
