library(shiny, quietly = TRUE, warn.conflicts = FALSE)
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE)
# library(dashboardthemes)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(DT, quietly = TRUE, warn.conflicts = FALSE)


# Set Options -------------------------------------------------------------

src <- "primary" 
load <- TRUE

# Load everything ---------------------------------------------------------

if (load) {
  path_store <- list.files("data/RDS", full.names = TRUE)
  store <-  stringr::str_remove(list.files("data/RDS"), ".rds")

  for (i in seq_along(path_store)) assign(store[i], readRDS(file = path_store[i]))
}


# Source ------------------------------------------------------------------

if (src == "primary") {
  # suppressMessages(
    list.files(c("R"), full.names = TRUE, pattern = "-src.R") %>% 
      purrr::map(source)
  # )
}else {
  # suppressMessages(
    list.files(c("R"), full.names = TRUE, pattern = ".R") %>% 
      purrr::map(source)
  # )
}



# Header ------------------------------------------------------------------


header <- dashboardHeader(

  title =  "International Housing Observatory",
  titleWidth = 350

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
  tags$style(
    type = "text/css",
    '.box-title {color:rgb(49, 70, 88);}'
  ),
  tags$style(
    type = "text/css",
    '.box-subtitle {color:rgb(23, 87, 151);font-size:15px;}'
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
                box2(
                  title = "Real House Prices",
                  subtitle = "Peak-to-Trough Contraction Periods",
                  plotOutput("autoplot_datestamp_price")
                ),
                box2(
                  title = "House-Price-to-Income Ratio",
                  subtitle = "Peak-to-Trough Contraction Periods",
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
                             house-price-to-disposable-income ratios (housing affordability) 
                            starting in 1975, exuberance statistics, as well as date-stamping of the 
                            specific periods of exuberance.")
                  ),
                  column(6,
                         style = "padding-left:5em;",
                         selectInput("country", "Select Country:", cnames)
                  )
              ),
              
              
              fluidRow(
                box(
                  title = "Real House Prices",
                  plotOutput("plot_price"),
                  width = 6
                ),
                box(
                  title = "House-Price-to-Income Ratio", 
                  plotOutput("plot_income"),
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
                  title = "House-Price-to-Income Ratio", 
                  tableOutput("table2")
                )
              ),
              
              
              fluidRow(
                box(
                  width = 12,
                  background = "blue",
                  p("Date-Stamping Periods of Exuberance", 
                    style = "font-size:22px;text-align:center;")
                )
              ),
              
              fluidRow(
                box(
                  title = "Real House Prices",
                  plotOutput("autoplot_price"),
                  width = 6),
                box(
                  title = "House-Price-to-Income Ratio", 
                  plotOutput("autoplot_income"),
                  width = 6),
                p(
                  em(
                    span("There is exuberance when the"), 
                    span("statistic", style = "color:blue"), 
                    span("exceeds the"),
                    span("critical value.",  style = "color:red;")
                ), 
                style = "text-align:center;font-size:14px;")
              ),
              
              fluidRow(
                box(
                  width = 12,
                  background = "blue",
                  p("Date-Stamping Periods of Exuberance Table", 
                    style = "font-size:22px;text-align:center;")
                )
              ),
              fluidRow(
                box(
                  title = "Real House Prices",
                  dataTableOutput("table3")
                ),
                box(
                  title = "House-Price-to-Income Ratio", 
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
                                title = "House-Price-to-Income Ratio")
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
                                title = "House-Price-to-Income Exuberance Statistics"),
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
      exuber::datestamp(radf_price, cv = mc_con) %>% 
        exuber::autoplot()
    })
  output$autoplot_datestamp_income <- 
    renderPlot({
      exuber::datestamp(radf_income, cv = mc_con) %>% 
        exuber::autoplot()
    })
  
  
  # Analysis ----------------------------------------------------------------
  
  
  output$plot_price <- renderPlot({
    plot_var(price, input$country)})
  
  output$plot_income <- renderPlot({
    plot_var(price, input$country)})
  
  output$table1 <-
    renderTable({
      summary(radf_price, mc_con) %>% 
        purrr::pluck(input$country)},
      striped = TRUE, bordered = TRUE,  
      width = '100%', rownames = TRUE,
      align = 'ccccc')
  
  output$table2 <- renderTable({
    summary(radf_income, mc_con) %>% 
      purrr::pluck(input$country)},
    striped = TRUE, bordered = TRUE,  
    width = '100%', rownames = TRUE,
    align = 'ccccc')
  
  # autoplot_price
  
  autoplot_price_reactive <- 
    reactive({
      if (any(exuber::diagnostics(radf_price, mc_con)$accepted %in% input$country)) {
        autoplot_var(radf_var = radf_price, 
                     cv_var = mc_con, 
                     input = input$country)
      }else{
        NULL_plot()
      }
    })
  
  output$autoplot_price <- 
    renderPlot({
      autoplot_price_reactive()
    })
  
  # autoplot_income
  
  autoplot_income_reactive <- 
    reactive({
      if (any(exuber::diagnostics(radf_income, mc_con)$accepted %in% input$country)) {
        autoplot_var(radf_var = radf_income, 
                     cv_var = mc_con, 
                     input = input$country)
      }else{
        NULL_plot()
      }
    })
  
  output$autoplot_income <- 
    renderPlot({
      autoplot_income_reactive()
    })
  
  # table 3
  
  table3_reactive <- 
    reactive({
      if (any(exuber::diagnostics(radf_price, mc_con)$accepted %in% input$country)) {
        exuber::datestamp(radf_price, mc_con) %>%
          purrr::pluck(input$country) %>%
          to_yq(radf_price, cv_var = mc_con)
      }else{
       NULL
      }
    })
  
  
  output$table3 <- 
    DT::renderDataTable({
      table3_reactive()
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      dom = "t"))
  # table 4
  
  table4_reactive <- 
    reactive({
      if (any(exuber::diagnostics(radf_income, mc_con)$accepted %in% input$country)) {
        exuber::datestamp(radf_income, mc_con) %>%
          purrr::pluck(input$country) %>%
          to_yq(radf_income, cv_var = mc_con)
      }else{
        tibble::tibble()
      }
    })
  
  output$table4 <- 
    DT::renderDataTable({
      table4_reactive()
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      dom = "t"))
  

  

  # Data --------------------------------------------------------------------
  
  
  ### Data section
  citation_data <- HTML(glue::glue(
    "We would appreciate that anyone wishing to use this dataset, modified or 
otherwise, acknowledge the source of the data publicly available through this website with 
a citation of the working paper: for example, including a <br> 
statement such as, 'The authors acknowledge use of the dataset described in Mack and Martínez-García (2011).'"))
  citation_estimation <- HTML(glue::glue(
    "We would appreciate that anyone wishing to use this dataset, modified or 
  otherwise, acknowledge the source of the data publicly available through this website with 
  a citation of the working paper: for example, including a <br> 
  statement such as, 'The authors acknowledge use of the dataset described in Pavlidis et al. (2016).'"))
  
  # Raw
  
  output$data_price <- DT::renderDataTable({
    make_DT(price, "price", citation_data)
  })
  
  
  output$data_income <- renderDataTable({
    make_DT(price_income, "income", citation_data)
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
