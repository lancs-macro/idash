
# pkgs <- c("shiny", "shinydashboard", "shinydashboardPlus", "tidyverse","DT", "rlang")
# lapply(pkgs, library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)

suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(tidyverse)
  library(DT)
  library(shinyWidgets)
  library(exuber)
})


# update ------------------------------------------------------------------

# library(later)
# library(promises)
# library(future)
# 
# auto_shiny <- function(interval = 2*60*60){ # 2 hours 60 minutes 60 seconds
#   source("script source1")
#   source("shinyappscript")
#   later::later(shiny.auto, interval)
# }

# Set Options -------------------------------------------------------------

path_store_rds <- list.files("data/RDS", full.names = TRUE)
store_rds <-  stringr::str_remove(list.files("data/RDS"), ".rds")
for (i in seq_along(path_store_rds)) {
  assign(store_rds[i], readRDS(file = path_store_rds[i]))
}

# Source ------------------------------------------------------------------

idx <- tibble(Date = index(radf_price, trunc = FALSE))

suppressMessages({
  source("R/00-functions-src.R")
  # source("R/00-functions-src.R", local = TRUE)$value
})

# Version -----------------------------------------------------------------

vers <- price[nrow(price), 1][[1]] %>% 
  zoo::as.yearqtr()

# Header ------------------------------------------------------------------

header <- dashboardHeaderPlus(
  titleWidth = 430,
  title = shiny::tagList(
    span(
      class = "logo-lg",
      a(
        href = "https://int.housing-observatory.com/",
        span(
          shiny::img(src = "logo.png",  height = "30", width = "30"),
          HTML('<span class="name"> International </span>
             <span class= "bottom-name"> Housing Observatory </span>')
        )
      )
    ),
    shiny::img(src = "logo.png",  height = "30", width = "30")
  ),
  tags$li(
    class = "dropdown",
    p(
      class = "release", 
      glue::glue("Release: {vers}")
    )
  )
)

# Sidebar -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    sidebarMenu(
      id = "tabs",
      # menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),
      menuItem("Overview", tabName = "overview", selected = TRUE,
               icon = icon("globe",  lib = "glyphicon")),
      menuItem("Analysis", tabName = "analysis", icon = icon("table")),
      # menuItem("Data & Methodology", tabName = "methodology",
      #          icon = icon("chalkboard-teacher")),
      menuItem("Download Data", tabName = "download", 
               icon = icon("download")),
      hr()
    )
  )
)

# Body --------------------------------------------------------------------


body <- dashboardBody(
  
  # Make theme "html/theme.R"
  # theme_boe_website,
  
  ######## Customization #################
  
  tags$head(
    tags$meta(name = "description", content = "Real-time monitoring of real estate markets across the world"),
    tags$meta(name = "keywords", content = "housing Observatory, house prices, international house prices, exuberance indicators"),
    tags$title("International Housing Observatory"),
    tags$link(rel = "shortcut icon", href = "logo.png"),
    
    tags$link(rel = "stylesheet", type = "text/css", 
              href = 'https://fonts.googleapis.com/css?family=Gloria Hallelujah'),
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://use.fontawesome.com/releases/v5.5.0/css/all.css"),
    includeHTML("content/google-analytics.html")
    
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
  
  
  tabItems(
    
    # Home Tab ----------------------------------------------------------------    
    
    # tabItem(
    #   tabName = "home",
    #   includeCSS("content/style.css"),
    #   includeHTML("content/home.html"),
    #   includeHTML("content/footer.html")
    #   
    # ),
    
    # Overview Tab ------------------------------------------------------------
    
    tabItem(
      tabName = "overview",
      
      includeCSS("content/style.css"),
      
      fluidPage(
                h2("Overview", style = "padding:0 0 0 20px;"),
                
                h3("Chronology of exuberance in international housing markets", 
                   style = "padding:0 0 0 20px;"),
                
                p("The below figures show the periods during which real house prices 
          and house-price-to-income ratios displayed explosive dynamics 
          (i.e., the periods during which the estimated BSADF statistics 
          exceeded the corresponding 95% critical values). Most prominently, 
          they show the synchronization of exuberance across markets in the 
          2000s. ",  style = "padding:1em 0 2em 1.5em;"),
                fluidRow(
                  box2(
                    title = "Real House Prices",
                    subtitle = "Episodes of Exuberance",
                    plotOutput("autoplot_datestamp_price")
                  ),
                  box2(
                    title = "House-Price-to-Income Ratio",
                    subtitle = "Episodes of Exuberance",
                    plotOutput("autoplot_datestamp_income")
                  )
                ),
                fluidRow(
                  box2(
                    title = "Aggregate Real House Prices",
                    subtitle = "Index Levels",
                    plotOutput("plot_price_aggregate")
                  ),
                  box2(
                    title = "Aggregate House-Price-to-Income Ratio",
                    subtitle = "Index Levels",
                    plotOutput("plot_income_aggregate")
                  ),
                  p(
                    em(
                      span("NOTE:"),
                      span("Shaded areas ", style = "color:#B3B3B3"),
                      span(
                        "indicate contraction (peak to trough) of the 
                aggregate real house price index.")
                    ),  style = "text-align:center;font-size:14px;"),
                  br()
                ),
                fluidRow(
                  box2(
                    title = "Aggregate Real House Prices",
                    subtitle = "Growth Rates",
                    plotOutput("plot_growth_price_aggregate")
                  ),
                  box2(
                    title = "Aggregate House-Price-to-Income Ratio",
                    subtitle = "Growth Rates",
                    plotOutput("plot_growth_income_aggregate")
                  ),
                  p(
                    em(
                      span("NOTE: The "),
                      span("interquartile range", style = "color:#174B97"),
                      span(
                        "refers to the difference between the 
                upper and lower quartiles (the highest and lowest 25 percent) 
                of the growth rates across all countries.")
                    ), style = "text-align:center;font-size:14px;")
                )
      ),
      br(),
      includeHTML("content/footer.html")
    ),
    
    
    # Analysis Tab ------------------------------------------------------------
    
    tabItem(
      tabName = "analysis",
      fluidPage(
        
        h2("Analysis", 
           style = "padding: 0  0 0 1em;"),
        
        div(class = "row",  
            style = "text-align:left;padding:2em;",
            
            column(width = 6, 
                   p(
                     "This page provides figures for real house prices and
                     house-price-to-disposable-income ratios (housing affordability) 
                     starting in 1975, exuberance statistics, as well as date-stamping 
                     of the specific periods of exuberance.")
            ),
            column(width = 4,
                   style = "padding-left: 3em;",
                   selectInput("country", "Select Country:", cnames)
            ),
            column(width = 2,
                   style = "text-align:center;padding-top:1.5em;",
                   downloadButton(
                     class = "btn-report",
                     "report", "Download Report"),
                   p(HTML("(Generate Dynamic Report)"),
                     style = "font-size:11px; padding-top:1rem;")
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
            p("Exuberance Statistics", 
              style = "font-size:22px;text-align:center;")
          )
        ),
        
        fluidRow(
          box(
            title = "Real House Prices",
            dataTableOutput("table1")
          ),
          box(
            title = "House-Price-to-Income Ratio", 
            dataTableOutput("table2")
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
        br(),
        
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
    
    
    # Download Data Tab -------------------------------------------------------
    
    tabItem(
      tabName = "download",
      fluidPage(
        navlistPanel(
          well = TRUE,
          widths = c(3, 9),
          "Data",
          # "---------",
          tab_panel("data_price", "Real House Prices"),
          tab_panel("data_income", "House-Price-to-Income Ratio"),
          "Exuberance Statistics",
          tab_panel("cv_table", "Exuberance Statistics and Critical Values (GSADF)", 
                    prefix = "Exuberance Statistics: "),
          tab_panel("estimation_price", "Real House Prices Exuberance Statistics (BSADF)", 
                    prefix = "Exuberance Statistics: "),
          tab_panel("estimation_income", "House-Price-to-Income Exuberance Statistics (BSADF)", 
                    prefix = "Exuberance Statistics: "),
          tab_panel("cv_seq", "BSADF Critical Value Sequence Statistics", 
                  prefix = "Exuberance Statistics: ")
          # "---------",
          # tabPanel("Archive", icon = icon("angle-double-right"), 
          #          box2(width = 12, includeHTML("www/archive-table.html")))
        )
      ),
      includeHTML("content/footer.html")
    ),
    
    
    # tabItem(
    #   tabName = "download",
    #   
    #   fluidPage(
    #     style = "padding: 0 5em;",
    #     
    #     h2("Download", 
    #        style = "padding:1em 0 0 20px;"),
    #     h3("1) Exuberance Statistics", 
    #        style = "padding:0 0 0 20px;"),
    #     br(),
    #     fluidRow(
    #       tabBox(width = 12, 
    #              side = "left",
    #              tabPanel(dataTableOutput("estimation_price"), 
    #                       title = "Real House Price Exuberance Statistics"),
    #              tabPanel(dataTableOutput("estimation_income"), 
    #                       title = "House-Price-to-Income Exuberance Statistics"),
    #              tabPanel(dataTableOutput("cv_seq"), 
    #                       title = "BSADF Critical Value Sequence Statistics"),
    #              tabPanel(dataTableOutput("cv_table"), 
    #                       title = "GSADF Statistics & Critical Values")
    #       )
    #       
    #     ),
    #     h3("2) Raw Data", 
    #        style = "padding:0 0 0 20px;"),
    #     br(),
    #     fluidRow(
    #       tabBox(width = 12,
    #              side = "left",
    #              tabPanel(dataTableOutput("data_price"), 
    #                       title = "Real House Prices"),
    #              tabPanel(dataTableOutput("data_income"), 
    #                       title = "House-Price-to-Income Ratio")
    #       )
    #     )
    #   ),
    #   includeHTML("content/footer.html")
    # ),
    
    # Methodology Tab ---------------------------------------------------------
    
    tabItem(
      tabName = "methodology",
      includeHTML("content/methodology.html"),
      includeHTML("content/footer.html")
    )
  )
)

server <- function(input, output, session) {
  
  # Overview ----------------------------------------------------------------
  
  output$autoplot_datestamp_price <- 
    renderPlot({
      autoplot_datestamp_price
    })
  
  output$autoplot_datestamp_income <- 
    renderPlot({
      autoplot_datestamp_income
    })
  
  output$plot_price_aggregate <- 
    renderPlot({
      plot_var(price, "Aggregate", rect = TRUE,
               rect_data = exuber::datestamp(radf_price, mc_con)[["Aggregate"]]) 
      
    })
  
  output$plot_income_aggregate <- 
    renderPlot({
      plot_var(price_income, "Aggregate", rect = TRUE,
               rect_data = exuber::datestamp(radf_income, mc_con)[["Aggregate"]])
    })
  
  output$plot_growth_price_aggregate <- 
    renderPlot({
      plot_growth_var(
        price, "Aggregate",
        rect_data = exuber::datestamp(radf_price, mc_con)[["Aggregate"]]) 
    })
  
  output$plot_growth_income_aggregate <- 
    renderPlot({
      plot_growth_var(
        price_income, "Aggregate",
        rect_data = exuber::datestamp(radf_income, mc_con)[["Aggregate"]])
    })
  
  # Analysis ----------------------------------------------------------------
  
  output$report <- downloadHandler(
    
    filename = function() {
      paste0("Analysis-", input$country, "-", gsub(" ", "", vers), ".pdf")
    },
    # filename = "mpla.pdf",
    
    content = function(file) {
      withProgress(
        value = 0.2,
        message = 'Rendering, please wait!', {
          
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("R/report.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(
            country = input$country, 
            version = paste0(" ", gsub(" ", ":", vers)),
            data_price = price,
            data_price_income = price_income,
            rprice = radf_price,
            rincome = radf_income,
            cv = mc_con
          )
          
          rmarkdown::render(
            tempReport, 
            output_file = file, 
            params = params,
            envir = new.env(parent = globalenv())
          )
          shiny::setProgress(1) 
        }
      )
    }
  )
  
  
  output$plot_price <- renderPlot({
    plot_var(price, input$country)})
  
  output$plot_income <- renderPlot({
    plot_var(price_income, input$country)})
  
  output$table1 <-
    DT::renderDataTable(server = FALSE, {
      DT_summary(
        summary(radf_price, mc_con) %>% 
          purrr::pluck(input$country) %>% 
          mutate(name = toupper(name)) %>% 
          set_names(c("", "tstat", "90%", "95%", "99%"))
      )
    })
  
  output$table2 <- 
    DT::renderDataTable(server = FALSE, {
      DT_summary(
        summary(radf_income, mc_con) %>% 
          purrr::pluck(input$country) %>% 
          mutate(name = toupper(name)) %>% 
          set_names(c("", "tstat", "90%", "95%", "99%"))
      )
    })
  # output$table2 <- renderTable({
  #   summary(radf_income, mc_con) %>% 
  #     purrr::pluck(input$country)},
  #   striped = TRUE, bordered = TRUE,  
  #   width = '100%', rownames = TRUE
  #   # align = 'ccccc'
  #   )
  
  # autoplot_price
  autoplot_price_reactive <- 
    reactive({
      if (any(exuber::diagnostics(radf_price, mc_con)$positive %in% input$country)) {
        autoplot(radf_price, mc_con, select_series = input$country) + 
          ggtitle("") + scale_custom(idx)
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
      if (any(exuber::diagnostics(radf_income, mc_con)$positive %in% input$country)) {
        autoplot(radf_income, mc_con, select_series = input$country) + 
          ggtitle("") + scale_custom(idx)
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
      if (any(exuber::diagnostics(radf_price, mc_con)$positive %in% input$country)) {
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
      if (any(exuber::diagnostics(radf_income, mc_con)$positive %in% input$country)) {
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
  
  output$data_price <- DT::renderDataTable(server = FALSE, {
    make_DT(price, "price", citation_data)
  })
  
  output$data_income <- DT::renderDataTable(server = FALSE, {
    make_DT(price_income, "income", citation_data)
  })
  
  ### Estimation Statistics and Critical Values
  
  output$estimation_price <- DT::renderDataTable(server = FALSE, {
    make_DT(estimation_price, "estimation-price", citation_estimation)
  })
  
  output$estimation_income <- DT::renderDataTable(server = FALSE, {
    make_DT(estimation_income, "estimation-income", citation_estimation)
  })
  
  output$cv_seq <- DT::renderDataTable(server = FALSE, {
    make_DT_general(cv_seq, "cv-sequence")
  })
  
  output$cv_table <- DT::renderDataTable(server = FALSE, {
    make_DT_general(cv_table, "cv-table")
  })
  
}

# Launch ------------------------------------------------------------------

shinyApp(ui = dashboardPagePlus(skin = "black", title = "Dashboard | UK Housing Observatory",  
                                header, sidebar, body), server)
