
# pkgs <- c("shiny", "shinydashboard", "shinydashboardPlus", "tidyverse","DT", "rlang")
# lapply(pkgs, library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
# install_github("kvasilopoulos/exuber")

suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(tidyverse)
  library(DT)
  library(shinyWidgets)
  library(exuber)
})

# * Download ------------------------------------------------------------------


rel <- "https://api.housing-observatory.com/datasets/int/index.json" %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  .$releases %>% 
  as.vector() %>% 
  .[1]

price <- glue::glue("https://api.housing-observatory.com/datasets/int/{rel}/rhpi.json") %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))
  

radf_price <- glue::glue("https://api.housing-observatory.com/datasets/int/{rel}/rhpi-radf.json") %>%
  read_lines() %>% 
  jsonlite::unserializeJSON(.)


price_income <- glue::glue("https://api.housing-observatory.com/datasets/int/{rel}/pti.json") %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))


radf_income <- glue::glue("https://api.housing-observatory.com/datasets/int/{rel}/pti-radf.json") %>%
  read_lines() %>% 
  jsonlite::unserializeJSON(.)

mc_con <- radf_crit[[NROW(price)]]

psyivx_data <- 
  glue::glue("https://api.housing-observatory.com/datasets/int/{rel}/psyivx-data.json") %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(
    country = as.factor(country),
    Date = as.Date(Date)
  ) %>% 
  arrange(country, Date)

psyivx_ds <- 
  glue::glue("https://api.housing-observatory.com/datasets/int/{rel}/psyivx-ds.json") %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  mutate(Start = as.Date(Start), End = as.Date(End)) %>% 
  filter(Signal == "positive")
psyivx_countries <- unique(psyivx_data$country)
psyivx_countries_ds <- unique(filter(psyivx_ds, type == "psyivx")$country)



# * Wrangle ------------------------------------------------------------------

estimation_price <- 
  radf_price %>%
  .$bsadf %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

estimation_income <- 
  radf_income %>%
  .$bsadf %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

cv_seq <- mc_con %>% 
  .$bsadf_cv %>% 
  as_tibble() %>% 
  "["(-1,) %>% 
  bind_cols(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

cv_table <- 
  tibble(
    Countries = names(price)[-1],
    `Real House Prices` = radf_price$gsadf,
    `House-Price-Income` = radf_income$gsadf,
    `90% Critical Values` = mc_con$gsadf_cv[1],
    `95% Critical Values` = mc_con$gsadf_cv[2],
    `99% Critical Values` = mc_con$gsadf_cv[3]
  )


# * Version -----------------------------------------------------------------

idx <- tibble(Date = index(radf_price, trunc = FALSE))

cnames <- series_names(radf_price)

vers <- price[nrow(price), 1][[1]] %>%
  zoo::as.yearqtr()


# Header ------------------------------------------------------------------

header <- dashboardHeader(
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
      menuItem("Overview", tabName = "overview", selected = T,
               icon = icon("globe",  lib = "glyphicon")),
      menuItem("Analysis", tabName = "analysis", icon = icon("table")),
      menuItem("Analysis Price-to-Rent", tabName = "exuber", selected = F, icon = icon("chart-area")),
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
    
    # ** Overview Tab ------------------------------------------------------------
    
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
            )#,
            # column(width = 2,
            #        style = "text-align:center;padding-top:1.5em;",
            #        downloadButton(
            #          class = "btn-report",
            #          "report", "Download Report"),
            #        p(HTML("(Generate Dynamic Report)"),
            #          style = "font-size:11px; padding-top:1rem;")
            # )
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
          box2(
            width = 12,
            background = "blue",
            p("Exuberance Statistics", style = "font-size:22px;text-align:center;"),
            p("(First Stage)", style = "font-size:16px;text-align:center;")
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
          box2(
            width = 12,
            background = "blue",
            p("Date-Stamping Periods of Exuberance", 
              style = "font-size:22px;text-align:center;"),
            p("(Second Stage)", style = "font-size:16px;text-align:center;")
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
          uiOutput("exuberance_note"),
          br()
        ),
        
        fluidRow(
          box2(
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
    
    
    # ** Analysis Price-to-Rent -----------------------------------------------
    
    tabItem(
      tabName = "exuber",
      
      fluidPage(
        
        h2("Analysis with the PSY-IVX method", style = "padding:0 0 0 20px;"),
        
        
        fluidRow(
          column(
            width = 6,
            h3("An application to the price-to-rent ratio", 
               style = "padding:0 0 0 20px;"),
            
            p("This page provides figures for the price-to-rent (black line) starting in 1975 whenever 
                 possible for a selection of the countries covered in the database. The page also provides a 
                 fundamental-based measure (red line) of the price-to-rent ratio based on two explanatory variables (long-term interest rates 
                 and the log rent), exuberance statistics obtained from the the difference 
                 between the price-to-rent ratio and the fundamental-based prediction, as well as date-stamping of the specific periods of exuberance.",  
              style = "padding:1em 0 0 1.5em;"),
            
            p("The predictive regression is trained on the sample up to 2019:Q4, therefore excluding the pandemic, as indicated by 
                the dashed vertical lines. The functionality added to the webpage includes a button that allows to consider this predictive 
                model or to exclude it altogether.",  
              style = "padding:1em 0 2em 1.5em;"),
          ),
          column(
            width = 6,
            
            selectInput(
              "psyivx_country",
              "Select Country:",
              c ("All", as.character(psyivx_countries))
            ),
            
            p("Choose model:", style = "font-weight:600;"),
            
            p("When activated this checkbox uses exuberance statistics obtained from the the difference between the price-to-rent ratio 
            and the fundamental-based prediction. Otherwise we use the natural log of the price-to-rent ratio."),
            switchButton(inputId = "go_psyivx",
                         label = NULL, 
                         value = TRUE, col = "GB", type = "OO")
          )
        ), 
        uiOutput("psyivx_panel"),
      ),
      includeHTML("content/footer.html")
    ),
    
    
    # ** Download Data Tab -------------------------------------------------------
    
      
    
    tabItem(
      tabName = "download",
      fluidPage(
        titlePanel("Download Data"),
        div(
          class = "row",
          style = "padding-bottom:10px;",
          column(
            width = 2, 
            style = "padding-right:0px;",
            p("Set Year-Quarter Format:", style = "font-weight:600; font-size: 18px;padding-top:25px;")
          ),
          column(
            width = 1, 
            style = "padding-left:0px;",
            switchButton(inputId = "go_yqtr", label = NULL, value = FALSE, col = "GB", type = "OO")
          )
        ),
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
                    prefix = "Exuberance Statistics: "),
          "Analysis Price-to-Rent Statistics",
          tab_panel("psyivx_data", "Data", prefix = "PSYIVX Statistics:")
        ),
        
      ),
      includeHTML("content/footer.html")
    )
  )
)


server <- function(input, output, session) {
  
  # * Overview ----------------------------------------------------------------
  
  output$autoplot_datestamp_price <- 
    renderPlot({
      radf_price %>%  
        datestamp(cv = mc_con) %>% 
        autoplot() +
        scale_custom(idx) 
    })
  
  output$autoplot_datestamp_income <- 
    renderPlot({
      radf_income %>% 
        datestamp(cv = mc_con) %>% 
        autoplot() +
        scale_custom(idx) 
    })
  
  output$plot_price_aggregate <- 
    renderPlot({
      price %>% 
        select(Date, starts_with("Aggregate")) %>% 
        set_names(c("Date", "Fixed Weights (2005)", "Dynamic Weights")) %>% 
        pivot_longer(-Date) %>% 
        ggplot(aes(Date, value, col = name)) + 
        geom_line(size = 0.8) +
        geom_rect(
          mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
          data = exuber::datestamp(radf_price, mc_con)[["Aggregate - Dynamic Weights"]], 
          inherit.aes =FALSE, fill = "grey70", alpha = 0.55
        ) +
        scale_color_manual(values = c("black", "blue")) +
        my_theme +
        theme(
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.15, 0.85),
          axis.title.y = element_blank()
        ) +
        scale_custom(object = price, div = 7)
      
    })
  
  output$plot_income_aggregate <- 
    renderPlot({
      price_income %>% 
        select(Date, starts_with("Aggregate")) %>% 
        set_names(c("Date", "Fixed Weights (2005)", "Dynamic Weights")) %>% 
        pivot_longer(-Date) %>% 
        ggplot(aes(Date, value, col = name)) + 
        geom_line(size = 0.8) +
        geom_rect(
          mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
          data = exuber::datestamp(radf_income, mc_con)[["Aggregate - Dynamic Weights"]], 
          inherit.aes =FALSE, fill = "grey70", alpha = 0.55
        ) +
        scale_color_manual(values = c("black", "blue")) +
        my_theme +
        theme(
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.85, 0.85),
          axis.title.y = element_blank()
        ) +
        scale_custom(object = price_income, div = 7)
    })
  
  output$plot_growth_price_aggregate <- 
    renderPlot({
      
      growth_price <- price %>% 
        mutate_at(vars(-Date), growth_rate) %>% 
        tidyr::drop_na() 
      quantiles <- growth_price %>%
        select(-starts_with("Aggregate")) %>%
        pivot_longer(-Date) %>%
        group_by(Date) %>%
        summarise(
          q75 = quantile(value, 0.25), 
          q25 = quantile(value, 0.75),
          .groups = "drop")
      
      suppressWarnings({
        growth_price %>% 
          select(Date, starts_with("Aggregate")) %>% 
          set_names(c("Date", "Fixed Weights (2005)", "Dynamic Weights")) %>% 
          pivot_longer(-Date) %>% 
          full_join(quantiles, by = "Date") %>%
          ggplot(aes(Date, value, col = name)) + 
          
          geom_rect(
            mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
            data = exuber::datestamp(radf_price, mc_con)[["Aggregate - Dynamic Weights"]], 
            inherit.aes = FALSE, fill = "grey70", alpha = 0.55) +
          geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#174B97", alpha = 0.3, colour = NA) +
          ylab("% Quarter on Quarter") +
          scale_color_manual(values = c("black", "blue")) +
          geom_line(size = 0.8) + 
          my_theme +
          theme(
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.position = c(0.15, 0.85),
            axis.title.y = element_blank()
          ) +
          scale_custom(object = growth_price, div = 7)
      })
      
    })
  
  output$plot_growth_income_aggregate <- 
    renderPlot({
      growth_income <- price_income %>% 
        mutate_at(vars(-Date), growth_rate) %>% 
        tidyr::drop_na() 
      quantiles <- growth_income %>%
        select(-starts_with("Aggregate")) %>%
        pivot_longer(-Date) %>%
        group_by(Date) %>%
        summarise(
          q75 = quantile(value, 0.25), 
          q25 = quantile(value, 0.75),
          .groups = "drop")
      
      suppressWarnings({
        growth_income %>% 
          select(Date, starts_with("Aggregate")) %>% 
          set_names(c("Date", "Fixed Weights (2005)", "Dynamic Weights")) %>% 
          pivot_longer(-Date) %>% 
          full_join(quantiles, by = "Date") %>%
          ggplot(aes(Date, value, col = name)) + 
          
          geom_rect(
            mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
            data = exuber::datestamp(radf_income, mc_con)[["Aggregate - Dynamic Weights"]], 
            inherit.aes = FALSE, fill = "grey70", alpha = 0.55) +
          geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#174B97", alpha = 0.3, colour = NA) +
          ylab("% Quarter on Quarter") +
          scale_color_manual(values = c("black", "blue")) +
          geom_line(size = 0.8) + 
          my_theme +
          theme(
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.position = c(0.15, 0.85),
            axis.title.y = element_blank()
          ) +
          scale_custom(object = growth_income, div = 7)
      })
    })
  
  # * Analysis ----------------------------------------------------------------
  
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
          file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
          
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
    autoplot2(radf_price, cv = mc_con, select_series = input$country, nonrejected = TRUE) + 
      ggtitle("") + scale_custom(idx)
  })
  
  output$plot_income <- renderPlot({
    autoplot2(radf_income, cv = mc_con, select_series = input$country, nonrejected = TRUE) + 
      ggtitle("") + scale_custom(idx)
  })
  
  output$table1 <-
    DT::renderDataTable(server = FALSE, {
      DT_summary(
        summary(radf_price, mc_con) %>% 
          purrr::pluck(input$country) %>% 
          mutate(stat = toupper(stat)) %>% 
          set_names(c("", "tstat", "90%", "95%", "99%"))
      )
    })
  
  output$table2 <- 
    DT::renderDataTable(server = FALSE, {
      DT_summary(
        summary(radf_income, mc_con) %>% 
          purrr::pluck(input$country) %>% 
          mutate(stat = toupper(stat)) %>% 
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
  # autoplot_price_reactive <- 
  #   reactive({
  #     if (any(exuber::diagnostics(radf_price, mc_con)$positive %in% input$country)) {
  #       # make_autoplotly(radf_price, mc_con, ctry = input$country)
  #       autoplot(radf_price, mc_con, select_series = input$country) + 
  #         ggtitle("") + scale_custom(idx)
  #     }else{
  #       NULL_plot()
  #     }
  #   })
  
  output$autoplot_price <- 
    renderPlot({
      # autoplot_price_reactive()
      autoplot(radf_price, mc_con, select_series = input$country, nonrejected = TRUE) + 
        ggtitle("") + scale_custom(idx)
    })
  
  # autoplot_income
  # autoplot_income_reactive <- 
  #   reactive({
  #     if (any(exuber::diagnostics(radf_income, mc_con)$positive %in% input$country)) {
  #       autoplot(radf_income, mc_con, select_series = input$country) + 
  #         ggtitle("") + scale_custom(idx)
  #     }else{
  #       NULL_plot()
  #     }
  #   })
  
  output$autoplot_income <- 
    renderPlot({
      autoplot(radf_income, mc_con, select_series = input$country, nonrejected = TRUE) + 
        ggtitle("") + scale_custom(idx)
    })
  
  output$exuberance_note <- renderUI({
    
    if(any(exuber::diagnostics(radf_income, mc_con)$positive %in% input$country)) {
      p(
        em(
          span("There is exuberance when the"), 
          span("statistics of the second stage (BSADF)", style = "color:blue"), 
          span("exceeds the"),
          span("critical value sequence.",  style = "color:red;")
        ), 
        style = "text-align:center;font-size:14px;")
    }else{
      p(
        em(
          span("Despite the fact that there are periods that the"),
          span("statistics of the second stage (BSADF)", style = "color:blue"), 
          span("exceed the corresponding"),
          span("critical value sequence,",  style = "color:red;"),
          span("no exuberance is detected because the test statistic 
               of the first stage (GSADF) is not significant.")
        ), 
        style = "text-align:center;font-size:14px;")
    }
    
  })
  
  
  # table 3
  
  table3_reactive <- 
    reactive({
      if (any(exuber::diagnostics(radf_price, mc_con)$positive %in% input$country)) {
        exuber::datestamp(radf_price, mc_con) %>%
          purrr::pluck(input$country) %>%
          select(Start, Peak, End, Duration) %>% 
          to_yq(radf_price)
      }else{
        NULL
      }
    })
  
  output$table3 <- 
    DT::renderDataTable({
      table3_reactive()
    }, options = list(
      searching = FALSE,
      language = list(emptyTable = "The series does not exhibit any periods of exuberance"),
      ordering = FALSE,
      dom = "t"
    ))
  # table 4
  
  table4_reactive <- 
    reactive({
      if (any(exuber::diagnostics(radf_income, mc_con)$positive %in% input$country)) {
        exuber::datestamp(radf_income, mc_con) %>%
          purrr::pluck(input$country) %>%
          select(Start, Peak, End, Duration) %>% 
          to_yq(radf_income)
      }else{
        tibble::tibble()
      }
    })
  
  output$table4 <- 
    DT::renderDataTable({
      table4_reactive()
    }, options = list(
      searching = FALSE,
      language = list(emptyTable = "The series does not exhibit any periods of exuberance"),
      ordering = FALSE,
      dom = "t"
    ))
  
  
  # * Analysis-fundamentals -----------------------------------------------------
  
  output$psyivx_panel <- renderUI({
    
    if(input$psyivx_country != "All") {
      fluidRow(
        box2(
          title = "House Price to Rent",
          # popover = TRUE,
          # popover_title = "Note:",
          # popover_content = note_shade,
          width = 6,
          plotOutput("plot_exuber_fundamentals", height = 600)
        ),
        box2(
          title = "Datestamping Periods of Exuberance",
          width = 6,
          dataTableOutput("table_ds_psyivx")
        )
      )
    }else{
      fluidRow(
        box2(
          title = "House Price to Rent",
          # popover = TRUE,
          # popover_title = "Note:",
          # popover_content = note_shade,
          width = 8,
          plotOutput("plot_exuber_fundamentals", height = 1200)
        ),
        box2(
          title = "Datestamping Periods of Exuberance",
          width = 4,
          dataTableOutput("table_ds_psyivx")
          
        )
      )
    }
  })
  
  psyivx_ds_reactive <- reactive({
    if(input$go_psyivx) {
      ds <- filter(psyivx_ds, type == "psyivx")
    }else{
      ds <- filter(psyivx_ds, type == "ptr")
    }
    ds <- select(ds, -type, -Signal)
    if(input$psyivx_country != "All") {
      ds <- filter(ds, country == input$psyivx_country) %>% 
        select(-country) 
    }
    ds
  })
  
  output$plot_exuber_fundamentals <- renderPlot({
    if(input$psyivx_country != "All") {
      psyivx_data <- filter(psyivx_data, country == input$psyivx_country)
    }
    
    gg <- psyivx_data %>% 
      ggplot() +
      geom_line(aes(Date, price)) +
      theme_exuber() + 
      annotate("text", label = "Monitoring", y = -Inf, x = as.Date("2019-04-01"), 
               vjust = 1, hjust = 0, angle = 90) +
      geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed") +
      facet_wrap(~country, scales = "free") +
      scale_custom(idx, 7)
    
    if(input$go_psyivx) {
      if(input$psyivx_country %in% c("All", psyivx_countries_ds)) {
        gg <- gg +
          geom_rect(
            data = psyivx_ds_reactive(), fill = "grey55", alpha = 0.3,
            aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf))
      }
    }else{
      gg <- gg +
        geom_rect(
          data = psyivx_ds_reactive(), fill = "grey55", alpha = 0.3,
          aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf))
    }
    
    
    if(input$go_psyivx) {
      gg <- gg +
        geom_line(aes(Date, fundamental), col = "red")
    }
    if(input$psyivx_country == "All") {
      gg <- gg + 
        facet_wrap(~country, scales = "free") +
        scale_custom(idx, 4)
    }
    gg
  })
  
  output$table_ds_psyivx <- renderDT({
    datatable(
      psyivx_ds_reactive() %>% 
        mutate_at(vars(Start, Peak, End), ~ as.Date(.x) %>% zoo::as.yearqtr() %>% as.character()),
      rownames = FALSE,
      options = list(
        autoWidth = TRUE,
        searching = FALSE,
        ordering = FALSE,
        language = list(emptyTable = "The series does not exhibit any periods of exuberance"),
        pageLength = 40,
        dom = "t")
    )
  })
  
  
  
  # * Data --------------------------------------------------------------------
  
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
  
  #_ Raw ----
  
 
  output$data_price <- DT::renderDataTable(server = FALSE, {
      make_DT(trans_yqtr(price, input$go_yqtr), "price", citation_data)
  })
  
  output$data_income <- DT::renderDataTable(server = FALSE, {
    make_DT(trans_yqtr(price_income, input$go_yqtr), "income", citation_data)
  })
  
  #_ Estimation Statistics and Critical Values ----
  
  output$estimation_price <- DT::renderDataTable(server = FALSE, {
    make_DT(trans_yqtr(estimation_price, input$go_yqtr), "estimation-price", citation_estimation)
  })
  
  output$estimation_income <- DT::renderDataTable(server = FALSE, {
    make_DT(trans_yqtr(estimation_income, input$go_yqtr), "estimation-income", citation_estimation)
  })
  
  output$cv_seq <- DT::renderDataTable(server = FALSE, {
    make_DT_general(trans_yqtr(cv_seq, input$go_yqtr), "cv-sequence")
  })
  
  output$cv_table <- DT::renderDataTable(server = FALSE, {
    make_DT_general(cv_table, "cv-table")
  })
  
  #_ PSYVIX Statistics ----
  
  output$psyivx_data <- DT::renderDataTable(server = FALSE, {
    trans_yqtr(psyivx_data, input$go_yqtr) %>% 
      set_names(
        c("Country", "Date", "Price-to-Rent", "Bubble", "Fundamental", "Log Rent", "Long-term rate",
          "Exuberance Indicator Price-to-Rent", "Exuberance Indicator Bubble", "BSADF Price-to-Rent", "BSADF Bubble", "BSADF Critical Value"
          )
      ) %>% 
      datatable(
        escape = F,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list( 
          dom = 'Bfrtip', #'Blfrtip'
          autoWidth = TRUE,
          scrollY = TRUE,
          paging = TRUE,
          language = list(search = "Select Country:"),
          columnDefs = list(list(targets = c(1), width = "80px")),
          buttons = specify_buttons("psyivx-data")
        )
      )
  })
  
}

# Launch ------------------------------------------------------------------

# enableBookmarking("url")
shinyApp(ui = dashboardPage(skin = "black", title = "Dashboard | International Housing Observatory",  
                            header, sidebar, body), server)
