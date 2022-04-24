
library(rlang)
library(ggplot2)

NULL_plot <- function(n = 1, .size  = 5) {
  text <- "The series does not exhibit exuberant behavior"
  np <- list(length = n)
  for (i in 1:n) {
    np[[i]] <- ggplot() + 
      annotate("text", x = 4, y = 25, size = .size, label = text) +
      theme_void()
  }
  if (n > 1) np else np[[1]] 
}

# Custom Labels  ----------------------------------------------------------


extract_yq <- function(object) {
  yq <- object %>% 
    select_if(lubridate::is.Date) %>% 
    setNames("Date") %>% 
    mutate(Quarter = lubridate::quarter(Date),
           Year = lubridate::year(Date)) %>% 
    tidyr::unite(labels, c("Year", "Quarter"), sep = " Q") %>% 
    rename(breaks = Date)
  yq
}

custom_date <- function(object, variable, div) {
  yq <- extract_yq(object)
  seq_slice <- seq(1, NROW(yq), length.out = div)
  yq %>% 
    slice(as.integer(seq_slice)) %>% 
    pull(!!parse_expr(variable))
}

scale_custom <- function(object, div = 7) {
  require(lubridate)
  scale_x_date(
    breaks = custom_date(object, variable = "breaks", div = div),
    labels = custom_date(object, variable = "labels", div = div)
  )
}


# Plot Normal Series ------------------------------------------------------

my_theme <- theme_light() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank() ,
    panel.grid.major = element_line(linetype = "dashed")
  )


plot_var <- function(.data, .var, custom_labels = TRUE, rect = FALSE, 
                     rect_data = NULL, div = 7) {
  g <- .data %>% 
    # mutate(last_obs = ifelse(row_number() > nrow(.) - 1, TRUE, FALSE)) %>% 
    ggplot(aes_string("Date", as.name(.var))) +
    geom_line(size = 0.8) + 
    my_theme +
    theme(axis.title.y = element_blank())
  
  if (rect) {
    g <- g +  geom_rect(
      mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
      data = rect_data, inherit.aes =FALSE,
      fill = "grey70", alpha = 0.55
    )
  }
  
  if(custom_labels){
    g <- g + scale_custom(object = .data, div = div)
  }
  g
}

growth_rate <- function(x, n  = 1) (log(x) - dplyr::lag(log(x), n = n))*100

plot_growth_var <- function(.data, .var, rect_data) {
  
  .data <- .data %>% 
    mutate_at(vars(-Date), growth_rate) %>% 
    tidyr::drop_na()
  
  q75 <- apply(.data[,-1], 1, quantile, 0.25)
  q25 <- apply(.data[,-1], 1, quantile, 0.75)
  suppressWarnings({
    .data %>% 
      ggplot(aes_string("Date", as.name(.var))) +
      geom_rect(
        mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
        data = rect_data, inherit.aes=FALSE, fill = "grey70", alpha = 0.55)+
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#174B97", alpha = 0.5) +
      geom_line(size = 0.8) + 
      ylab("% Quarter on Quarter") +
      my_theme +
      scale_custom(object = .data)
  })
}

# Autoplot radf objects ---------------------------------------------------


analysis_theme <- theme_light() +
  theme(
    title = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank() ,
    panel.grid.major = element_line(linetype = "dashed")
  )

autoplot_var <- function(radf_var, cv_var, input, custom_labels = TRUE) {
  g <- exuber::autoplot(radf_var, cv = cv_var, include = TRUE, select = input) + 
    analysis_theme
  
  g$layers[[1]]$aes_params$size <- 0.8
  
  if(custom_labels){
    g <- g + scale_custom(object = fortify(radf_var, cv = cv_var))
  }
  g
}

ds_yq <- function(ds, index_yq) {
  start <- ds[, 1]
  start_ind <- which(index_yq$breaks %in% start)
  start_label <- index_yq[start_ind ,2]
  
  peak <- ds[, 2]
  peak_ind <- which(index_yq$breaks %in% start)
  peak_label <- index_yq[start_ind ,2]
  
  end <- ds[, 3]
  end_ind <- which(index_yq$breaks %in% end)
  if (anyNA(end)) end_ind <- c(end_ind, NA)
  end_label <- index_yq[end_ind ,2]
  
  ds[, 1] <- start_label 
  ds[, 2] <- peak_label
  ds[, 3] <- end_label
  ds
}

# Datestamp into yq
to_yq <- function(ds, radf_var){
  idx <- tibble(Date = index(radf_var, trunc = FALSE))
  index_yq <- extract_yq(idx)
  ds_yq(ds, index_yq)
}



# download html -----------------------------------------------------------


trans_yqtr <- function(x, lgl) {
  if(lgl) {
    x <- mutate(x, Date = as.character(zoo::as.yearqtr(Date)))
  }else{
    x
  }
}

DT_preview <- function(x, title = NULL) {
  box2(
    width = 12, 
    title = title, 
    dataTableOutput(x)
  )
}

tab_panel <- function(x, title, prefix = "") {
  tabPanel(
    title, 
    icon = icon("angle-double-right"), 
    DT_preview(
      x, 
      title = paste0(prefix, title)
    )
  )
}

# datatable ---------------------------------------------------------------

specify_buttons <- function(filename) {
  list(
    list(
      extend = "collection",
      buttons =
        list(
          list(extend = 'csv',
               filename = filename
               , exportOptions  =
                 list(
                   modifier = 
                     list(
                       page = "all",
                       search = 'none')
                 )
          ),
          list(extend = 'excel',
               filename = filename,
               title = "International Housing Observatory")
        ),
      text = "Download"
    )
  )
}

DT_summary <- function(x) {
  DT::datatable(
    x,
    rownames = FALSE,
    options = list( 
      dom = "t",
      searching = FALSE,
      ordering = FALSE
    )
  ) %>% 
    DT::formatRound(2:NCOL(x), 3) 
}




make_DT <- function(x, filename, caption_string = ""){
  DT::datatable(
    x,
    escape = F,
    rownames = FALSE,
    caption = caption_string,
    extensions = 'Buttons',
    options = list( 
      dom = 'Bfrtip', #'Blfrtip'
      searching = FALSE,
      autoWidth = TRUE,
      paging = TRUE,
      # scrollY = T,
      scrollX = T,
      columnDefs = list(
        list(
          targets = c(0, 14, 18, 21), width = "80px")),
      buttons = specify_buttons(filename)
    )
  ) %>%
    DT::formatRound(2:NCOL(x), 3) 
}


make_DT_general <- function(x, filename) {
  
  DT::datatable(x,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',#'Blfrtip',
                               searching = FALSE,
                               autoWidth = TRUE,
                               paging = TRUE,
                               scrollX = F,
                               # columnDefs = list(list(targets = c(0), width = "80px")),
                               buttons = specify_buttons(filename)
                )
  ) %>%
    DT::formatRound(2:NCOL(x), 3) 
}


# html --------------------------------------------------------------------

box2 <- function(..., title = NULL, subtitle = NULL, footer = NULL, status = NULL, 
                 solidHeader = FALSE, background = NULL, width = 6, height = NULL, 
                 popover = FALSE, popover_title = NULL, popover_content = NULL,
                 data_toggle = "popover", collapsible = FALSE, collapsed = FALSE) 
{
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    shinydashboard:::validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    shinydashboard:::validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }
  subtitleTag <- NULL
  if (!is.null(title)) {
    subtitleTag <- h5(class = "box-subtitle", subtitle)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- div(class = "box-tools pull-right", 
                       tags$button(class = paste0("btn btn-box-tool"), 
                                   `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  popoverTag <- NULL
  if (popover) {
    popoverTag <- div(
      class = "box-tools pull-right", 
      tags$button(
        class = paste0("btn btn-box-tool"), 
        `title` = popover_title,
        `data-content` = popover_content,
        `data-trigger` = "focus",
        `data-placement` = "right",
        # `data-html` = "true",
        `data-toggle` = data_toggle, shiny::icon("info"))
    )
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag) || !is.null(popoverTag)) {
    headerTag <- div(class = "box-header", titleTag, subtitleTag, collapseTag, popoverTag)
  }
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
        div(class = "box-footer", footer)))
}

note_exuber <- 
  HTML('<span>There is exuberance when the </span> <span class="textbf"> solid line </span> <span> surpasses the </span><span class="color-red"> dashed line </span>.')

note_ds <- 
  HTML('Periods of time identified as exuberant by the financial stability analysis.')

note_shade <- 
  HTML('<span class="color-grey">Shaded areas</span> <span>indicate identified periods of exuberance.</span>')

note_bands <- 
  HTML('<span>The </span> <span class="color-grey">shaded bands </span><span> refer to the difference between the top and bottom decile of growth rates across all regions in the UK.</span>')


column_4 <- function(...) {
  column(width = 4, ...)
}


# switchbutton ------------------------------------------------------------


# Customised TRUE-FALSE switch button for Rshiny
# Only sing CSS3 code (No javascript)
#
# Sébastien Rochette
# http://statnmap.com/en/
# April 2016
#
# CSS3 code was found on https://proto.io/freebies/onoff/
# For CSS3 customisation, refer to this website.

#' A function to change the Original checkbox of rshiny
#' into a nice true/false or on/off switch button
#' No javascript involved. Only CSS code.
#' 
#' To be used with CSS script 'button.css' stored in a 'www' folder in your Shiny app folder
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value (TRUE or FALSE).
#' @param col Color set of the switch button. Choose between "GB" (Grey-Blue) and "RG" (Red-Green)
#' @param type Text type of the button. Choose between "TF" (TRUE - FALSE), "OO" (ON - OFF) or leave empty for no text.

switchButton <- function(inputId, label, value=FALSE, col = "GB", type="TF") {
  
  # color class
  if (col != "RG" & col != "GB") {
    stop("Please choose a color between \"RG\" (Red-Green) 
      and \"GB\" (Grey-Blue).")
  }
  if (!type %in% c("OO", "TF", "YN")){
    warning("No known text type (\"OO\", \"TF\" or \"YN\") have been specified, 
     button will be empty of text") 
  }
  if(col == "RG"){colclass <- "RedGreen"}
  if(col == "GB"){colclass <- "GreyBlue"}
  if(type == "OO"){colclass <- paste(colclass,"OnOff")}
  if(type == "TF"){colclass <- paste(colclass,"TrueFalse")}
  if(type == "YN"){colclass <- paste(colclass,"YesNo")}
  
  # No javascript button - total CSS3
  # As there is no javascript, the "checked" value implies to
  # duplicate code for giving the possibility to choose default value
  
  if(value){
    tagList(
      tags$div(
        class = "form-group shiny-input-container",
        tags$div(
          class = colclass,
          tags$label(
            label, 
            class = "control-label"
          ),
          tags$div(
            class = "onoffswitch",
            tags$input(
              type = "checkbox", 
              name = "onoffswitch", 
              class = "onoffswitch-checkbox",
              id = inputId, 
              checked = ""
            ),
            tags$label(
              class = "onoffswitch-label", 
              `for` = inputId,
              tags$span(class = "onoffswitch-inner"),
              tags$span(class = "onoffswitch-switch")
            )
          )
        )
      )
    )
  } else {
    tagList(
      tags$div(
        class = "form-group shiny-input-container",
        tags$div(
          class = colclass,
          tags$label(
            label, class = "control-label"),
          tags$div(
            class = "onoffswitch",
            tags$input(
              type = "checkbox", 
              name = "onoffswitch", 
              class = "onoffswitch-checkbox",
              id = inputId
            ),
            tags$label(
              class = "onoffswitch-label", 
              `for` = inputId,
              tags$span(class = "onoffswitch-inner"),
              tags$span(class = "onoffswitch-switch")
            )
          )
        )
      )
    ) 
  }
}
