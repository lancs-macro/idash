
NULL_plot <- function(n = 1) {
  text <- "The series does not exhibit exuberant behavior"
  np <- list(length = n)
  for (i in 1:n) {
    np[[i]] <- ggplot() + 
      annotate("text", x = 4, y = 25, size = 5, label = text) +
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
}

scale_custom <- function(object, div = 10) {
  require(lubridate)
  custom_date <- function(object, variable, div) {
    
    yq <- extract_yq(object)
    seq_slice <- seq(1, NROW(yq), length.out = div)
    yq %>% 
      slice(seq_slice) %>% 
      pull(!!parse_expr(variable))
  }
  
  scale_x_date(
    breaks = custom_date(fortify(object), variable = "breaks", div = div),
    labels = custom_date(fortify(object), variable = "labels", div = div)
  )
}


# Plot Normal Series ------------------------------------------------------


plot_var <- function(.data, .var) {
  
    .data %>% 
    # mutate(last_obs = ifelse(row_number() > nrow(.) - 1, TRUE, FALSE)) %>% 
    ggplot(aes_string("Date", as.name(.var))) +
    geom_line() + ylab("") + xlab("") +
    # geom_point(aes_string(col = last_obs)) +
    theme_light() + ggtitle("") +
    scale_custom(object = .data)
    
}
  
# my_gg <- price %>% 
#   mutate(last_obs = ifelse(row_number() > nrow(.) - 1, TRUE, FALSE)) %>% 
#   mutate_if(is.numeric, round, 2) %>% 
#   ggplot(aes(Date, Australia)) +
#   geom_line() + ylab("") + xlab("") +
#   # geom_point_interactive(aes(tooltip = Australia  ,col = last_obs)) +
#   scale_color_manual(values = c("black", "red")) +
#   theme_light() + ggtitle("")
  
# library(ggiraph)
# g <- girafe(code = print(my_gg))

# Autoplot radf objects ---------------------------------------------------

autoplot_var <- function(radf_var, cv_var, input) {
  exuber::autoplot(radf_var, cv = cv_var, include = TRUE, select = input) + 
    ggtitle("") +
    scale_custom(object = fortify(radf_var, cv = cv_var))
}

# Datestamp into yq

to_yq <- function(ds, radf_var, cv_var){
  
  index_yq <- extract_yq(fortify(radf_var, cv =  cv_var))
  
  ds_yq <- function(ds) {
    start <- ds[, 1]
    start_ind <- which(index_yq$breaks %in% start)
    start_label <- index_yq[start_ind ,2]
    
    end <- ds[, 2]
    end_ind <- which(index_yq$breaks %in% end)
    if (anyNA(end)) end_ind <- c(end_ind, NA)
    end_label <- index_yq[end_ind ,2]
    
    ds[, 1] <- start_label 
    ds[, 2] <- end_label
    ds
  }
  
  ds %>% 
    ds_yq()
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
  


make_DT <- function(x, filename, caption_string = ""){
  DT::datatable(x,
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

box2 <- function (..., title = NULL, subtitle = NULL, footer = NULL, status = NULL, 
                  solidHeader = FALSE, background = NULL, width = 6, height = NULL, 
                  collapsible = FALSE, collapsed = FALSE) 
{
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    validateColor(background)
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
    subtitleTag <- h4(class = "box-subtitle", subtitle)
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
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, subtitleTag, collapseTag)
  }
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
        div(class = "box-footer", footer)))
}

