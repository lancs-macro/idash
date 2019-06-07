library(rlang, warn.conflicts = FALSE)

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
  ggplot(aes_string("Date", as.name(.var)), data = .data) +
    geom_line() + ylab("") + xlab("") +
    theme_light() + ggtitle("") +
    scale_custom(object = .data)
}
  


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


make_DT <- function(x, filename, caption_string = ""){
  DT::datatable(x,
                rownames = FALSE,
                caption = caption_string,
                extensions = 'Buttons',
                options = list( dom = 'Bfrtip',#'Blfrtip',
                                searching = FALSE,
                                autoWidth = TRUE,
                                paging = TRUE,
                                pageLength = 6,#NROW(x),
                                # scrollY = T,
                                scrollX = T,
                                columnDefs = list(
                                  list(
                                    targets = c(0, 14, 18, 21), width = "80px")),
                                buttons = list(
                                  list(
                                    extend = "collection",
                                    buttons = list(list(extend = 'csv',
                                                        filename = filename),
                                                   list(extend = 'excel',
                                                        filename = filename,
                                                        title = "International Housing Observatory")),
                                    text = "Download"
                                  )
                                )
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
                               pageLength = 6,
                               # pageLength = NROW(x),
                               scrollX = F,
                               # columnDefs = list(list(targets = c(0), width = "80px")),
                               buttons = list(
                                 list(
                                   extend = "collection",
                                   buttons = list(list(extend = 'csv',
                                                       filename = filename),
                                                  list(extend = 'excel',
                                                       filename = filename,
                                                       title = "International Housing Observatory")),
                                   text = "Download"
                                 )
                               )
                )
  ) %>%
    DT::formatRound(2:NCOL(x), 3) 
}