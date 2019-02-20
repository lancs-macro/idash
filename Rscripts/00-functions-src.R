
make_DT <- function(x, filename, caption_string = ""){
  DT::datatable(x,
                rownames = FALSE,
                caption = caption_string,
                extensions = 'Buttons',
                options = list( dom = 'Bfrtip',#'Blfrtip',
                                searching = FALSE,
                                autoWidth = TRUE,
                                paging = FALSE,
                                pageLength = 20,#NROW(x),
                                # scrollY = T,
                                scrollX = T,
                                columnDefs = list(
                                  list(
                                    targets = c(0), width = "80px")),
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
    formatRound(2:NCOL(x), 3) 
}

make_DT_general <- function(x, filename) {
  DT::datatable(x,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',#'Blfrtip',
                               searching = FALSE,
                               autoWidth = TRUE,
                               paging = FALSE,
                               pageLength = NROW(x),
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
    formatRound(2:NCOL(x), 3) 
}