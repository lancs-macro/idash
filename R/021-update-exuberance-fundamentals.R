
update_exuber_fundamentals <- function() {
  
  library(tidyverse)
  library(readxl)
  
  # stat pkgs ---------------------------------------------------------------
  
  library(ivx)
  library(exuber)
  
  # options -----------------------------------------------------------------
  
  options(transx.display = FALSE)
  datafile <- here::here("data-raw","psyivx.xlsx")
  
  # read all data -----------------------------------------------------------
  
  nms0 <- gsub("\\.\\.", "\\.", names(readxl::read_excel(datafile)))[-1]
  sheets <- excel_sheets(datafile)
  lsheets <- map(
    sheets, ~
      readxl::read_excel(datafile, sheet = .x, skip = 5, col_names = FALSE) %>%
      set_names(c("Date", nms0)) %>%
      mutate(Date = zoo::as.Date(zoo::as.yearqtr(Date, format = "Q%q/%Y")))
  )
  
  nms <-
    c("Australia", "Belgium", "Canada", "Denmark", "Finland", "France",
      "Germany", "Ireland", "Netherlands", "New.Zealand", "Norway", "Spain",
      "UK", "US")
  
  vars <- c("hpi", "rent", "ltrate", "credit", "ngdp", "pdi", "un", "resi", "rgdp", "cpi", "pop", "permits")
  hdata_raw <- map2(lsheets, vars, ~ pivot_longer(.x, -Date, values_to = .y)) %>%
    reduce(full_join, by = c("Date", "name"))
  
  hdata <- hdata_raw %>%
    filter(name %in% nms) %>% 
    group_by(name) %>%
    mutate(
      rhpi = hpi/cpi*100,
      pti = hpi/pdi*100,
      ptr = hpi/rent*100,
      rrent = rent/cpi*100,
      rpdi = pdi/cpi*100,
      ltrate = ltrate/100,
      un = un/100
    ) %>%
    mutate(
      grhpi = transx::ldiffx(rhpi),
      gpti = transx::ldiffx(pti),
      gptr = transx::ldiffx(ptr),
    ) %>%
    mutate(
      log_hpi = log(hpi),
      log_rent = log(rent),
      log_pdi = log(pdi),
      log_credit = log(credit),
      log_ngdp = log(ngdp),
      log_resi = log(resi),
      log_cpi = log(cpi),
      log_pop = log(pop),
      log_permits = log(permits)
    ) %>%
    mutate(
      log_rhpi  = log(rhpi),
      log_rrent = log(rrent),
      log_rpdi = log(rpdi),
      log_pti = log(pti),
      log_ptr = log(ptr)
    )
  
  tbl_data <- group_split(drop_na(hdata))
  names(tbl_data) <- nms
  map(tbl_data, nrow)


  # helpers -----------------------------------------------------------------
  
  ds_fun <- function(x) datestamp(x, min_duration = 2, nonrejected = T)[[1]]
  safe_ds_fun <- safely(ds_fun)
  
  shade_aplt <- function(ds_data, fill = "grey75", fill_negative =  "#ffffff00",
                         opacity = 0.5) {
    
    ds_pos <- filter(ds_data, Signal == "positive")
    ds_neg <- filter(ds_data, Signal == "negative")
    
    
    any_pos <- nrow(ds_pos) > 0
    x1 <- ds_pos %>% 
      geom_rect(
        data = ., inherit.aes = FALSE, fill = fill, alpha = opacity, 
        aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf))
    
    any_neg <- nrow(ds_neg) > 0
    x2 <- ds_neg %>% 
      geom_rect(
        data = ., inherit.aes = FALSE, fill = fill_negative, alpha = opacity, 
        aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf))
    
    list(any_pos %NULL% x1, any_neg %NULL% x2)
  }
  
  `%NULL%` <- function(cond, x) {
    if(isTRUE(cond)) {
      x
    } else{
      NULL
    }
  }
  
  # price-to-rent -----------------------------------------------------------
  
  radf_ptr <- map(tbl_data, ~ select(.x, Date, log_ptr) %>% radf(lag = 1))
    
  ds_ptr <-  map(radf_ptr, safe_ds_fun) %>% 
    map("result") %>% 
    bind_rows(.id = "name") %>% 
    select(Coucntry = name, Start, Peak, End, Duration)
  
  plt_ptr <- hdata %>% 
    ggplot(aes(Date, log_ptr)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y", ncol = 3) +
    shade_aplt(ds_ptr) +
    theme_exuber()
  
  saveRDS(ds_ptr, "data/RDS/ds_exuber_ptr.rds")
  saveRDS(plt_ptr, "data/RDS/plot_exuber_ptr.rds")
  
  # estimation --------------------------------------------------------------
  
  residuals_ivx <- function(formula, data) {
    res <- cumsum(residuals(ivx(formula, data = data)))
    tibble(Date = data$Date[-1], value = res)
  }
  
  radf_ivx <- map(tbl_data, ~ residuals_ivx(gptr ~ ltrate + log_rent, data = .x)) %>% 
    map(radf, lag = 1)
  
  ds_ivx <-  map(radf_ivx, safe_ds_fun) %>% 
    map("result") %>% 
    bind_rows(.id = "name") %>% 
    select(Coucntry = name, Start, Peak, End, Duration)
  
  plt_ivx <- hdata %>% 
    ggplot(aes(Date, log_ptr)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y", ncol = 3) +
    shade_aplt(ds_ivx) +
    theme_exuber()
  
  saveRDS(ds_ivx, "data/RDS/ds_exuber_ivx.rds")
  saveRDS(plt_ivx, "data/RDS/plot_exuber_ivx.rds")
}
