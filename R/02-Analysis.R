
source(here::here("R", "01-manipulation.R"))
source(here::here("R", "00-functions-src.R")

library(exuber)

# Estimation and critical values

radf_price <- 
  price %>% 
  radf(lag = 1)

radf_income <- 
  price_income %>% 
  radf(lag = 1)

mc_con <- 
  mc_cv(NROW(price), opt_bsadf = "conservative")

# Summary -----------------------------------------------------------------

summary_price <- 
  radf_price %>% 
  summary(cv = mc_con)

summary_income <- 
  radf_income %>% 
  summary(cv = mc_con)

# we will need diagnostics to rename the plot output

rejected_price <- 
  radf_price %>% 
  diagnostics(cv = mc_con) %>% 
  pluck("rejected")

rejected_income <- 
  radf_income %>% 
  diagnostics(cv = mc_con) %>% 
  pluck("rejected")

datestamp_price <-
  radf_price %>% 
  datestamp(cv = mc_con)

datestamp_income <- 
  radf_income %>% 
  datestamp(cv = mc_con) 

# autoplot ----------------------------------------------------------------

autoplot_price <- 
  radf_price %>% 
  autoplot(cv = mc_con, include = TRUE) %>% 
  map( ~.x + ggtitle("") +
         scale_custom(object = fortify(radf_price))
  )

# NULL plot when they do not pass the test: bypass include
autoplot_price[rejected_price] <- NULL_plot(length(rejected_price)) 

autoplot_income <- 
  radf_income %>% 
  autoplot(cv = mc_con, include = TRUE) %>% 
  map( ~.x + ggtitle("") +
         scale_custom(object = fortify(radf_price))
  )

autoplot_income[rejected_income] <- NULL_plot(length(rejected_income))

# autoplot datestamp ------------------------------------------------------

autoplot_datestamp_price <-
  datestamp_price %>%  
  autoplot() +
  scale_custom(fortify(radf_price)) +
  scale_color_viridis_d()

autoplot_datestamp_income <- 
  datestamp_income %>% 
  autoplot() + 
  scale_custom(fortify(radf_price))  +
  scale_color_viridis_d()

# Overwrite datestamp --------------------------------------------------------

index_yq <- extract_yq(fortify(radf_price)) # Remake into yq

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

datestamp_price <-
  radf_price %>% 
  datestamp(cv = mc_con) %>% 
  map(ds_yq)

datestamp_income <- 
  radf_income %>% 
  datestamp(cv = mc_con) %>% 
  map(ds_yq)


# Plot -------------------------------------------------------------------

# House Prices plots
plot_price <- list()
for (i in seq_along(cnames)) {
  plot_price[[i]] <- ggplot(aes_string("Date", as.name(cnames[i])), 
                         data = price) +
    geom_line() + ylab("") + xlab("") +
    theme_light()
}
names(plot_price) <- cnames

plot_price <- 
  plot_price %>% 
  map( ~.x + ggtitle("") +
         scale_custom(object = price)
  )

# Personal Income plots
plot_income <- list()
for (i in seq_along(cnames)) {
  plot_income[[i]] <- ggplot(aes_string("Date", as.name(cnames[i])), data = price_income) +
    geom_line() + ylab("") + xlab("") +
    theme_light()
}
names(plot_income) <- cnames

plot_income <- 
  plot_income %>% 
  map( ~.x + ggtitle("") +
         scale_custom(object = fortify(radf_price))
  )



# data export -------------------------------------------------------------

estimation_price <- 
  radf_price %>%
  pluck("bsadf") %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())
  
estimation_income <- 
  radf_income %>%
  pluck("bsadf") %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

cv_seq <- mc_con %>% 
  pluck("bsadf_cv") %>% 
  as_tibble() %>% 
  "["(-1,) %>% 
  bind_cols(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())

cv_table <- 
  tibble(
    Countries = names(price)[-1],
    gsadf_rhpi = radf_price$gsadf,
    gsadf_hpi_dpi = radf_income$gsadf,
    gsadf_cv90 = mc_con$gsadf_cv[1],
    gsadf_cv95 = mc_con$gsadf_cv[2],
    gsadf_cv99 = mc_con$gsadf_cv[3]
  )

# save data ---------------------------------------------------------------
items <- c("price", "income")
store <- c("price", "price_income",
           c("cnames"), 
           "mc_con", "cv_seq", "cv_table",
           glue::glue("estimation_{items}"),
           glue::glue("radf_{items}"))

path_store <- glue::glue("data/RDS/{store}.rds")

for (i in seq_along(store)) saveRDS(get(store[i]), file = path_store[i])




