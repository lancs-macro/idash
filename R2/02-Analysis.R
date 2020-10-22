library(here)
library(tidyverse)

# source(here::here("R", "01-Manipulation.R"))
# source(here::here("R", "00-functions-src.R"))

library(exuber)

# Estimation and critical values

radf_price <- 
  price %>% 
  radf(lag = 1)

radf_income <- 
  price_income %>% 
  radf(lag = 1)

mc_con <- radf_crit[[NROW(price)]]

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
  .$rejected

rejected_income <- 
  radf_income %>% 
  diagnostics(cv = mc_con) %>% 
  .$rejected

datestamp_price <-
  radf_price %>% 
  datestamp(cv = mc_con) 

datestamp_income <- 
  radf_income %>% 
  datestamp(cv = mc_con) 


# Theem -------------------------------------------------------------------

analysis_theme <- theme_light() +
  theme(
    title = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank() ,
    panel.grid.major = element_line(linetype = "dashed")
  )

# autoplot ----------------------------------------------------------------

# Currently not used

# autoplot_price <- 
#   radf_price %>% 
#   autoplot(cv = mc_con, include = TRUE, arrange = FALSE) %>% 
#   map( ~.x + analysis_theme +
#          scale_custom(object = fortify(radf_price))
#   )
# 
# # NULL plot when they do not pass the test: bypass include
# autoplot_price[rejected_price] <- NULL_plot(length(rejected_price)) 
# 
# autoplot_income <- 
#   radf_income %>% 
#   autoplot(cv = mc_con, include = TRUE, arrange = FALSE) %>% 
#   map( ~.x + analysis_theme +
#          scale_custom(object = fortify(radf_price))
#   )
# 
# autoplot_income[rejected_income] <- NULL_plot(length(rejected_income))
# 
# for (i in seq_along(autoplot_price)) {
#   autoplot_price[[i]]$layers[[1]]$aes_params$size <- 0.8
# }
# 
# for (i in seq_along(autoplot_income)) {
#   autoplot_income[[i]]$layers[[1]]$aes_params$size <- 0.8
# }

# autoplot datestamp ------------------------------------------------------

idx <- tibble(Date = index(radf_price, trunc = FALSE))

autoplot_datestamp_price <-
  datestamp_price %>%  
  autoplot() +
  scale_custom(idx) 

autoplot_datestamp_income <- 
  datestamp_income %>% 
  autoplot() + 
  scale_custom(idx) 


# Plot -------------------------------------------------------------------

# House Prices plots
plot_price <- list()
for (i in seq_along(cnames)) {
  plot_price[[i]] <- ggplot(aes_string("Date", as.name(cnames[i])), data = price) +
    geom_line() + analysis_theme
}
names(plot_price) <- cnames

plot_price <- 
  plot_price %>% 
  map( ~.x + analysis_theme +
         scale_custom(object = price)
  )

# Personal Income plots
plot_income <- list()
for (i in seq_along(cnames)) {
  plot_income[[i]] <- ggplot(aes_string("Date", as.name(cnames[i])), data = price_income) +
    geom_line() + analysis_theme
}
names(plot_income) <- cnames

plot_income <- 
  plot_income %>% 
  map( ~.x + analysis_theme + scale_custom(object = idx))

# data export -------------------------------------------------------------

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

# save data ---------------------------------------------------------------

items <- c("price", "income")
store <- c("price", "price_income",
           c("cnames"), 
           "mc_con", "cv_seq", "cv_table",
           glue::glue("estimation_{items}"),
           # glue::glue("autoplot_{items}"),
           glue::glue("autoplot_datestamp_{items}"),
           glue::glue("radf_{items}"))

path_store <- glue::glue("data/RDS/{store}.rds")

for (i in seq_along(store)) {
  saveRDS(get(store[i]), file = path_store[i])
}

