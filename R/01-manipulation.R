
library(tidyverse)

# Download file -----------------------------------------------------------

vers <- xml2::read_html("https://www.dallasfed.org/institute/houseprice#tab2") %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href") %>%
  stringr::str_subset(".xlsx") %>% 
  `[`(1) %>% 
  str_extract("(?<=houseprice/).+(?=.xlsx)")

dataURL <- glue::glue("https://www.dallasfed.org/~/media/documents/institute/houseprice/{vers}.xlsx")

temp <- glue::glue("data/{vers}.xlsx")

download.file(dataURL, destfile = temp, mode = 'wb')

rhpi <- readxl::read_excel(temp, sheet = 3)

rpdi <- readxl::read_excel(temp, sheet = 5)

if (file.exists(temp)) {
  file.remove(temp)
}
  

# Manipulation ------------------------------------------------------------


# Real House Price Index
price <-
  rhpi %>%
  slice(-1) %>% 
  rename(Date = 1) %>% 
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "%Y:Q%q") %>%
           zoo::as.Date()
  ) %>% 
  select( -25) %>% 
  na.omit()

# Real Personal Disposable Index
income <- 
  rpdi %>% 
  slice(-1) %>% 
  rename(Date = 1) %>% 
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "%Y:Q%q") %>%
           zoo::as.Date()
  ) %>% 
  select( -25) %>% 
  na.omit()

# Full Data
full_data <- full_join(
  price %>% 
    gather(countries, price, -Date),
  income %>% 
    gather(countries, income, -Date),
  by = c("Date", "countries")) %>% 
  mutate(price_income = price/income)

# Price-to-Income Ratios
price_income <- full_data %>% 
  select(Date, countries, price_income) %>% 
  spread(countries, -Date) %>% 
  select(-Aggregate, everything())


# Country Names Ordered
cnames <- price %>%
  select(-Date, -Aggregate) %>%
  names() %>%
  sort() %>% 
  c("Aggregate") #reposition Aggregate to be last
