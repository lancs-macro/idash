
# Download file -----------------------------------------------------------

library(readxl)
library(glue)

version <- "hp1803"

dataURL <- glue("https://www.dallasfed.org/~/media/documents/institute/houseprice/{version}.xlsx")

temp <- glue("data/{version}.xlsx")

download.file(dataURL, destfile = temp, mode = 'wb')

rhpi <- readxl::read_excel(temp, sheet = 3)

rpdi <- readxl::read_excel(temp, sheet = 5)


# Manipulation ------------------------------------------------------------

library(dplyr)
library(lubridate)


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


price_income <- full_data %>% 
  select(Date, countries, price_income) %>% 
  spread(countries, -Date) %>% 
  select(-Aggregate, everything())

# pi_frac <- ((price %>% select(-Date)) / (income %>% select(-Date))) %>% 
#   as_tibble()
# 
# price_income <- bind_cols(Date = price$Date, pi_frac)

# Country Names Ordered
cnames <- price %>%
  select(-Date, -Aggregate) %>%
  names() %>%
  sort() %>%
  c("Aggregate") #reposition Aggregate to be last
