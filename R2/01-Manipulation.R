
# Manipulation ------------------------------------------------------------

library(tidyverse)

full_data <- ihpdr::ihpd_get()

cnames <- pull(full_data, country) %>% 
  unique() %>% 
  sort(.)
cnames <- cnames[c(3:length(cnames),1,2)]

price <- select(full_data, Date, country, rhpi) %>% 
  pivot_wider(names_from = country, values_from = rhpi)
income <- select(full_data, Date, country, rpdi) %>% 
  pivot_wider(names_from = country, values_from = rpdi)
price_income <- full_data %>% 
  group_by(country) %>% 
  mutate(price_income = rhpi/rpdi) %>% 
  select(Date, country, price_income) %>% 
  pivot_wider(names_from = country, values_from = price_income)
