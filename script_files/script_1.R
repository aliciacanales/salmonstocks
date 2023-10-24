coho <- readxl::read_xlsx(here('data', 'OC Coho Abundance.xlsx')) %>%
  clean_names() %>% 
  distinct()

coho_yr <- coho %>%
  mutate(date = lubridate::mdy(year)) #Convert to date for tibble, this didn't work tho so we need to revisit this.

coho_ts <- coho_yr %>%
  as_tsibble(key = NULL, index = year) 