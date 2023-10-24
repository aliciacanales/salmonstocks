library(tidyverse) 
library(here)
library(janitor)
library(readxl)
library(tsibble)
library(lubridate)
library(feasts)
library(patchwork)
library(kableExtra)
library(purrr)
library(quadprog)

coho <- readxl::read_xlsx(here('data', 'OC Coho Abundance.xlsx')) %>%
  clean_names() %>% 
  distinct()

coho_yr <- coho %>%
  mutate(date = lubridate::mdy(year)) #Convert to date for tibble, this didn't work tho so we need to revisit this.

coho_ts <- coho_yr %>%
  as_tsibble(key = NULL, index = year) 