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
library(dplyr)
library(broom)

coho <- readxl::read_xlsx(here('data', 'OC Coho Abundance.xlsx')) %>%
  clean_names() %>% 
  distinct()

coho_yr <- coho %>%
  mutate(date = lubridate::mdy(year)) #Convert to date for tibble, this didn't work tho so we need to revisit this.

coho_ts <- coho_yr %>%
  as_tsibble(key = NULL, index = year) 

pop_mean <- sapply(coho[2:22], mean)
pop_var <- sapply(coho[2:22], var)
pop_sd <- sapply(coho[2:22], sd)

pop_df <- data.frame(pop_mean, pop_var, pop_sd)

pop_cov <- cov(coho[2:22])

# population_cov

## Only show bottom triangle of covariances
upper<-pop_cov
upper[upper.tri(pop_cov)] <-""
upper<-as.data.frame(upper)
upper