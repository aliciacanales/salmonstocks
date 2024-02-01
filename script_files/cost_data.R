##.....................Isolating cost data...............

alsea <- read_csv(here('data', 'final_table_alsea.csv')) %>% 
  clean_names() %>% 
  select(cost)


beaver <- read_csv(here('data', 'final_table_beaver.csv')) %>% 
  clean_names() %>% 
  select(cost)


coos <- read_csv(here('data', 'final_table_coos.csv')) %>% 
  clean_names() %>% 
  select(cost)

coquille <- read_csv(here('data', 'final_table_coquille.csv')) %>% 
  clean_names() %>% 
  select(cost) %>% 
  mutate(cost = ifelse(cost == 0.00, 190097.2, cost))


floras <- read_csv(here('data', 'final_table_floras.csv')) %>% 
  clean_names() %>% 
  select(cost) 

lower_umpqua <- read_csv(here('data', 'final_table_lowerump.csv')) %>% 
  clean_names() %>% 
  select(cost) 

middle_umpqua <- read_csv(here('data', 'final_table_midump.csv')) %>% 
  clean_names() %>% 
  select(cost)

necanicum <- read_csv(here('data', 'final_table_necanicum.csv')) %>% 
  clean_names() %>% 
  select(cost) %>% 
  mutate(cost = ifelse(cost == 0.00, 190097.2, cost))

nehalem <- read_csv(here('data', 'final_table_nehalem.csv')) %>% 
  clean_names() %>% 
  select(cost)

nestucca <- read_csv(here('data', 'final_table_nestucca.csv')) %>% 
  clean_names() %>% 
  select(cost)

north_umpqua <- read_csv(here('data', 'final_table_northump.csv')) %>% 
  clean_names() %>% 
  select(cost)

salmon <- read_csv(here('data', 'final_table_salmon.csv')) %>% 
  clean_names() %>% 
  select(cost)

siletz <- read_csv(here('data', 'final_table_siletz.csv')) %>% 
  clean_names() %>% 
  select(cost)

siltcoos <- read_csv(here('data', 'final_table_siltcoos.csv')) %>% 
  clean_names() %>% 
  select(cost)

siuslaw <- read_csv(here('data', 'final_table_siuslaw.csv')) %>% 
  clean_names() %>% 
  select(cost)

sixes <- read_csv(here('data', 'final_table_sixes.csv')) %>% 
  clean_names() %>% 
  select(cost)

south_umpqua <- read_csv(here('data', 'final_table_southumpqua.csv')) %>% 
  clean_names() %>% 
  select(cost)


tenmile <- read_csv(here('data', 'final_table_tenmile.csv')) %>% 
  clean_names() %>% 
  select(cost)

tillamook <- read_csv(here('data', 'final_table_tillamook.csv')) %>% 
  clean_names() %>% 
  select(cost)

yaquina <- read_csv(here('data', 'final_table_yaquina.csv')) %>% 
  clean_names() %>% 
  select(cost)

max_length <- max(length(alsea$cost), length(beaver$cost), length(coos$cost),
                  length(coquille$cost), length(floras$cost), length(lower_umpqua$cost),
                  length(middle_umpqua$cost), length(necanicum$cost), length(nehalem$cost),
                  length(nestucca$cost), length(north_umpqua$cost), length(salmon$cost),
                  length(siletz$cost), length(siltcoos$cost), length(siuslaw$cost),
                  length(sixes$cost), length(south_umpqua$cost), length(tenmile$cost),
                  length(tillamook$cost), length(yaquina$cost))

cost <- data.frame(alsea = c(alsea$cost, rep(NA, max_length - length(alsea$cost))),
                                   beaver = c(beaver$cost, rep(NA, max_length - length(beaver$cost))),
                                   coos = c(coos$cost, rep(NA, max_length - length(coos$cost))),
                                   coquille = c(coquille$cost, rep(NA, max_length - length(coquille$cost))),
                                   floras = c(floras$cost, rep(NA, max_length - length(floras$cost))),
                                   lower_umpqua = c(lower_umpqua$cost, rep(NA, max_length - length(lower_umpqua$cost))),
                                   middle_umpqua = c(middle_umpqua$cost, rep(NA, max_length - length(middle_umpqua$cost))),
                                   necanicum = c(necanicum$cost, rep(NA, max_length - length(necanicum$cost))),
                                   nehalem = c(nehalem$cost, rep(NA, max_length - length(nehalem$cost))),
                                   nestucca = c(nestucca$cost, rep(NA, max_length - length(nestucca$cost))),
                                   north_umpqua = c(north_umpqua$cost, rep(NA, max_length - length(north_umpqua$cost))),
                                   salmon = c(salmon$cost, rep(NA, max_length - length(salmon$cost))),
                                   siletz = c(siletz$cost, rep(NA, max_length - length(siletz$cost))),
                                   siltcoos = c(siltcoos$cost, rep(NA, max_length - length(siltcoos$cost))),
                                   siuslaw = c(siuslaw$cost, rep(NA, max_length - length(siuslaw$cost))),
                                   sixes = c(sixes$cost, rep(NA, max_length - length(sixes$cost))),
                                   south_umpqua = c(south_umpqua$cost, rep(NA, max_length - length(south_umpqua$cost))),
                                   tenmile = c(tenmile$cost, rep(NA, max_length - length(tenmile$cost))),
                                   tillamook = c(tillamook$cost, rep(NA, max_length - length(tillamook$cost))),
                                   yaquina = c(yaquina$cost, rep(NA, max_length - length(yaquina$cost))))
