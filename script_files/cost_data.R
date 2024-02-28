library(Dict)
##.....................Isolating cost data...............

alsea_barriers <- read_csv(here('data', 'final_table_alsea_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

beaver_barriers <- read_csv(here('data', 'final_table_beaver_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

coos_barriers <- read_csv(here('data', 'final_table_coosbay_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

coquille_barriers <- read_csv(here('data', 'final_table_coquille_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

floras_barriers <- read_csv(here('data', 'final_table_floras_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

lower_umpqua_barriers <- read_csv(here('data', 'final_table_lowerUmp_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

middle_umpqua_barriers <- read_csv(here('data', 'final_table_midUmp_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

necanicum_barriers <- read_csv(here('data', 'final_table_necanicum_v3.csv')) %>% 
  clean_names() %>% 
  rename(pass_score=pass_scores) %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

nehalem_barriers <- read_csv(here('data', 'final_table_nehalem_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

nestucca_barriers <- read_csv(here('data', 'final_table_nestucca_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

north_umpqua_barriers <- read_csv(here('data', 'final_table_northUmp_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

salmon_barriers <- read_csv(here('data', 'final_table_salmon_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

siletz_barriers <- read_csv(here('data', 'final_table_siletz_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

siltcoos_barriers <- read_csv(here('data', 'final_table_siltcoos_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

siuslaw_barriers <- read_csv(here('data', 'final_table_siuslaw_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

sixes_barriers <- read_csv(here('data', 'final_table_sixes_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

south_umpqua_barriers <- read_csv(here('data', 'final_table_southumpqua_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

tenmile_barriers <- read_csv(here('data', 'final_table_tenmile_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

tillamook_barriers <- read_csv(here('data', 'final_table_tillamook_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

yaquina_barriers <- read_csv(here('data', 'final_table_yaquina_v3.csv')) %>% 
  clean_names() %>% 
  select(c(pass_score, strm_lev, cost)) %>% 
  arrange(cost)
# arrange(strm_lev,pass_score)

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


##................Starting dictionary..................
ages <- Dict$new(
  Charlie = 40L,
  Alice = 30L,
  Bob = 25L,
  .class = "integer",
  .overwrite = TRUE
)
