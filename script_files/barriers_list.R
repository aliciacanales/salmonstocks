## making the barrier data into a list of dataframes with only cost, stream level, and passability for all barriers. Arranged by stream level and stream ID
alsea_barriers <- read_csv(here('data', 'final_table_alsea_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

beaver_barriers <- read_csv(here('data', 'final_table_beaver_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id')%>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

coos_barriers <- read_csv(here('data', 'final_table_coosbay_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

coquille_barriers <- read_csv(here('data', 'final_table_coquille_v3.csv')) %>% 
  clean_names() %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost),
         barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

floras_barriers <- read_csv(here('data', 'final_table_floras_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

lower_umpqua_barriers <- read_csv(here('data', 'final_table_lowerUmp_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

middle_umpqua_barriers <- read_csv(here('data', 'final_table_midUmp_v3.csv')) %>% 
  clean_names() %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost),
         barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

necanicum_barriers <- read_csv(here('data', 'final_table_necanicum_v3.csv')) %>% 
  clean_names() %>% 
  rename(pass_score=pass_scores) %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost),
         barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id')%>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

nehalem_barriers <- read_csv(here('data', 'final_table_nehalem_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

nestucca_barriers <- read_csv(here('data', 'final_table_nestucca_v3.csv')) %>% 
  clean_names() %>% 
  mutate(cost = ifelse(cost == 0.00, 19097.2, cost),
          barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id')

north_umpqua_barriers <- read_csv(here('data', 'final_table_northUmp_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

salmon_barriers <- read_csv(here('data', 'final_table_salmon_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

siletz_barriers <- read_csv(here('data', 'final_table_siletz_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

siltcoos_barriers <- read_csv(here('data', 'final_table_siltcoos_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

siuslaw_barriers <- read_csv(here('data', 'final_table_siuslaw_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

sixes_barriers <- read_csv(here('data', 'final_table_sixes_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

south_umpqua_barriers <- read_csv(here('data', 'final_table_southumpqua_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

tenmile_barriers <- read_csv(here('data', 'final_table_tenmile_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

tillamook_barriers <- read_csv(here('data', 'final_table_tillamook_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

yaquina_barriers <- read_csv(here('data', 'final_table_yaquina_v3.csv')) %>% 
  clean_names() %>% 
  mutate(barrier_id = row_number()) %>% 
  select(c(barrier_id, pass_score, stream_id, strm_lev, cost)) %>% 
  arrange(strm_lev,stream_id, pass_score) %>% 
  rename('strm_id' = 'stream_id') %>% 
  mutate(pass_score = ifelse(pass_score == 1.0, 0.9, pass_score))

barrier_list<- list(alsea = alsea_barriers,
                    
                    beaver = beaver_barriers,
                    
                    coos = coos_barriers,
                    
                    coquille = coquille_barriers,
                    
                    floras = floras_barriers,
                    
                    lower_umpqua = lower_umpqua_barriers,
                    
                    middle_umpqua = middle_umpqua_barriers,
                    
                    necanicum = necanicum_barriers,
                    
                    nehalem = nehalem_barriers,
                    
                    nestucca = nestucca_barriers,
                    
                    north_umpqua = north_umpqua_barriers,
                    
                    salmon = salmon_barriers,
                    
                    siletz = siletz_barriers,
                    
                    siltcoos = siltcoos_barriers,
                    
                    siuslaw = siuslaw_barriers,
                    
                    sixes = sixes_barriers,
                    
                    south_umpqua = south_umpqua_barriers,
                    
                    tenmile = tenmile_barriers,
                    
                    tillamook = tillamook_barriers,
                    
                    yaquina = yaquina_barriers)

