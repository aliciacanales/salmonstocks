## barrier data

alsea <- read_csv(here('data', 'final_table_alsea.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

beaver <- read_csv(here('data', 'final_table_beaver.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

coos <- read_csv(here('data', 'final_table_coos.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

coquille <- read_csv(here('data', 'final_table_coquille.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

floras <- read_csv(here('data', 'final_table_floras.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

lower_umpqua <- read_csv(here('data', 'final_table_lowerump.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

middle_umpqua <- read_csv(here('data', 'final_table_midump.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

necanicum <- read_csv(here('data', 'final_table_necanicum.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

nehalem <- read_csv(here('data', 'final_table_nehalem.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

nestucca <- read_csv(here('data', 'final_table_nestucca.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

north_umpqua <- read_csv(here('data', 'final_table_northump.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

salmon <- read_csv(here('data', 'final_table_salmon.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

siletz <- read_csv(here('data', 'final_table_siletz.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

siltcoos <- read_csv(here('data', 'final_table_siltcoos.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass') 

siuslaw <- read_csv(here('data', 'final_table_siuslaw.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

sixes <- read_csv(here('data', 'final_table_sixes.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

south_umpqua <- read_csv(here('data', 'final_table_southumpqua.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

tenmile <- read_csv(here('data', 'final_table_tenmile.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

tillamook <- read_csv(here('data', 'final_table_tillamook.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

yaquina <- read_csv(here('data', 'final_table_yaquina.csv')) %>% 
  clean_names() %>% 
  rename('bpassage' = 'beta_pass')

tillamook <- tillamook %>% 
  arrange(pass_score)


