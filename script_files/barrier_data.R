## ...............................Getting passability isolated..............................
alsea <- read_csv(here('data', 'final_table_alsea.csv')) %>% 
  clean_names() %>% 
  select(pass_score) %>% 
  arrange(pass_score)


beaver <- read_csv(here('data', 'final_table_beaver.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)


coos <- read_csv(here('data', 'final_table_coos.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

coquille <- read_csv(here('data', 'final_table_coquille.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

floras <- read_csv(here('data', 'final_table_floras.csv')) %>% 
  clean_names()%>% 
  select(pass_score) %>% 
  arrange(pass_score)

lower_umpqua <- read_csv(here('data', 'final_table_lowerump.csv')) %>% 
  clean_names() %>% 
  select(pass_score) %>% 
  arrange(pass_score)

middle_umpqua <- read_csv(here('data', 'final_table_midump.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

necanicum <- read_csv(here('data', 'final_table_necanicum.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

nehalem <- read_csv(here('data', 'final_table_nehalem.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

nestucca <- read_csv(here('data', 'final_table_nestucca.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

north_umpqua <- read_csv(here('data', 'final_table_northump.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

salmon <- read_csv(here('data', 'final_table_salmon.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

siletz <- read_csv(here('data', 'final_table_siletz.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

siltcoos <- read_csv(here('data', 'final_table_siltcoos.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

siuslaw <- read_csv(here('data', 'final_table_siuslaw.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

sixes <- read_csv(here('data', 'final_table_sixes.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

south_umpqua <- read_csv(here('data', 'final_table_southumpqua.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)


tenmile <- read_csv(here('data', 'final_table_tenmile.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

tillamook <- read_csv(here('data', 'final_table_tillamook.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)

yaquina <- read_csv(here('data', 'final_table_yaquina.csv')) %>% 
  clean_names() %>% 
  select(pass_score)%>% 
  arrange(pass_score)



## creating this max length to find the max length the passability df should be. It should be as long as the population with the most barriers which is s_umpqua.
max_length <- max(length(alsea$pass_score), length(beaver$pass_score), length(coos$pass_score),
                  length(coquille$pass_score), length(floras$pass_score), length(lower_umpqua$pass_score),
                  length(middle_umpqua$pass_score), length(necanicum$pass_score), length(nehalem$pass_score),
                  length(nestucca$pass_score), length(north_umpqua$pass_score), length(salmon$pass_score),
                  length(siletz$pass_score), length(siltcoos$pass_score), length(siuslaw$pass_score),
                  length(sixes$pass_score), length(south_umpqua$pass_score), length(tenmile$pass_score),
                  length(tillamook$pass_score), length(yaquina$pass_score))

## Passability data frame with all the barriers from each population. NA values filled in the extra spaces. Not sure how the NAs will play out in out functions. 
## This was really quick. Just a bunch of copy and paste, so no biggie if we have to get rid of it bc of all the NAs I added. It was the only way to combine all the columns together. -AC

passability_values_df <- data.frame(alsea = c(alsea$pass_score, rep(NA, max_length - length(alsea$pass_score))),
                          beaver = c(beaver$pass_score, rep(NA, max_length - length(beaver$pass_score))),
                          coos = c(coos$pass_score, rep(NA, max_length - length(coos$pass_score))),
                          coquille = c(coquille$pass_score, rep(NA, max_length - length(coquille$pass_score))),
                          floras = c(floras$pass_score, rep(NA, max_length - length(floras$pass_score))),
                          lower_umpqua = c(lower_umpqua$pass_score, rep(NA, max_length - length(lower_umpqua$pass_score))),
                          middle_umpqua = c(middle_umpqua$pass_score, rep(NA, max_length - length(middle_umpqua$pass_score))),
                          necanicum = c(necanicum$pass_score, rep(NA, max_length - length(necanicum$pass_score))),
                          nehalem = c(nehalem$pass_score, rep(NA, max_length - length(nehalem$pass_score))),
                          nestucca = c(nestucca$pass_score, rep(NA, max_length - length(nestucca$pass_score))),
                          north_umpqua = c(north_umpqua$pass_score, rep(NA, max_length - length(north_umpqua$pass_score))),
                          salmon = c(salmon$pass_score, rep(NA, max_length - length(salmon$pass_score))),
                          siletz = c(siletz$pass_score, rep(NA, max_length - length(siletz$pass_score))),
                          siltcoos = c(siltcoos$pass_score, rep(NA, max_length - length(siltcoos$pass_score))),
                          siuslaw = c(siuslaw$pass_score, rep(NA, max_length - length(siuslaw$pass_score))),
                          sixes = c(sixes$pass_score, rep(NA, max_length - length(sixes$pass_score))),
                          south_umpqua = c(south_umpqua$pass_score, rep(NA, max_length - length(south_umpqua$pass_score))),
                          tenmile = c(tenmile$pass_score, rep(NA, max_length - length(tenmile$pass_score))),
                          tillamook = c(tillamook$pass_score, rep(NA, max_length - length(tillamook$pass_score))),
                          yaquina = c(yaquina$pass_score, rep(NA, max_length - length(yaquina$pass_score))))


## ................................Getting bpassage isolated................................
alsea <- read_csv(here('data', 'final_table_alsea.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(alsea = 'beta_pass') 


beaver <- read_csv(here('data', 'final_table_beaver.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(beaver = 'beta_pass')

coos <- read_csv(here('data', 'final_table_coos.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(coos = 'beta_pass')

coquille <- read_csv(here('data', 'final_table_coquille.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(coquille = 'beta_pass')

floras <- read_csv(here('data', 'final_table_floras.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(floras = 'beta_pass')

lower_umpqua <- read_csv(here('data', 'final_table_lowerump.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(lower_umpqua = 'beta_pass')

middle_umpqua <- read_csv(here('data', 'final_table_midump.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(middle_umpqua = 'beta_pass')

necanicum <- read_csv(here('data', 'final_table_necanicum.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(necanicum = 'beta_pass')

nehalem <- read_csv(here('data', 'final_table_nehalem.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(nehalem = 'beta_pass')

nestucca <- read_csv(here('data', 'final_table_nestucca.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(nestucca = 'beta_pass')

north_umpqua <- read_csv(here('data', 'final_table_northump.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(north_umpqua = 'beta_pass')

salmon <- read_csv(here('data', 'final_table_salmon.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(salmon = 'beta_pass')

siletz <- read_csv(here('data', 'final_table_siletz.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(siletz = 'beta_pass')

siltcoos <- read_csv(here('data', 'final_table_siltcoos.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(siltcoos = 'beta_pass')

siuslaw <- read_csv(here('data', 'final_table_siuslaw.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(siuslaw = 'beta_pass')

sixes <- read_csv(here('data', 'final_table_sixes.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(sixes = 'beta_pass')

south_umpqua <- read_csv(here('data', 'final_table_southumpqua.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(south_umpqua = 'beta_pass')

tenmile <- read_csv(here('data', 'final_table_tenmile.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(tenmile = 'beta_pass')

tillamook <- read_csv(here('data', 'final_table_tillamook.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(tillamook = 'beta_pass')

yaquina <- read_csv(here('data', 'final_table_yaquina.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(yaquina = 'beta_pass')

bpassage_base <- as.data.frame(cbind(alsea, beaver, coos, coquille,floras,lower_umpqua, middle_umpqua, necanicum, nehalem, nestucca, north_umpqua, salmon, siletz, siltcoos, siuslaw, sixes, south_umpqua, tenmile, tillamook, yaquina)) %>% 
  pivot_longer(1:20,
               names_to = 'population',
               values_to = 'bpassage')




