## all 21 populations 

## needs cleaning but it works 
coho_recruits <- coho %>% 
pivot_longer(cols = 2:22,
             names_to = 'population',
             values_to = 'abundance') %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(population) %>% 
  mutate(recruits = lead(abundance)) %>% 
  drop_na() %>% 
  mutate(recruits_flip = 1/ recruits,
         abundance_flip = 1/ abundance)
coeff_fun<- function(rec, abund, data){
  nest() %>% 
    lm(recruits_flip~abundance_flip, data = )
}

coho_recruits %>% 
  nest() %>% 
  mutate(coeff = map(lm(recruits_flip~abundance_flip, data = .x)))


pre_hat <- do(tidy(lm(recruits_flip~abundance_flip, data = coho_recruits)))
pre_hat
