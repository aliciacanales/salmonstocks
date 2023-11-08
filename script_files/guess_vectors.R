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

## calculate pre_hat to then calculate p_hat and c_hat not in flipped form.
pre_hat <- coho_recruits %>% 
  group_by(population) %>% 
  summarize(intercept = coefficients(lm(recruits_flip ~ abundance_flip))[1], coefficient=coefficients(lm(recruits_flip ~ abundance_flip))[2])

pre_hat

## Take pre_hat out of flipped form to calculate guess vectors (p_hat and c_hat)

alsea_guess <- c(1 / pre_hat$coefficients[2],
                 1 / (pre_hat$coefficients[1] * (1/pre_hat$coefficients[2])))
alsea_guess

