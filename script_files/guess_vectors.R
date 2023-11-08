## all 21 populations 

## cleaning data to generate reciprocal form of recruits and abundance 
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

## calculating pre_hat coefficients 
pre_hat <- coho_recruits %>% 
  group_by(population) %>% 
  summarize(intercept = coefficients(lm(recruits_flip ~ abundance_flip))[1], 
            coefficient = coefficients(lm(recruits_flip ~ abundance_flip))[2])

pre_hat

## calculating p_hat and c_hat for all 21 populations


## Take pre_hat out of flipped form to calculate guess vectors (p_hat and c_hat)

alsea_guess <- c(1 / pre_hat$coefficients[2],
                 1 / (pre_hat$coefficients[1] * (1/pre_hat$coefficients[2])))
alsea_guess

