## all 21 populations 

## Should we move lines 5-14 to the nls script?

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


###################################################################################################################################################################################
## Don't think we need any of this

## calculating pre_hat coefficients 
# pre_hat <- coho_recruits %>% 
#   group_by(population) %>% 
#   summarize(intercept = coefficients(lm(recruits_flip ~ abundance_flip))[1], 
#             coefficient = coefficients(lm(recruits_flip ~ abundance_flip))[2]) %>%
#   mutate(p_hat = 1/coefficient, ## calculating p_hat and c_hat for all 21 populations
#          c_hat = 1/ (intercept * (1/ coefficient)))
# 
# pre_hat

