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


## making a function using our original question --> s_t+1 = ((p_i)(S_t_i))/(1+ (S_t_i/c_i))
calculate_equil_abund <- function(abundance, p_hat, c_hat){
  y = (p_hat * abundance)/(1 + (abundance/c_hat))
  return(y)
}


## Population model before investment 
all_nls<- function(coho_recruits){
  
  #  browser()
  
  intercept = coefficients(lm(coho_recruits$recruits_flip ~ coho_recruits$abundance_flip,data=coho_recruits))[1]
  coefficient = coefficients(lm(coho_recruits$recruits_flip ~ coho_recruits$abundance_flip,data=coho_recruits))[2] 
  
  guess_p=1/coefficient
  guess_c=1/ (intercept * (1/ coefficient))
  
  nls(recruits~calculate_equil_abund(abundance, p_hat, c_hat),
      data = coho_recruits,
      start = list(p_hat=guess_p,c_hat=guess_c),
      control=nls.control(minFactor=1/8000,maxiter = 500,tol = 1e-03)) ## 5 
}

equilibrium_all <- coho_recruits %>% 
  filter(population!="tahkenitch") %>% 
  nest() %>% 
  mutate(nls_model = map(data, ~all_nls(.x))) %>% 
  mutate(coeff=map(nls_model, ~coefficients(.x)))