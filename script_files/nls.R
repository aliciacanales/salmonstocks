## s_t+1 = ((p_i)(S_t_i))/(1+ (S_t_i/c_i))
calculate_equil_abund <- function(abundance, p_hat, c_hat){
  y = (p_hat * abundance)/(1 + (abundance/c_hat))
  return(y)
}


## intercept = coefficients(lm(recruits_flip ~ abundance_flip))[1],
##coefficient = coefficients(lm(recruits_flip ~ abundance_flip))[2])

## creating function that will run over the entire datset
all_nls<- function(coho_recruits){
  group_by(population) %>%
    mutate(intercept = coefficients(lm(coho_recruits$recruits_flip ~ coho_recruits$abundance_flip))[1], 
              coefficient = coefficients(lm(coho_recruits$recruits_flip ~ coho_recruits$abundance_flip))[2]) %>% 
    #mutate(mdl1 = coefficients(lm(coho_recruits$recruits_flip ~ coho_recruits$abundance_flip))) %>%  
    mutate(p_hat = 1/coho_recruits$coefficients,
           c_hat = 1/ (coho_recruits$intercept * (1/coho_recruits$coefficients))) %>%  
  nls(return~calculate_equil_abund(abundance, p_hat, c_hat),
              data = coho_recruits,
              start = list(p_hat,
                           c_hat)) #list(p_hat = coho_guess$p_hat[1],c_hat = coho_guess$c_hat))
}

equilibrium_all <- coho_recruits %>% 
  nest() %>% 
  mutate(nls_model = map(data, ~all_nls(.x)))

# Official outputs for p hat and c hat
broom::tidy(run_nls)

nls_predict<-alsea_coho %>% 
  mutate(predict=predict(run_nls,newdata=.))

ggplot(data=nls_predict)+
  geom_point(aes(x=year,y=alsea))+
  geom_path(aes(x=year,y=predict),color='red')+
  theme_minimal()

## tables of nls values for control and first run. The productivity and capacity are negative... shouldn't they be positive?
broom::tidy(control_nls) %>% 
  kable(caption = "Control NLS") %>% 
  kable_classic()

broom::tidy(run_nls) %>% 
  kable(caption = "Original NLS") %>% 
  kable_classic()
