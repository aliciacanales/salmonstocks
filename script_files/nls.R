## s_t+1 = ((p_i)(S_t_i))/(1+ (S_t_i/c_i))
calculate_equil_abund <- function(abundance, p_hat, c_hat){
  y = (p_hat * abundance)/(1 + (abundance/c_hat))
  return(y)
}

## optimized/informed guesses for nls
guess_vec = coho_guess

## creating function that will run over the entire datset
all_nls<- function(coho_recruits){
  nls(return~calculate_equil_abund(abundance, p_hat, c_hat),
              data = coho_recruits,
              start = list(p_hat = guess_vec$p_hat,
                           c_hat = guess_vec$c_hat))
}

equilibrium_all <- coho_recruits %>% 
  group_by(population) %>% 
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
