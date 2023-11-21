## making a function using our original question --> s_t+1 = ((p_i)(S_t_i))/(1+ (S_t_i/c_i))
calculate_equil_abund <- function(abundance, p_hat, c_hat){
  y = (p_hat * abundance)/(1 + (abundance/c_hat))
  return(y)
}


## creating function that will run over the entire dataset
all_nls<- function(coho_recruits){
  
#  browser()

    intercept = coefficients(lm(coho_recruits$recruits_flip ~ coho_recruits$abundance_flip,data=coho_recruits))[1]
  coefficient = coefficients(lm(coho_recruits$recruits_flip ~ coho_recruits$abundance_flip,data=coho_recruits))[2] 
    
  guess_p=1/coefficient
  guess_c=1/ (intercept * (1/ coefficient))

  nls(recruits~calculate_equil_abund(abundance, p_hat, c_hat),
              data = coho_recruits,
              start = list(p_hat=guess_p,c_hat=guess_c),
              control=nls.control(minFactor=1/8000,maxiter = 500,tol = 1e-03))
}

equilibrium_all <- coho_recruits %>% 
  filter(population!="tahkenitch") %>% 
  nest() %>% 
  mutate(nls_model = map(data, ~all_nls(.x))) %>% 
  mutate(coeff=map(nls_model, ~coefficients(.x)))


#####################

### Code up the model for a hypothetical scenario to create framework:
## $10 of investment leads to a 1% increase in p and c

calculate_return_investment <- function(p, c, alpha, beta, weight){
  y = (((p*alpha(weight))-1)*(c*beta(weight)))
  return(y)
}

### Notes from Nathan's office hours on 11/17
## Big Function
S <- function(w, p_change, c_change){
  equilibrium <- (p_hat(w,p_change)-1)c_hat(w,c_change)
}

## Calculate change in p_hat which will be put into "big equation"
## This really means: P is a function of Beta(passage) * w(i) + p_hat (from NLS)
p_hat <- function(w,p_change, c_hat_nls){ #can probably remove p_hat_nls
  p <- (w * p_change) + p_hat_nls
}

## Calculate change in c_hat which will be put into "big equation"
c_hat <- function(w, c_change, c_hat_nls){ #can probably remove p_hat_nls
  c <- (w * c_change) + c_hat_nls
}





###################################################################################################################################################################################
## Don't think we need any of this 


# # Official outputs for p hat and c hat
# broom::tidy(run_nls)
# 
# nls_predict<-alsea_coho %>% 
#   mutate(predict=predict(run_nls,newdata=.))
# 
# ggplot(data=nls_predict)+
#   geom_point(aes(x=year,y=alsea))+
#   geom_path(aes(x=year,y=predict),color='red')+
#   theme_minimal()
# 
# ## tables of nls values for control and first run. The productivity and capacity are negative... shouldn't they be positive?
# broom::tidy(control_nls) %>% 
#   kable(caption = "Control NLS") %>% 
#   kable_classic()
# 
# broom::tidy(run_nls) %>% 
#   kable(caption = "Original NLS") %>% 
#   kable_classic()
