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

## Big Function
# s_fun <- function(population, w, p_change, c_change){
#   eq <- (p_hat(weight, p_change) - 1) * c_hat(weight,c_change)
#   return(eq)
# }

## Calculate change in p_hat which will be put into "big equation"
# This really means: P is a function of Alpha(passage) * w(i) + p_hat (from nls)
p_hat <- function(p_hat,p_change, w){ 
  p <- (w * p_change) + p_hat
}

## Calculate change in c_hat which will be put into "big equation"
# This really means: c is a function of Beta(passage) * w(i) + c_hat (from nls)
c_hat <- function(c_hat, c_change, w){ 
  c <- (w * c_change) + c_hat
}

## bringing out the coefficients into separate columns and applying a $10 investment which will have a .01 increase
new_stock <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>% 
  mutate(weight = 10,
         p_change = .01,
         c_change = .01,
         delta_p = map(p_hat, ~p_hat(.x,p_change, weight)),
         delta_c = map(c_hat, ~c_hat(.x,c_change, weight)))

new_stock2 <- pmap(new_stock, ~s_fun(..1, ..3, ..4, ..5))



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
