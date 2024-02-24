
#..........................Calculate 'z' constant for each population.........................
# Isolate p_hat and population
p_hat_temp = equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']])) %>% #Sixes has negative p_hat, should we remove?
  select(population, p_hat)

# Isolate c_hat and population
c_hat_temp = equilibrium_all %>% 
  mutate(c_hat = map_dbl(coeff, ~.[['c_hat']])) %>% #Sixes has negative c_hat, should we remove?
  select(population, c_hat)

# Import b_passage dataframe (the dataframe should have the following columns: population, b_passage)
view(avg_passability)


# function to calculate z using p_hat and beta_passage
z_p_fcn <- function(p_hat, b_passage){
  z = p_hat / b_passage
  return(z)
}


# create a dataframe of p_hat, z, and bpassage
z_p_df <- bpassage_base %>%
  cbind(p_hat_temp$p_hat) %>% 
  rename(p_hat = 3) %>% 
  mutate(
    z = pmap_dbl(list(p_hat,bpassage),z_p_fcn) # use pmat_dbl to calculate z for each population
  )

# function to calculate z using c_hat and beta_passage
z_c_fcn <- function(c_hat, b_passage){
  z = c_hat / b_passage
  return(z)
}


# create a dataframe with c_hat, z, and bpassage
z_c_df <- bpassage_base %>%
  cbind(c_hat_temp$c_hat) %>% 
  rename(c_hat = 3) %>% 
  mutate(
    z = pmap_dbl(list(c_hat,bpassage),z_c_fcn) # use pmat_dbl to calculate z for each population
  )



#..........................calculate 'p_invest' and 'c_invest' using 'b_passage' after investment.........................

# function to calculate impact on productivity after investment
p_invest_fcn <- function(z,bpassage){
  p_invest = z * bpassage 
  return(p_invest)
}


# function to calculate impact on capacity after investment
c_invest_fcn <- function(z,bpassage){
  c_invest = z * bpassage
  return(c_invest)
}


#..........................Create portfolios using max_fcn.........................
## Simulating portfolios 
max_fcn <- function(weight){
  


  weight=weight %>% unlist()
 
  baseline <- equilibrium_all %>% 
    mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
           c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
    select(population, p_hat, c_hat) 
  
  p_invest <- pmap_dbl(list(z_p_df$z,z_p_df$b_passage, weight),p_invest_fcn) # this works! We should manually check this tho
  c_invest <- pmap_dbl(list(z_c_df$z,z_c_df$b_passage),c_invest_fcn) # renamed this, but feel free to change
  #impact_p <- p_change(b_passage = .555) ## this function is working. if we have multiple b_passages how can we manually change the values. another list? another purrr??? yikes
  #c_change = .001
  var <- sapply(coho[2:22], var)
  var_rm<-var[-18]

           
  #delta_p <- baseline$p_hat * (1 + p_change * weight) # I don't think we need this anymore? let's talk about this
  #delta_c <- baseline$c_hat * (1 + c_change * weight) # I don't think we need this anymore?
  s_invest <- ((p_invest - 1) * c_invest)
  s_baseline <- ((baseline$p_hat -1) * baseline$c_hat)
  var_invest <- var_rm * (s_invest^2)
  esu_returns <- sum(s_invest)
  esu_baseline <- sum(s_baseline)
  esu_var <- sum(var_invest)




  return(round(data.frame(s_invest, s_baseline),3)) #esu_returns,esu_baseline, esu_var
}

portfolios = map_df(.x=grid_list,~max_fcn(.x))



