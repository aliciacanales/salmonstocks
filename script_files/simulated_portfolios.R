library(tidyverse)
library(LaplacesDemon)
library(nloptr)


n=100
abundance_data <- coho[2:22] # raw population abundance data
abundance_data <- data.frame(abundance_data[-18]) # remove Tahkenitch
raw<-rdirichlet(n,rep(1,ncol(abundance_data))) #Generate 100 portfolio weights for all 20 streams
rounded<-round(raw,2) # round to two decimals

# Need to make sure the weights still add up to 1
diff<-as.data.frame(raw-rounded) %>% # Take the difference at each element
  mutate(sum=rowSums(across(everything()))) # Sum across rows. Add the sum to highest weighted stream

for(i in 1:nrow(abundance_data)){
  rounded[i,which.max(rounded[i,])]<-rounded[i,which.max(rounded[i,])]+diff$sum[i]
}

# Check to make sure weights still add up to 1 for rounded dataframe
check<-as.data.frame(rounded) %>% 
  mutate(sum=rowSums(across(everything()))) %>% 
  filter(sum==1.00) %>% ## filter for only portfolios that sum to 1
  select(-sum)

# Add in QC: run a few portfolios that are the same each time, so we can make sure it is running correctly. Give the entire budget to each stream and see how it responds
full<-diag(ncol(abundance_data)) # create set portfolios

# add set simulations with random weights
#weights<-rbind(check,full) ## commenting this out just for now to run it with fewer portfolios while we get function running
# weights <- check


## Renaming column names and making df a list 
colnames(weights) <- names(abundance_data) 
grid_list<-split(weights,seq(nrow(weights)))

p_change <- function(b_passage){
  z <- 5.5 ## arbitrary number for the z constant 
  y = z * b_passage
  return(y)
}


## Simulating portfolios 
max_fcn <- function(weight){
  

  weight=weight %>% unlist()
 
  baseline <- equilibrium_all %>% 
    mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
           c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
    select(population, p_hat, c_hat) 
  
  impact_p <- p_change(b_passage = .555) ## this function is working. if we have multiple b_passages how can we manually change the values. another list? another purrr??? yikes
  c_change = .001
  var <- sapply(coho[2:22], var)
  var_rm<-var[-18]
           
  delta_p <- baseline$p_hat * (1 + impact_p * weight)
  delta_c <- baseline$c_hat * (1 + c_change * weight)
  s_invest <- ((delta_p - 1) * delta_c)
  s_baseline <- ((baseline$p_hat -1) * baseline$c_hat)
  var_invest <- var_rm * (s_invest^2)
  esu_returns <- sum(s_invest)
  esu_var <- sum(var_invest)


  return(round(data.frame(esu_returns, esu_var),3))
}

portfolios = map_df(.x=grid_list,~max_fcn(.x))

