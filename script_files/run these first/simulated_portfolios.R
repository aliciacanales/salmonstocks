library(tidyverse)
library(LaplacesDemon)
library(nloptr)


#..........................Create randomized weight allocations.........................
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
weights <- check


## Renaming column names and making df a list 
colnames(weights) <- names(abundance_data) 
grid_list<-split(weights,seq(nrow(weights)))


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
# b_passage_temp <- cbind(p_hat_temp$population) %>% 
#   data.frame(b_passage=c(.000125, .0001, .000005, .0002, .00010, .00012, .000015, .0001, .00004, .00008, .000095, .00013, .0001, .000045, .00005, .00011, .0002, .000005, .000125, .000125)) %>%
#   rename(population = 1) # replace with real data: 'bpassage_base'
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

# test function outside to make sure it works
# p_temp <- z_p_df %>% # using made-up data right now, but its working, so cool!
#   mutate(
#     p_invest = pmap_dbl(list(z,b_passage),p_invest_fcn) # need to add weight
#   )



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



