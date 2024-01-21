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


########### Calculate 'z' for each population
## Isolate p_hat and population
p_hat_temp = equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']])) %>% 
  select(population, p_hat)

## Import beta dataframe the dataframe should have the following columns: population, beta_passage
### For now, create temp dataframe to get function running, replace when b_passage for each population is ready
b_passage_temp <- cbind(p_hat_temp$population) %>% 
  data.frame(b_passage=c(.000125, .0001, .000005, .0002, .00010, .00012, .000015, .0001, .00004, .00008, .000095, .00013, .0001, .000045, .00005, .00011, .0002, .000005, .000125, .000125)) %>%
  rename(population = 1)


## Create function to calculate z using p_hat and beta_passage
z_fcn <- function(p_hat, b_passage){
  z = p_hat / b_passage
  return(z)
}

z_b_df <- b_passage_temp %>% 
  cbind(p_hat_temp$p_hat) %>%
  rename(p_hat = 3) %>% 
  mutate(
    z = pmap_dbl(list(p_hat,b_passage),z_fcn)
  )
########### z calculation working and ready for b_passage input when data is ready


########### calculate b_invest (beta passage after investment with weight allocation). Actually the weight allocation should be down below, so nevermind. helpppp this is hard
## we need to have an intermediate between these two steps where our weights come in to impact b_invest

########## create function to see impact on productivity after investment
p_invest_fcn <- function(z,b_passage,weight){
  p_invest = z * (b_passage * (weight * 1000000)) #replace 1000000 with defined budget, but doing manually first to check if it works
  return(p_invest) # this is wrong right now because we are multiplying money by passage, but we need to multiply money by investment in increasing passage so that it is money into money
}

########## create function to see impact on capacity after investment
c_invest_fcn <- function(z,b_passage){
  c_invest = z * (b_passage)
  return(c_invest)
}

# test function outside to make sure it works
p_temp <- z_b_df %>% # using fake data right now but its working, so cool!
  mutate(
    p_invest = pmap_dbl(list(z,b_passage),c_invest_fcn) # this b_passage should be b_invest (update when we have real data and weights added in)
  )


####### to link weight allocations to budget, we need to:
# define budgets (make it a dataframe)
# weight * budget
# multiply this by b_baseline? or is there a better way to link it? Q for Nathan and Tamma

## Simulating portfolios 
max_fcn <- function(weight){
  

  weight=weight %>% unlist()
 
  baseline <- equilibrium_all %>% 
    mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
           c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
    select(population, p_hat, c_hat) 
  
  p_invest <- pmap_dbl(list(z_b_df$z,z_b_df$b_passage, weight),p_invest_fcn) ## this works! We should manually check this tho
  c_invest <- pmap_dbl(list(z_b_df$z,z_b_df$b_passage),c_invest_fcn)
  #impact_p <- p_change(b_passage = .555) ## this function is working. if we have multiple b_passages how can we manually change the values. another list? another purrr??? yikes
  #c_change = .001
  var <- sapply(coho[2:22], var)
  var_rm<-var[-18]
           
  #delta_p <- baseline$p_hat * (1 + p_change * weight) #can we change delta_p to p_invest?
  #delta_c <- baseline$c_hat * (1 + c_change * weight) #can we change delta_c to c_invest?
  s_invest <- ((p_invest - 1) * c_invest)
  s_baseline <- ((baseline$p_hat -1) * baseline$c_hat)
  var_invest <- var_rm * (s_invest^2)
  esu_returns <- sum(s_invest)
  esu_baseline <- sum(s_baseline)
  esu_var <- sum(var_invest)


  return(round(data.frame(esu_returns,esu_baseline, esu_var),3))
}

portfolios = map_df(.x=grid_list,~max_fcn(.x))

