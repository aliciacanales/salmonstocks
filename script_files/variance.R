#### Code Nathan shared via email Week 10 and during office hours Week 9

library(tidyverse)
library(LaplacesDemon)
library(nloptr)

#Generate 1000 portfolio weights for all 20 streams
n=1000
#raw<-rdirichlet(n,rep(1,20)) #Replace 20 with relative reference e.g. ncol(abundance_data)

abundance_data <- coho[2:22]
abundance_data <- data.frame(abundance_data[-18])

raw<-rdirichlet(n,rep(1,ncol(abundance_data)))

# I don't like the really small 0s and decimals. What is a budget of 0.0002%?
rounded<-round(raw,2)

#But now I need to make sure the weights still add up to 1. Take the difference at each element then sum. Add the sum to highest weighted stream

diff<-as.data.frame(raw-rounded) %>% 
  mutate(sum=rowSums(across(everything())))

for(i in 1:nrow(c)){
  rounded[i,which.max(rounded[i,])]<-rounded[i,which.max(rounded[i,])]+diff$sum[i]
}

# Check to make sure weights still add up to 1 for rounded dataset
check<-as.data.frame(rounded) %>% 
  mutate(sum=rowSums(across(everything()))) ## Does not all add up to 1, need to fix

# I think we should always run a few portfolios that are the same each time. Mainly, give the entire budget to each stream and see how it responds.

#full<-diag(20) #Replace 20 with relative reference e.g. ncol(abundance_data)
full<-diag(ncol(abundance_data))
# add set simulations with random weights

weights<-rbind(rounded,full)

# From here you can make the "weights" a dataframe add the stream names in each column and anything else to make it ready to pass into purrr map

colnames(weights) <- names(abundance_data) ## Assign column names from original dataframe

grid_list<-split(weights,seq(nrow(weights))) ## need to make it a list to pass through. Include the guess vectors in this list?


## test, created an arbitrary function to get this to run
objective_endo <- function(x){
  y = 100 * x^2
  return(y)
}
##

max_fcn<-function(x){
  temp=x %>% unlist()
  
  out=nloptr(x0=temp, #guess vectors.
             eval_f=objective_endo,
             #eval_g_eq = constraints_endo,
             lb=c(0,0,0),
             ub=c(1,1,1),
             opts=options,
             mu=pop_mean, ##update to our means
             sigma=cov,
             var=var,
             gamma=0,
             budget=1,
             alpha=alpha)
  
  tempsol=out$solution
  
  
  return(round(data.frame(x=out$solution[1],y=out$solution[2],z=out$solution[3],obj=out$objective),5))
}

try=map_df(.x=grid_list,~max_fcn(.x))


c=filter(try,obj==min(try$obj))






######### Nathan's code he shared in Week 9 office hours

a<-expand_grid(x=seq(0,.5,by=.05),y=seq(0,.5,by=.05),z=seq(0,.5,by=.05)) %>%
  mutate(sum=rowSums(across(everything()))) %>%
  filter(sum==1) %>%
  select(-sum) %>%
  filter(if_all(everything())>=1)



grid_list<-split(a,seq(nrow(a)))

max_fcn<-function(x){
  temp=x %>% unlist()
  
  out=nloptr(x0=temp,
             eval_f=objective_endo,
             eval_g_eq = constraints_endo,
             lb=c(0,0,0),
             ub=c(1,1,1),
             opts=options,
             mu=means,
             sigma=cov,
             var=var,
             gamma=0,
             budget=1,
             alpha=alpha)
  
  tempsol=out$solution
  
  
  return(round(data.frame(x=out$solution[1],y=out$solution[2],z=out$solution[3],obj=out$objective),5))
}

try=map_df(.x=grid_list,~max_fcn(.x))


c=filter(try,obj==min(try$obj))





