#### Code Nathan shared via email Week 10 and during office hours Week 9

library(tidyverse)
library(LaplacesDemon)
library(nloptr)

#Generate 1000 portfolio weights for all 20 streams
n=100
#raw<-rdirichlet(n,rep(1,20)) #Replace 20 with relative reference e.g. ncol(abundance_data)

abundance_data <- coho[2:22]
abundance_data <- data.frame(abundance_data[-18])

raw<-rdirichlet(n,rep(1,ncol(abundance_data)))

# I don't like the really small 0s and decimals. What is a budget of 0.0002%?
rounded<-round(raw,2)

#But now I need to make sure the weights still add up to 1. Take the difference at each element then sum. Add the sum to highest weighted stream

diff<-as.data.frame(raw-rounded) %>% 
  mutate(sum=rowSums(across(everything())))

for(i in 1:nrow(abundance_data)){
  rounded[i,which.max(rounded[i,])]<-rounded[i,which.max(rounded[i,])]+diff$sum[i]
}


# Check to make sure weights still add up to 1 for rounded dataset
check<-as.data.frame(rounded) %>% 
  mutate(sum=rowSums(across(everything()))) %>% ## Does not all add up to 1
  filter(sum==1.00) %>% ## filter for only portfolios that sum to 1
  select(-sum) ## remove sum column

# I think we should always run a few portfolios that are the same each time. Mainly, give the entire budget to each stream and see how it responds.

#full<-diag(20) #Replace 20 with relative reference e.g. ncol(abundance_data)
full<-diag(ncol(abundance_data))
# add set simulations with random weights

weights<-rbind(check,full)

# From here you can make the "weights" a dataframe add the stream names in each column and anything else to make it ready to pass into purrr map

colnames(weights) <- names(abundance_data) ## Assign column names from original dataframe




grid_list<-split(weights,seq(nrow(weights)))
  ## need to make it a list to pass through. Include the guess vectors in this list?


## Define eval_f (What do we define s_fun, p_hat_fun, c_hat_fun as to pass through nloptr?)

s_fun <- function(delta_p, delta_c){
  s_invest <- (delta_p-1)*delta_c
  return(s_invest)
}

## Calculate change in p_hat after investment
p_hat_fun <- function(p_hat,p_change, weight){ 
  p <- p_hat * (1 + p_change * weight) 
}

## Calculate change in c_hat after investment
c_hat_fun <- function(c_hat, c_change, weight){ 
  c <- c_hat * (1 + c_change * weight)
}

##### create new max function that incorporate p_hat_fun, c_hat_fun, s_fun, and variance

## The starting dataframe should have the following columns population, p_hat, c_hat, p_change, c_change, var
## Then insert weight matrix for multiple portfolios

## I was able to get some columns to work. I just realized I also didn't include the s function in this. - Alicia

max_fcn <- function(weight){
  weight=weights %>% unlist()
 
invest <- equilibrium_all %>% 
    mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
           c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
    select(population, p_hat, c_hat) 
  
  p_change = .001 
  c_change = .001
  var <- sapply(coho[2:22], var)
  var[-18]
           
  delta_p <- invest$p_hat * (1 + p_change * weight)
  delta_c <- invest$c_hat * (1 + c_change * weight)
  s_invest <- ((delta_p - 1) * delta_c)
  # var_invest <- var * (s_invest^2)

  return(round(data.frame(delta_p, delta_c, s_invest)))
                          # ,var_invest=out$objective),5))
}

try=map_df(.x=grid_list,~max_fcn(.x))

#####








max_fcn<-function(x){
  temp=x %>% unlist()
  
  # browser()
  out=nloptr(x0=temp, #guess vectors.
             eval_f=s_fun, #objective utility function
             #eval_g_eq = p_hat_fun,
             #eval_f_eq = c_hat_fun, #made up this term (delete)
             lb=c(0,0,0), #lower constraint 0 (becuase dealing with percentage)
             ub=c(1,1,1), #upper constraint 1 
             opts=options,
             mu=pop_mean, ##update to our means
             sigma=cov,
             var=var,
             gamma=0, #risk aversion parameter (variance)
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





