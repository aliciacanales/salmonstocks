## running a for loop to pass this function to each population in the dataset
calculate_returns <- function(data) {
  rt <- numeric(length(data) - 1)  
  for (i in 2:length(data)) {
    rt[i-1] <- (data[i-1] / data[i]) - 1
  }
  return(rt)
} ## i-1 is the current population for whatever year we are looking at and i is the initial population from the previous year

returns <- data.frame(apply(coho[2:22], 2, calculate_returns))


## calculation for returns

## means of the returns for each population
pop_return_mean <- sapply(returns, mean)
pop_return_mean_df <- as.data.frame(pop_return_mean)

## mean of ALL means -- should return one number: 0.774
mean_returns <- mean(pop_return_mean)


## variance of the returns
pop_return_variance <- sapply(returns, var)

## mean of ALL variances -- should return one number: 6.889
mean_variance <- mean(pop_return_variance)

pop_return_sd <- sapply(returns, sd)

###Table is in rmarkdown


## Calculate variance covariance matrix. 
dmat <- cov(returns) #This is the variance covariance matrix which is amat (diagonal is the variance) that will be inputted into Quadprog

# pop_cov

## Only show bottom triangle of covariances
upper<-dmat
upper[upper.tri(dmat)] <-""
upper<-as.data.frame(upper)
upper

dmat


# combining date column to new returns df
returns_yr<- cbind(returns, coho_yr$year[-1]) %>% 
  rename('year' = 'coho_yr$year[-1]')

return_ts <- returns_yr %>%
  as_tsibble(key = NULL, index = year)


###Individual plot in rmarkdown