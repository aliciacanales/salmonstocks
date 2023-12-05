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

### Hypothetical scenario to create framework: $10 of investment leads to a 1% increase in p and c

# calculate_return_investment <- function(p, c, alpha, beta, weight){
#   y = (((p*alpha(weight))-1)*(c*beta(weight)))
#   return(y)
# }

## Big Function - can we instead use the simpler function below (s_fun_new) to calculate s? - OS
# s_fun <- function(population, weight, p_change, c_change){
#   eq <- (p_hat(weight, p_change) - 1) * c_hat(weight,c_change)
#   return(eq)
# }

# Calculate s for population after investment
s_fun <- function(delta_p, delta_c){
  s_invest <- (delta_p-1)*delta_c
  return(s_invest)
}

## Calculate change in p_hat after investment
p_hat_fun <- function(p_hat,p_change, weight){ 
  p <- p_hat * (1 + p_change * weight) # don't need to divide weight by 10 because we already accounted for it with p_change
}

## Calculate change in c_hat after investment
c_hat_fun <- function(c_hat, c_change, weight){ 
  c <- c_hat * (1 + c_change * weight)
}

## create 10 Portfolios with weights that sum to 100
wgt_1 <- data.frame(weight=c(30,70,0,160,10,90,0,20,0,40,0,130,90,50,20,80,40,50,0,120))
wgt_2 <- data.frame(weight=c(8,1,2,7,4,0,2,5,12,1,14,3,7,5,8,3,1,8,3,6))
wgt_3 <- data.frame(weight=c(0,9,4,2,7,8,2,3,0,4,3,5,8,14,6,3,4,5,10,3))
wgt_4 <- data.frame(weight=c(4,5,3,11,4,1,6,6,7,2,9,8,1,0,6,7,2,9,5,4))
wgt_5 <- data.frame(weight=c(7,8,4,0,3,4,11,8,2,1,6,3,9,3,7,5,1,3,8,7))
wgt_6 <- data.frame(weight=c(10,2,5,1,7,4,9,2,3,0,2,14,3,10,2,3,5,1,9,8))
wgt_7 <- data.frame(weight=c(5,6,3,4,8,7,5,5,4,6,3,4,8,7,5,4,0,9,5,2))
wgt_8 <- data.frame(weight=c(1,2,12,6,3,9,4,2,8,0,3,0,7,7,5,11,3,6,3,8))
wgt_9 <- data.frame(weight=c(3,7,1,16,4,2,7,8,8,6,0,3,8,2,11,1,1,5,0,7))
wgt_10 <- data.frame(weight=c(9,6,3,5,1,3,8,0,9,13,2,1,7,2,1,12,8,3,2,5))
sum(wgt_1$weight)

## sum to 1000
wgt_1 <- data.frame(weight=c(30, 70, 0, 160, 10, 90, 0, 20, 0, 40, 0, 130, 90, 50, 20, 80, 40, 50, 0, 120))
wgt_2 <- data.frame(weight=c(0, 0, 0 , 10, 40, 70 , 200, 120, 50, 40, 30, 10, 10, 90, 0, 140, 10, 20, 30, 60, 70))
wgt_3 <- data.frame(weight=c(100, 60, 70, 20, 20, 10, 50, 0, 0, 30, 0, 80, 70, 40, 60, 140, 120, 30, 80, 10, 10))
wgt_4 <- data.frame(weight=c(200, 120, 0, 10, 40, 30, 50, 0, 0, 30, 40, 70, 80, 20, 10, 0, 180, 90, 10, 20, 0))
wgt_5 <- data.frame(weight=c(72, 29, 36, 34, 29, 34, 28, 39, 57, 0, 15, 46, 27, 22, 10, 34, 23, 48, 40, 0, 14))
wgt_6 <- data.frame(weight=c(27, 38, 37, 48, 47, 29, 35, 43, 0, 25, 22, 65, 16, 37, 20, 12, 19, 25, 29, 27, 47))
wgt_7 <- data.frame(weight=c(47, 35, 0, 43, 16, 52, 40, 18, 30, 27, 19, 29, 15, 41, 39, 35, 42, 48, 16, 14, 17))
wgt_8 <- data.frame(weight=c(13, 40, 52, 50, 53, 22, 40, 16, 55, 43, 9, 39, 18, 10, 26, 24, 0, 51, 19, 35, 6))
wgt_9 <- data.frame(weight=c(34, 49, 25, 29, 54, 28, 14, 0, 31, 27, 46, 34, 21, 45, 11, 38, 35, 22, 37, 20, 24))
wgt_10 <- data.frame(weight=c(46, 38, 14, 32, 54, 19, 45, 43, 33, 27, 15, 12, 20, 23, 0, 26, 27, 33, 31, 20, 29))
sum(wgt_1$weight)



invested_stock <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_1) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

## need to sum the individual population s before and after investment to get ESU stock abundance

# variance data from population
pop_var <- sapply(coho[2:22], var) 
clean_var <- data.frame(var=pop_var[-18]) # remove tahkenitch because removed from above

## calculate variance after investment
temp_variance <- invested_stock %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% #is variance already squared?
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)
  #mutate(port_var = var / s_baseline *(return_investment^2))

## portfolio mean (y axis)
portfolio_return <- sum(invested_stock$s_invest)

#variance_return <- sum(temp$port_var)
variance_return <- sum(temp$invest_var)
variance_return_parcent_change <- mean(temp$var_percent_change) #average of the percent change in variance across the ESU
variance_return_sum <- sum(temp$var_percent_change)


################
# Running our defined portfolio scenarios from above (will automate this)
################



## wgt_1
invested_stock_wgt1 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_1) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt1 <- invested_stock_wgt1 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_2
invested_stock_wgt2 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_2) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt2 <- invested_stock_wgt2 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_3
invested_stock_wgt3 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_3) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt3 <- invested_stock_wgt3 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_4
invested_stock_wgt4 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_4) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt4 <- invested_stock_wgt4 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_5
invested_stock_wgt5 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_5) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt5 <- invested_stock_wgt5 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_6
invested_stock_wgt6 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_6) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt6 <- invested_stock_wgt6 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_7
invested_stock_wgt7 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_7) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt7 <- invested_stock_wgt7 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_8
invested_stock_wgt8 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_8) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt8 <- invested_stock_wgt8 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_9
invested_stock_wgt9 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_9) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt9 <- invested_stock_wgt9 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

## wgt_10
invested_stock_wgt10 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_10) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt10 <- invested_stock_wgt10 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)

#############
# ESU Portfolio return and variance
#############

portfolio_invest_s <- invested_stock_wgt1 %>% 
  select(population, s_invest) %>% 
  rename("s_invest_wgt1" = "s_invest") %>% 
  cbind(s_invest_wgt_2=invested_stock_wgt2$s_invest,
        s_invest_wgt_3=invested_stock_wgt3$s_invest,
        s_invest_wgt_4=invested_stock_wgt4$s_invest,
        s_invest_wgt_5=invested_stock_wgt5$s_invest,
        s_invest_wgt_6=invested_stock_wgt6$s_invest,
        s_invest_wgt_7=invested_stock_wgt7$s_invest,
        s_invest_wgt_8=invested_stock_wgt8$s_invest,
        s_invest_wgt_9=invested_stock_wgt9$s_invest,
        s_invest_wgt_10=invested_stock_wgt10$s_invest) %>%
  adorn_totals("row") # new row called "Total" is the portfolio mean return from investment

portfolio_var <- variance_wgt1 %>% 
  select(population, invest_var) %>% 
  rename("invest_var_wgt1" = "invest_var") %>% 
  cbind(invest_var_wgt2 = variance_wgt2$invest_var,
        invest_var_wgt3 = variance_wgt3$invest_var,
        invest_var_wgt4 = variance_wgt4$invest_var,
        invest_var_wgt5 = variance_wgt5$invest_var,
        invest_var_wgt6 = variance_wgt6$invest_var,
        invest_var_wgt7 = variance_wgt7$invest_var,
        invest_var_wgt8 = variance_wgt8$invest_var,
        invest_var_wgt9 = variance_wgt9$invest_var,
        invest_var_wgt10 = variance_wgt10$invest_var) %>% 
  adorn_totals("row") # new row called "total" is the portfolio variance from investment
  
  

## Combine the sum return and variance for the ESU and plot







################

n=1000
nvar=20
set.seed(1)
rand<-matrix(0,nrow=n,ncol=nvar)
for(i in 1:n){
  pull=runif(nvar)
  rand[i,1:nvar]<-pull/sum(pull)
  
}


#############
# Solve allocation with MPT
#############



#############
## Visualizations
#############
ggplot(new_stock, aes(x=population, y=p_hat)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(new_stock, aes(x=population, y=c_hat)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(new_stock, aes(x=population, y=s_invest)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) #not working
.