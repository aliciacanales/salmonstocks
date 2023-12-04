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

# calculate_return_investment <- function(p, c, alpha, beta, weight){
#   y = (((p*alpha(weight))-1)*(c*beta(weight)))
#   return(y)
# }

## Big Function - can we instead use the simpler function below (s_fun_new) to calculate s? - OS
# s_fun <- function(population, weight, p_change, c_change){
#   eq <- (p_hat(weight, p_change) - 1) * c_hat(weight,c_change)
#   return(eq)
# }

s_fun <- function(delta_p, delta_c){
  s_invest <- (delta_p-1)*delta_c
  return(s_invest)
}

## Calculate change in p_hat which will be put into "big equation"
p_hat_fun <- function(p_hat,p_change, weight){ 
  p <- p_hat * (1 + p_change * weight) # don't need to divide weight by 10 because we already accounted for it with p_change
}

## Calculate change in c_hat which will be put into "big equation"
c_hat_fun <- function(c_hat, c_change, weight){ 
  c <- c_hat * (1 + c_change * weight)
}

## create 10 Portfolios with weights that sum to 100
wgt_1 <- data.frame(weight=c(3,7,0,16,1,9,0,2,0,4,0,13,9,5,2,8,4,5,0,12))
wgt_2 <- data.frame(weight=c(8,1,2,7,4,0,2,5,12,1,14,3,7,5,8,3,1,8,3,6))
wgt_3 <- data.frame(weight=c(0,9,4,2,7,8,2,3,0,4,3,5,8,14,6,3,4,5,10,3))
wgt_4 <- data.frame(weight=c(4,5,3,11,4,1,6,6,7,2,9,8,1,0,6,7,2,9,5,4))
wgt_5 <- data.frame(weight=c(7,8,4,0,3,4,11,8,2,1,6,3,9,3,7,5,1,3,8,7))
wgt_6 <- data.frame(weight=c(10,2,5,1,7,4,9,2,3,0,2,14,3,10,2,3,5,1,9,8))
wgt_7 <- data.frame(weight=c(5,6,3,4,8,7,5,5,4,6,3,4,8,7,5,4,0,9,5,2))
wgt_8 <- data.frame(weight=c(1,2,12,6,3,9,4,2,8,0,3,0,7,7,5,11,3,6,3,8))
wgt_9 <- data.frame(weight=c(3,7,1,16,4,2,7,8,8,6,0,3,8,2,11,1,1,5,0,7))
wgt_10 <- data.frame(weight=c(9,6,3,5,1,3,8,0,9,13,2,1,7,2,1,12,8,3,2,5))
sum(wgt_10$weight)


## bringing out the coefficients into separate columns and applying a $10 investment which will have a .01 increase
new_stock <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), #calculate s for each population before investment to compare with s after investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

## need to sum the individual population s before and after investment to get ESU stock abundance

pop_var <- sapply(coho[2:22], var) 

clean_var <- data.frame(var=pop_var[-18])


## Varaince
temp <- new_stock %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_difference = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)
  #mutate(port_var = var / s_baseline *(return_investment^2))

## portfolio mean (y axis)
portfolio_return <- sum(new_stock$s_invest)
portfolio_return

variance_return <- sum(temp$port_var)









################
# Adding budgets and weights
################

## Need to come up with budgets for the entire ESU and randomly assign weights to populations for a given budget
## Start with a budget of $10, and make 21 portfolios with weight options
## Call in this dataframe for weight in p_hat_fun and c_hat_fun

# weights allocations for a given portfolio budget
# nest()
# portfolio budgets


## create a dataframe of all portfolio weight allocations for a budget of $10
# weight <- diag(21)
# colnames(weight) <- c('1':'21')

# weight <- weight %>% 
#   nest()



n=1000
nvar=20
set.seed(1)
rand<-matrix(0,nrow=n,ncol=nvar)
for(i in 1:n){
  pull=runif(nvar)
  rand[i,1:nvar]<-pull/sum(pull)
  
}


## Creating a dataframe of budgets and weights
budget <- data.frame(matrix(10,ncol=21)) # will need to do this for many budgets, but starting with 10. Will want to nest budgets and portfolio weights.
colnames(budget) <- c('1':'21')
rownames(budget) <- "budget"


## randomize portfolio weights, but need to fix this to a given budget
weight_matrix <- as.data.frame(matrix(round(runif(n=21*21, min=0, max=10), 0), nrow=21))
colnames(weight_matrix) <- c('1':'21') # Or add row and sum weight

weight_matrix <- weight_matrix %>% 
  nest()

budget_weight <- rbind(budget,weight_matrix)


#############
# Solve allocation with MPT
#############



#############
## Visualizations
ggplot(new_stock, aes(x=population, y=p_hat)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(new_stock, aes(x=population, y=c_hat)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(new_stock, aes(x=population, y=s_invest)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) #not working
