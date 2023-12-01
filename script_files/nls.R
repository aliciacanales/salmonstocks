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


wgt <- data.frame(c(10,20,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
## bringing out the coefficients into separate columns and applying a $10 investment which will have a .01 increase
new_stock <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>% 
  mutate(weight = 10,
         p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap(list(p_hat,c_hat),s_fun), #calculate s for each population before investment to compare with s after investment
         s_invest = pmap(c(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

## need to sum the individual population s before and after investment to get ESU stock abundance

## calculate difference in investment to visualize
new_stock <- new_stock %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline)


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


## Creating a dataframe of budgets and weights
budget <- data.frame(matrix(10,ncol=21)) # will need to do this for many budgets, but starting with 10. Will want to nest budgets and portfolio weights.
colnames(budget) <- c('1':'21')
rownames(budget) <- "budget"


## randomize portfolio weights, but need to fix this to a given budget
weight_matrix <- as.data.frame(matrix(round(runif(n=21*21, min=0, max=1), 0), nrow=21))
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
