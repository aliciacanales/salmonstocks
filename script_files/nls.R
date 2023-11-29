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

## Calculate change in p_hat which will be put into "big equation" (This means: P is a function of Alpha(passage) * w(i) + p_hat (from nls))
p_hat_fun <- function(p_hat,p_change, weight){ 
  p <- ((weight) * p_change) + p_hat # should weight be "w/10" so that we multiply by the number of investments and not the dollar amount? - OS
}

## Calculate change in c_hat which will be put into "big equation" (This means: c is a function of Beta(passage) * w(i) + c_hat (from nls))
c_hat_fun <- function(c_hat, c_change, weight){ 
  c <- ((weight) * c_change) + c_hat
}

## bringing out the coefficients into separate columns and applying a $10 investment which will have a .01 increase
new_stock <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>% 
  mutate(weight = 10,
         p_change = .01,
         c_change = .01,
         delta_p = map(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap(list(p_hat,c_hat),s_fun), #calculate s before investment to compare with s after investment
         s_invest = pmap(c(delta_p,delta_c),s_fun)) # calculate s after investment using new p and c

## calculate difference in investment to visualize
new_stock <- new_stock %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline)


# new_stock2 <- pmap(new_stock, ~s_fun(..1, ..3, ..4, ..5))



## Create a vector of weight allocation options
 w <- seq(10,1000, by=10)
w

wgt <- runif(n=)
#test


## Need to come up with budgets for the entire ESU and randomly assign weights to populations for a given budget
## Come up with all possible options.
## Need to call in this dataframe for weight in p_hat_fun and c_hat_fun

## Creating a dataframe of budgets and weights
budget <- data.frame(matrix(10,ncol=21)) #will need to do this for many budgets, but starting with this. Will want to nest budgets.
colnames(budget) <- c('1':'21')
rownames(budget) <- "budget"

# randomize weights, but need to fix this to a given budget
weight_matrix <- as.data.frame(matrix(round(runif(n=21*21, min=0, max=1), 0), nrow=21))
colnames(weight_matrix) <- c('1':'21') # Or add row and sum weight

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
