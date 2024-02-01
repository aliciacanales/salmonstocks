y <- solve.QP(dmat, dvec_matrix, amat_matrix, bvec_matrix, meq = 2)
y

sum(y$solution) #1 var_star = .549
#mean_list <- as.numeric(seq(min(pop_return_mean_df),max(pop_return_mean_df), length.out = 100)) #this line is not running, so using an alternative mean list (next line)
mean_list <- seq(0.1,2.5,length.out=1000)
min(pop_return_mean_df)
max(pop_return_mean_df)
#high returns low variance for largest
## so this min is a negative number so I should make it .0001 right?

##Clean up code and run it as a loop to try to create graph
##Change bvec with new Rbar
##Table of all Rbar and corresponding $values and plot

mpt_fun <- function(x) {
  bvec <- c(x, 1,rep(0, 21))
  y = solve.QP(dmat, dvec_matrix, amat_matrix, bvec, meq = 2)
  
  return(list(solution = y$solution, var = y$value))
}


front = map(mean_list, ~mpt_fun(.x)) %>%
  transpose()

pop_return_mean_df_t <- t(pop_return_mean_df) #transpose the df so that we can rename each of the pop columns

## contains the weights for each stream (population)
solution <- as.data.frame(pluck(front$solution)) %>%
  rename_with(~paste0("Run",1:length(front$solution))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename_with(~paste0('w', 1:length(pop_return_mean_df_t))) %>% #This line was not running because we need to transpose the pop_return_mean_df so that the length would fit. I did this in a quick way, but should probs clean it up later
  mutate(var = t(as.data.frame(pluck(front$var)))[,1])

var_fcn <- function(weights,dmat){
  temp = as.matrix(dmat) %*% as.matrix(weights)
  out = t(as.matrix(weights)) %*% temp
  return(out)
}

var_df_fcn<-function(weights,cov){
  temp=as.matrix(dmat) %*% t(as.matrix(weights))
  out=as.matrix(weights)%*%temp
  return(out)
}

mu_fcn<-function(weights,means){
  out=weights %*% means
  return(out)
}

#Notice the var_res column is simply double the var column which came from solve.QP
var_out<-solution %>% 
  nest(weights=w1:w21) %>% #I changed this from "w1:w3" to "w1:w21" and then it ran. Was that okay to do?
  mutate(var_res=var*2) %>% 
  mutate(port_mean = mean_list)