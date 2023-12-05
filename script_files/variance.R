## Use Nathan and Tamma's overleaf as a guide for estimating variance


## Utility function - people get utility from salmon returns u(s)

## society's utility function:
## u(s) = r(s) -y*v(s) ## utility = metapopulation mean - how much we tolerate risk * metapopulation variance

## Variance is a maximization problem, can use the function: optim() (verify this)

# will be made up of three (maybe 4) equations
# 1) output of s_fun (s_fun to get s(w), which is s_invest or s after investment)
# 2) variance (diagonal from variance-covariance matrix)
# 3) covariance equation (this may need to be broken up into two steps for i and j

## One overall equation bringing together the 3 equations above and adding risk tolerance variable (y)




#### DO NOT RUN until we have 10 portfolios completed 








a<-expand_grid(x=seq(0,1,by=.05),y=seq(0,1,by=.05),z=seq(0,1,by=.05)) %>% 
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





