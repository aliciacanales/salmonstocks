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