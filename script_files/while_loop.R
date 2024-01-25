

#..........................while loop.........................
## test loop
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
}
## end test loop


## we will have an df for each population, in the form of a list?
## need to bring in weights df for each population, allocate them into the budgetm and run it through

## sort passability for each dataframe from low to high


used_b = 0
i = 1
weight = .1
budget = 1000000
budget_allocated <- budget * weight
bpassage = data.frame(c(0,0,.5,1))

n = 5
cost = array(50000, n) #barriers cost $50,000 to remove

while(used_b < budget_allocated) { #stop running if used_b is greater than budget_allocated
  used_b = cost[1] + used_b
  i = i + 1
  print(i)
}

index_choice = i-1

bpassage[1:index_choice, ] <- 1

















