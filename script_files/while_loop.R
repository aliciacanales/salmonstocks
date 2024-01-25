

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
bpassage = c(0,0,.5,1)

n = 5
cost = array(50000, n)

while(used_b < budget_allocated) { #stop running if used_b is greater than budget_allocated
  used_b = cost[1] + used_b
  i = i +1
  print(i)
}

index_choice = i-1
bpassage[index_choice]



rows_to_replace <- index_choice

# Use an if-else statement to replace values in the specified rows
for (row_index in rows_to_replace) {
  if (row_index <= nrow(test_df)) {
    test_df[row_index, ] <- ifelse(!is.na(test_df[row_index, ]), 1, NA)
  } else {
    warning("Row index ", row_index, " exceeds the number of rows in the dataframe.")
  }
}


test_df<- data.frame(weight=c(8,1,2,7,4,0,2,5,12,1,14,3,7,5,8,3,1,8,3,6))


test_df[index_choice, ] <- lapply(df[index_choice, ], function(x) ifelse(!is.na(x), 1, NA))

# replace value to 1 for i columns
















