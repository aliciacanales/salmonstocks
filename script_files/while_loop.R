library(tidyverse)
library(purrr)

#.......................... while loop process.........................



# ..........................step 1: run 0 (one while loop running with one budget allocated).........................

#..........................step 2: Create df of allocated budget to each pop.........................
## Create dataframe of the allocated budget by weight for each population. columns = populations, rows = portfolios.

budget = 1300000
  
## function to determine the budget allocated using weights
budget_allocated_fcn <- function(budget,weight){
  weight=weight %>% unlist()
  budget_allocated <- budget * weight
  return(budget_allocated)
}

budget_allocated_df = map_df(.x=grid_list,~budget_allocated_fcn(budget,.x)) 
budget_allocated_list<-split(budget_allocated_df,seq(nrow(budget_allocated_df))) ## convert df to list (necessary for later inputs)



#..........................step 3: run 1 (run the while_loop_fcn using MANY budget allocations).........................
## Use purrr to run the list of budgets allocated through the 'while_fcn'. Output of this step is the index_choice for each budget


## Reset the variables used above
used_b = 0 # used budget
i = 1 
weight = .1 
#budget = 2300000 # total budget for all of esu. I used the 23,000,000 scenario that tamma suggested
budget_allocated <- budget * weight # budget allocated to this one population
bpassage = data.frame(c(.5,.5,.5,.5)) # dummy df of passability to test functions on
n = 5
cost = array(108847, n) #barriers cost $108847 (median from data)

## run the while loop within a function and call it 'while_fcn'. Doing this so we can run the function through purrr with many budget allocations.
while_fcn <- function(budget_allocated) {
  i = 1
  used_b=0
    while(used_b <= budget_allocated) { #stop running if used_b is greater than budget_allocated
      used_b = cost[1] + used_b
      i = i + 1
    }
  index_choice=i-1

return(index_choice)
}
  

## Create 'index_choice_fcn' to map the 'budget_grid_list' through it
index_choice_fcn <- function(budget_allocated){
  index_choice <- (pmap_dbl(list(budget_allocated),while_fcn) - 1)
  return(index_choice)
}


## run the 'index_choice_fcn' through purrr using the 'budget_grid_list'. index choice is the number of barriers that can be improved for a given budget, we will use this to update the 'bpassage' for each population to get 'bpassage_invest'
index_choice_df = map_df(.x=budget_allocated_list,~index_choice_fcn(.x))
index_choice_list<-split(index_choice_df,seq(nrow(index_choice_df)))


### test first with a simple (fake) df first. Use this as a QC
# budget_index_df = budget_df %>% # dummy budget dataframe from above ## this will need to be 'budget_allocated_df' but all in one column named 'money'
#   mutate(index_choice = map_dbl(.x=money,~while_fcn(.x))) ###oh my god it's working. I'm crying.



#..........................step 4: Improve bpassage dataframes using the index_choice_df created above.........................
## Run 'while_fcn' inside purrr. Each row within the column 'index_choice' in the 'budget_index_df' needs to be across columns

# #### Within this portfolio, take the product of the new passability values for each population and multiply by number of barriers in that population
# bpassage_invest <- apply(passability_values_df, 2, prod) ## create a new dataframe and take the product of each column
# bpassage_invest <- bpassage_invest * nrow(passability_values_df) ## multiply the product by the number of rows in the column (ie barriers)
# bpassage_invest_df <- data.frame(bpassage_invest) ## Create a new dataframe with a single row
# 
# print(bpassage_invest_df)


# passability_test <- passability_values_df
# 
# 
one_index_choice_df <- index_choice_df[1, ] # select the first row to test

# new function
bpassage_invest_fcn <- function(index_choice) {
  index_choice = index_choice %>% unlist()
  
  passability_test <- passability_values_df
  k=1

  for (col_name in names(index_choice_df)) {
    
    rows_to_change <- index_choice[k] # Get the number of rows to change for the current column
    passability_test[0:rows_to_change, col_name] <- 1 
    
    k<-k+1# Update the specified number of rows in passability_values_df
  }
  
  k=1
  bpassage_invest <- apply(passability_test, 2, prod) # product of passability values
  
  for (col_name in names(pass_values_na)) { #multiple by the number of barriers in that population
    
    nrow <- length(na.omit(pass_values_na[[col_name]]))
    
    if (k ==which(names(pass_values_na) == col_name)) {
      bpassage_invest[k] <- bpassage_invest[k] * nrow
    }
    
    k<-k+1
  }


  return(bpassage_invest)
} # output is new bpassage score for each population for this portfolio



bpassage_invest_df <- as.data.frame(bpassage_invest_fcn(one_index_choice_df)) # working - bpassage_invest for each population for one portfolio

# ### we need to run this for each row, because each row represents a dataframe
# ## apply the next row in the list
# temp_fcn <- function(index_choice) {
#   bpassage_invest_output <- pmap_dbl(list(index_choice), bpassage_invest_fcn)
#   return(bpassage_invest_output)
# }

# temp_df <- map_df(index_choice_list, ~bpassage_invest_fcn(.x))
