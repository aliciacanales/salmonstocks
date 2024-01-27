library(tidyverse)
library(purrr)

#.......................... while loop process.........................



#.......................... prep the data.........................
## sort passability data for each dataframe from low to high

#..........................step 1: run 0 (one while loop running with one budget allocated).........................
###### working original -- while loop running with a single budget (most simplest form, establishing the basline to make sure it runs)
used_b = 0 ## used budget
i = 1 
weight = .1 ## weight allocated to the population
budget = 1000000 ## total budget for all of esu
budget_allocated <- budget * weight ## budget allocated to this one population
bpassage = data.frame(c(0,0,.5,1)) ## dummy df of passability to test functions on
n = 5
cost = array(50000, n) #barriers cost $50,000 to remove

## create while loop
while(used_b < budget_allocated) { #stop running if used_b is greater than budget_allocated
  used_b = cost[1] + used_b
  i = i + 1
  print(i)
}

index_choice = i-1 ## number of barriers to be improved for a given budget allocated to the population

bpassage[1:index_choice, ] <- 1 ## in the bpassage df, update the passability to 1 for rows from 1:index_choice
###### end above




#..........................step 2: Create df of allocated budget to each pop.........................
#### (purrr #1) create dataframe of the allocated budget by weight for each population
## output of this step is dataframe of budget allocations
##columns = populations, rows = portfolios

used_b = 0
i = 1
budget_df = data.frame(c(100000,150000)) %>% ## one column, two rows
  rename(money=1)
bpassage = data.frame(c(0,0,.5,1)) ## bpassage to start with without improvement
  
## function to determine the budget allocated by weight allocation
budget_allocated_fcn <- function(budget,weight){
  weight=weight %>% unlist()
  budget_allocated <- budget * weight
  return(budget_allocated)
}

budget_allocated_df = map_df(.x=grid_list,~budget_allocated_fcn(budget,.x)) ## create budget_allocated_df by inputting the grid_list of weights into the budget_allocated_fcn (output is same format as weights og grid_list)
budget_allocated_list<-split(budget_allocated_df,seq(nrow(budget_allocated_df))) ## make the budget_allocated_df into a gridlist to pass into later functions.



#..........................step 3: run 1 (run the while_loop_fcn using MANY budget allocations).........................
##### (purrr #2) Use purrr to run the list of budgets allocated through the 'while_fcn' 
## output of this step is the index_choice for each budget

## start with fresh variables (the same use above, just a refresh)
used_b = 0 ## used budget
i = 1 
weight = .1 ## weight allocated to the population
budget = 1000000 ## total budget for all of esu
budget_allocated <- budget * weight ## budget allocated to this one population
bpassage = data.frame(c(0,0,.5,1)) ## dummy df of passability to test functions on
n = 5
cost = array(50000, n) #barriers cost $50,000 to remove

## create bpassage dataframe with two columns
bpassage = data.frame(c(0,0,.5,1)) %>% 
  mutate(portfolio_2 = c(0,.5,.5,.5)) %>% 
  rename(portfolio_1=1)

## run the while loop within a function and call it 'while_fcn'. Doing this so we can run the function through purrr.
while_fcn <- function(budget_allocated) {
  i = 1

while(used_b < budget_allocated) { #stop running if used_b is greater than budget_allocated
  used_b = cost[1] + used_b
  i = i + 1
  index_choice = i-1
}

return(index_choice)
}

## Create 'index_choice_fcn' so that we can map the 'budget_grid_list' through it
index_choice_fcn <- function(budget_allocated){
  index_choice <- (pmap_dbl(list(budget_allocated),while_fcn) - 1)
  return(index_choice)
}

## run the 'index_choice_fcn' through purrr using the 'budget_grid_list'. She runs!!!!!
index_choice_df = map_df(.x=budget_allocated_list,~temp_fcn(.x)) ## confirm that the output matches what we would expect based on the money invested
## index choice is the number of barriers that can be improved for a given budget, we will use this to update the 'bpassage' for each population to get 'bpassage_invest'


### test first with a simple (fake) df first. Use this as a QC
# budget_index_df = budget_df %>% # dummy budget dataframe from above ## this will need to be 'budget_allocated_df' but all in one column named 'money'
#   mutate(index_choice = map_dbl(.x=money,~while_fcn(.x))) ###oh my god it's working. I'm crying.


#..........................step 4: Improve bpassage dataframes using the index_choice_df created above.........................
##### (purrr #3) Run the while function inside purrr
##### Each row within the column 'index_choice' in the 'budget_index_df' needs to be across columns
##### output are the new passage dataframes (we will want to re-list these or maybe nest?)
## not working becuase don't know how to allocate purrr properly

bpassage = data.frame(c(0,0,.5,1)) %>%## clean bpassage dataframe. two columns
  #mutate(portfolio_2 = c(0,.5,.5,.5)) %>% 
  rename(portfolio_1=1)

bpassage[1:index_choice, ] <- 1 ##need to do this by column

## create function to run result_budget_df$index_choice
b_passage_index_fcn <- function(index_choice){
  bpassage[1:index_choice, ] <- 1
  return(bpassage)
}

result_budget_df_df = bpassage %>% 
  mutate(passage = map_df(.x=budget_index_df$index_choice,~b_passage_index_fcn(.x))) ## its working ahhhh


## list or nest dataframes (we will have 20 total dataframes of bpassage)
## alternative and probably more efficient is to calculate bpassage_invest for each population and portfolio. Would look like original df that was inputted


#..........................run 4.........................
##### (purrr #4 $ hopefully final) calculate new bpassage for the population from the passability scores after investment








