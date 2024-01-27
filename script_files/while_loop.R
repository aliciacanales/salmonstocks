library(tidyverse)
library(purrr)

#..........................while loop.........................



## we will have an df for each population, in the form of a list?
## need to bring in weights df for each population, allocate them into the budgetm and run it through

## sort passability for each dataframe from low to high

#..........................run 1.........................
###### working original -- while loop running with one single budget (most simplest form)
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
###### end above


#..........................run 2.........................
#### (purrr #1) create dataframe of the allocated budget by weight for each population
#### output of this step is dataframe of budget allocations
#### columns = populations, rows = portfolios

used_b = 0
i = 1
budget_df = data.frame(c(100000,150000)) %>% 
  rename(money=1)
  
## budget_allocated <- budget * weight ## turn this into a function to run weights through it
budget_allocated_fcn <- function(budget,weight){
  weight=weight %>% unlist()
  budget_allocated <- budget * weight
  return(budget_allocated)
}

budget_allocated_df = map_df(.x=grid_list,~budget_allocated_fcn(budget,.x)) ## create dataframe 


#..........................run 3.........................
##### (purrr #2) Run the while function inside purrr using the budget_df made above
##### output of this step is the index_choice for each budget

##### create bpassage dataframe with two columns
bpassage = data.frame(c(0,0,.5,1)) %>% 
  mutate(portfolio_2 = c(0,.5,.5,.5)) %>% 
  rename(portfolio_1=1)


while_fcn <- function(budget_allocated) {
  i=1

while(used_b < budget_allocated) { #stop running if used_b is greater than budget_allocated
  used_b = cost[1] + used_b
  i = i + 1
  #print(i)
  index_choice = i-1
}

return(index_choice)
}

budget_index_df = budget_df %>% # dummy budget dataframe from above ## this will need to be 'budget_allocated_df' but all in one column named 'money'
  mutate(index_choice = map_dbl(.x=money,~while_fcn(.x))) ###oh my god it's working. I'm crying.

#..........................run 4.........................
##### (purrr #3) Run the while function inside purrr
##### Each row within the column 'index_choice' in the 'budget_index_df' needs to be across columns

bpassage = data.frame(c(0,0,.5,1)) %>%## clean bpassage dataframe. two columns
  mutate(portfolio_2 = c(0,.5,.5,.5)) %>% 
  rename(portfolio_1=1)

bpassage[1:index_choice, ] <- 1 ##need to do this by column

## create function to run result_budget_df$index_choice
b_passage_index_fcn <- function(index_choice){
  bpassage[1:index_choice, ] <- 1
  return(bpassage)
}

result_budget_df_df = bpassage %>% 
  mutate(passage = map(.x=result_budget_df$index_choice,~b_passage_index_fcn(.x)))











