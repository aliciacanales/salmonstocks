library(tidyverse)
library(purrr)

#.......................... while loop process.........................



#.......................... prep the data.........................
## sort passability data for each dataframe from low to high



#..........................step 1: run 0 (one while loop running with one budget allocated).........................
# ###### working original -- while loop running with a single budget (most simplest form, establishing the basline to make sure it runs)
# ## once the iterative steps are running, we can comment this out - Working.
# used_b = 0 ## used budget
# i = 1 
# weight = .1 ## weight allocated to the population
# budget = 1000000 ## total budget for all of esu
# budget_allocated <- budget * weight ## budget allocated to this one population
# bpassage = data.frame(c(0,0,.5,1)) ## dummy df of passability to test functions on
# n = 5
# cost = array(50000, n) #barriers cost $50,000 to remove
# 
# ## create while loop
# while(used_b < budget_allocated) { #stop running if used_b is greater than budget_allocated
#   used_b = cost[1] + used_b
#   i = i + 1
#   print(i)
# }
# 
# index_choice = i-1 ## number of barriers to be improved for a given budget allocated to the population
# 
# bpassage[1:index_choice, ] <- 1 ## in the bpassage df, update the passability to 1 for rows from 1:index_choice
# ###### end above




#..........................step 2: Create df of allocated budget to each pop.........................
#### (purrr #1) create dataframe of the allocated budget by weight for each population
## output of this step is dataframe of budget allocations. columns = populations, rows = portfolios.

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
budget_allocated_list<-split(budget_allocated_df,seq(nrow(budget_allocated_df))) ## convert df to list (necessary for later inputs)



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
n = 4
cost = array(50000, n) #barriers cost $50,000 to remove

## create bpassage dataframe with two columns
bpassage = data.frame(c(0,0,.5,1)) %>% 
  mutate(portfolio_2 = c(0,.5,.5,.5)) %>% 
  rename(portfolio_1=1)

## run the while loop within a function and call it 'while_fcn'. Doing this so we can run the function through purrr with many budget allocations.
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


## run the 'index_choice_fcn' through purrr using the 'budget_grid_list'. index choice is the number of barriers that can be improved for a given budget, we will use this to update the 'bpassage' for each population to get 'bpassage_invest'
index_choice_df = map_df(.x=budget_allocated_list,~index_choice_fcn(.x)) ## QC output here
index_choice_list<-split(index_choice_df,seq(nrow(index_choice_df))) ## turn df into a list

# temp_fcn <- function(budget_allocated){
#   index_choice_2 <- (pmap_dbl(list(budget_allocated), while_fcn) - 1)
#   return(index_choice)
# }

# ## run the 'index_choice_fcn' through purrr using the 'budget_grid_list'. She runs!!!!!
# index_choice_df = map_df(.x=budget_allocated_list,~temp_fcn(.x)) ## confirm that the output matches what we would expect based on the money invested
# ## index choice is the number of barriers that can be improved for a given budget, we will use this to update the 'bpassage' for each population to get 'bpassage_invest'



### test first with a simple (fake) df first. Use this as a QC
budget_index_df = budget_df %>% # dummy budget dataframe from above ## this will need to be 'budget_allocated_df' but all in one column named 'money'
  mutate(index_choice = map_dbl(.x=budget_df$money,~while_fcn(.x))) ###oh my god it's working. I'm crying.


#..........................step 4: Improve bpassage dataframes using the index_choice_df created above.........................
##### (purrr #3) Run the while function inside purrr
##### Each row within the column 'index_choice' in the 'budget_index_df' needs to be across columns
##### output are the bpassage_invest dataframes (we will want to re-list these or maybe nest?)

## equation to improve passability
bpassage[0:index_choice, ] <- 1 ##need to do this by column

## create fake dataframe where each column is a population
passability_values_df = data.frame(
  port1 = c(0.5, 0.5, 0.5, 0.5),
  port2 = c(0.5, 0.5, 0.5, 0.5),
  port3 = c(0.5, 0.5, 0.5, 0.5),
  port4 = c(0.5, 0.5, 0.5, 0.5),
  port5 = c(0.5, 0.5, 0.5, 0.5),
  port6 = c(0.5, 0.5, 0.5, 0.5),
  port7 = c(0.5, 0.5, 0.5, 0.5),
  port8 = c(0.5, 0.5, 0.5, 0.5),
  port9 = c(0.5, 0.5, 0.5, 0.5),
  port10 = c(0.5, 0.5, 0.5, 0.5),
  port11 = c(0.5, 0.5, 0.5, 0.5),
  port12 = c(0.5, 0.5, 0.5, 0.5),
  port13 = c(0.5, 0.5, 0.5, 0.5),
  port14 = c(0.5, 0.5, 0.5, 0.5),
  port15 = c(0.5, 0.5, 0.5, 0.5),
  port16 = c(0.5, 0.5, 0.5, 0.5),
  port17 = c(0.5, 0.5, 0.5, 0.5),
  port18 = c(0.5, 0.5, 0.5, 0.5),
  port19 = c(0.5, 0.5, 0.5, 0.5),
  port20 = c(0.5, 0.5, 0.5, 0.5)
)
colnames(passability_values_df) <- names(abundance_data) #rename columns




# # automated below so commenting this out right now
# #### Run one row of index_choice_df through the for loop to test run it first #### this is working
# ## select the first row of index_choice_df
# one_index_choice_df <- index_choice_df[1, ]
# 
# ## for loop to iterate across columns
# for (col_name in names(one_index_choice_df)) {
#   # Get the number of rows to change for the current column
#   rows_to_change <- one_index_choice_df[[col_name]]
# 
#   # Update the specified number of rows in test_passability_values
#   passability_values_df[0:rows_to_change, col_name] <- 1
# } # my god it is working!!!!
# 
# print(passability_values_df) ## bless
# 
# #### Within this portfolio, take the product of the new passability values for each population and multiply by number of barriers in that population
# bpassage_invest <- apply(passability_values_df, 2, prod) ## create a new dataframe and take the product of each column
# bpassage_invest <- bpassage_invest * nrow(passability_values_df) ## multiply the product by the number of rows in the column (ie barriers)
# bpassage_invest_df <- data.frame(bpassage_invest) ## Create a new dataframe with a single row


one_index_choice_df <- index_choice_df[1, ]

### can we combine the two above and run to map? Heck yes
bpassage_invest_fcn <- function(index_choice) {

  for (col_name in names(one_index_choice_df)) {
    # Get the number of rows to change for the current column
    rows_to_change <- one_index_choice_df[[col_name]]
    
    # Update the specified number of rows in test_passability_values
    passability_values_df[0:rows_to_change, col_name] <- 1
  }
  
  bpassage_invest <- apply(passability_values_df, 2, prod) ## create a new dataframe and take the product of each column
  bpassage_invest <- bpassage_invest * nrow(passability_values_df) ## multiply the product by the number of rows in the column (ie barriers)
  bpassage_invest_output <- data.frame(t(bpassage_invest))

  return(bpassage_invest_output)
} # output is new bpassage score for each population for this portfolio

bpassage_invest_df <- t(bpassage_invest_fcn(one_index_choice_df)) # working - bpassage_invest for each population for one portfolio

### we need to run this for each row, becuase each row represents a dataframe
## apply the next row in the list
temp_fcn <- function(index_choice) {
  bpassage_invest_output <- pmap_dbl(list(index_choice), bpassage_invest_fcn)
  return(bpassage_invest_output)
}

temp_df <- map_df(index_choice_list, ~temp_fcn(.x))


### re-list values


################################################ left off here
#mutate a new column for each portfolio
test_fcn <- function(index_choice){
  bpassage_invest <- pmap_dbl(list(index_choice), bpassage_invest_fcn)
  return(bpassage_invest)
}
test_fcn_df = map_df(.x=one_index_choice_df,~test_fcn(.x))



temp_fcn <- function(index_choice) {
  bpassage_invest_output <- pmap_dbl(list(index_choice), bpassage_invest_fcn)
  return(bpassage_invest_output)
}

temp_df2 <- map_df(index_choice_list, ~temp_fcn(.x))





bpassage_invest <- apply(passability_values_df, 2, prod) ## create a new dataframe and take the product of each column
bpassage_invest <- bpassage_invest * nrow(passability_values_df) ## multiply the product by the number of rows in the column (ie barriers)
bpassage_invest_df <- data.frame(bpassage_invest) ## Create a new dataframe with a single row








output_bpassage_fcn <- function(index_choice){
  output <- pmap_dbl(list(index_choice),bpassage_invest_fcn)
  return(output)
}

temp_fcn <- function(index_choice){
  temp_output <- pmap_dbl(list(index_choice), bpassage_invest_fcn)
  return(temp_output)
}

temp_df = map_df(.x=index_choice_list,~temp_fcn(.x))




temp2 = map_df(.x=index_choice_list,~bpassage_invest_fcn(.x)) # not working



result <- index_choice_list %>%
  pmap_df(~ bpassage_invest_fcn(.x))





index_choice_df = map_df(.x=budget_allocated_list,~index_choice_fcn(.x))


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

## run the 'index_choice_fcn' through purrr using the 'budget_grid_list'. index choice is the number of barriers that can be improved for a given budget, we will use this to update the 'bpassage' for each population to get 'bpassage_invest'
index_choice_df = map_df(.x=budget_allocated_list,~index_choice_fcn(.x))








## data viz: Gabrielle laMarr LeMee la times











#
# ### test run on a small dataframe
# test_passability_values = data.frame(
#   pop1 = c(0.5, 0.5, 0.5, 0.5),
#   pop2 = c(0.5, 0.5, 0.5, 0.5),
#   pop3 = c(0.5, 0.5, 0.5, 0.5))
# 
# test_index_choice = data.frame(
#   pop1 = 1,
#   pop2 = 2,
#   pop3 = 0)
# 
# test_passability_values[1:test_index_choice, ] <- 1
# 
# for (col_name in names(test_index_choice)) {
#   # Get the number of rows to change for the current column
#   rows_to_change <- test_index_choice[[col_name]]
#   
#   # Update the specified number of rows in test_passability_values
#   test_passability_values[0:rows_to_change, col_name] <- 1
# }
# 
# print(test_passability_values) # working





# ## create function to run result_budget_df$index_choice
# b_passage_index_fcn <- function(index_choice){
#   bpassage[1:index_choice, ] <- 1
#   return(bpassage)
# } 






#..........................run 4.........................
##### (purrr #4 $ hopefully final) calculate new bpassage for the population from the passability scores after investment






