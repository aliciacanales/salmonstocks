
library(tidyverse)
library(purrr)


#.......................... combinging while_loop into one.........................

## 204045.3 <- mean
## 108847.2 <- median
## budget = 3500000 and 23 mil. or find something else
budget = 1300000

## functions being used:
# budget_allocated_fcn()
# while_fcn()
# bpassage_invest_fcn()



budget_allocated_fcn <- function(budget,weight){
  weight=weight %>% unlist()
  budget_allocated <- budget * weight
  return(budget_allocated)
}


test_max_fcn <- function(weight){
  weight=weight %>% unlist()
  output1 <- pmap_dbl(list(budget, weight),budget_allocated_fcn) 
  output2 <- (pmap_dbl(list(output1),while_fcn)-1) # check to see if this is being transformed
  bpassage_invest <- bpassage_invest_fcn(output2)  #map_dbl(list(output2),bpassage_invest_fcn) # not working
  c_invest <- c_invest_fcn(z_c_df$z, bpassage_invest)
  p_invest <- p_invest_fcn(z_p_df$z, bpassage_invest)
  s_invest <- ((p_invest - 1) * c_invest)
  s_baseline <- ((z_p_df$p_hat - 1) * z_c_df$c_hat)
  esu_returns_invest <- sum(s_invest)
  esu_returns_baseline <- sum(s_baseline)
  
  
  var <- sapply(coho[2:22], var)
  var_rm<-var[-18]
  cov <- sapply(coho[2:22], cov)
  cov_rm<-cov[-18]
  
  var_invest <- var_rm * (s_invest^2)
  var_baseline <- var_rm * (s_baseline^2)
  esu_var_invest <- sum(var_invest,cov_rm)
  esu_var_baseline <- sum(var_baseline)
  
  
  #return(s_invest) # to look at single dataframe
  return(round(data.frame(esu_returns_invest, esu_returns_baseline, esu_var_invest, esu_var_baseline),3))
}

test = map_df(.x=grid_list,~test_max_fcn(.x)) %>% 
  arrange(esu_returns_invest) # order by returns from investment








#...................................... plots ......................................
library(ggalt)

# baseline esu returns = 187118.2
# baseline esu variance = 3.141711e+17 (this will change with updated variance calculation)
baseline_point <- data.frame(x = 3.141711e+17, y = 187118.2)

# remove outliers to plot (is this okay to do?)
temp <- test[-c(1398:1350), ]

# portfolios and efficiency frontier
ggplot(temp, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2) + 
  # geom_curve(x = 3.521570e+17, y = 205623.0,
  # xend = 3.892000e+17, yend = 211781.8,
  # colour = 'red', curvature = -.3) +
  geom_smooth(method = "gam", se = FALSE, color = 'red2') +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 3) +
  geom_text(x = 7.541711e+18, y = 187118.2, label = "<----- Baseline Portfolio", size = 5) +
  labs(x = 'Variance', y = 'ESU Abundance') +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none") + 
  theme_minimal()





# wrangling
optimal_portfolio_1 <- budget_allocated_df[1, ] %>% # this is random, just using for framework for now
  pivot_longer(cols = 1:20,
               names_to = 'population',
               values_to = 'budget_allocated')

# lollipop plot 1
optimal_portfolio_1 %>% 
  ggplot(aes(x = fct_reorder(population, budget_allocated), #fct_reorder lets us set the order of the first value, by the second value ($ invested)
             y = budget_allocated)) +
  ggalt::geom_lollipop() +
  labs(x = "Population", y = "Budget Allocated (USD)") +
  scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
  # gghighlight::gghighlight(population == "tillamook") + # if we want to emphasize a single population
  coord_flip() +
  theme_minimal()







##################################### We need to clean this up when we get the chance ############################################

test_max_fcn <- function(weight){
  weight=weight %>% unlist()
  output1 <- pmap_dbl(list(weight), budget_allocated_fcn)
  return(output1)
}

test = map_df(.x=grid_list,~test_max_fcn(.x))


test = pmap_dbl(list(grid_list,test_max_fcn))




combined_function <- function(input) {
  result1 <- square(input)
  result2 <- double(result1)
  return(result2)
}












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
  
  while(used_b <= budget_allocated) { #stop running if used_b is greater than budget_allocated
    used_b = cost[1] + used_b
    i = i + 1
    index_choice = i-1
  }
  
  return(index_choice)
}

## Create 'index_choice_fcn' so that we can map the 'budget_grid_list' through it
index_choice_fcn <- function(budget_allocated){
  index_choice_2 <- (pmap_dbl(list(budget_allocated),while_fcn) -1)
  return(index_choice_2)
}

## run the 'index_choice_fcn' through purrr using the 'budget_grid_list'. She runs!!!!!
index_choice_df = map_df(.x=budget_allocated_list,~index_choice_fcn(.x)) ## confirm that the output matches what we would expect based on the money invested
## index choice is the number of barriers that can be improved for a given budget, we will use this to update the 'bpassage' for each population to get 'bpassage_invest'
index_choice_list<-split(index_choice_df,seq(nrow(index_choice_df))) ## turn it into a list

### test first with a simple (fake) df first. Use this as a QC
# budget_index_df = budget_df %>% # dummy budget dataframe from above ## this will need to be 'budget_allocated_df' but all in one column named 'money'
#   mutate(index_choice = map_dbl(.x=money,~while_fcn(.x))) ###oh my god it's working. I'm crying.


#..........................step 4: Improve bpassage dataframes using the index_choice_df created above.........................
##### (purrr #3) Update bpassage based on the index_choice
##### Each row within the column 'index_choice' in the 'budget_index_df' needs to be across columns
##### output are the new passage dataframes (we will want to re-list these or maybe nest?)
## not working becuase don't know how to allocate purrr properly


## begin with the dataframes needed
# bpassage dataframe where every column is a different population and there are 6 barriers in each stream
passability_values_df = data.frame(
  port1 = c(0.5, 0.5, 0.5, 0.5),
  port2 = c(0.5, 0.5, 0.5, 0.5),
  port3 = c(0.5, 0.5, 0.5, 0.5),
  port4 = c(0.5, 0.5, 0.5, 0.5),
  port5 = c(0.5, 0.5, 0.5, 0.5),
  port6 = c(0.5, 0.5, 0.5, 0.5),
  port7 = c(0.5, 0.5, 0.5, 0.5),
  port8 = c(0, 0, 0.5, 0.5),
  port9 = c(0, 1, 0.5, 0.5),
  port10 = c(0, 0, 0.5, 1),
  port11 = c(0, 0.5, 0.5, 0.5),
  port12 = c(0, 0.5, 0.5, 0.5),
  port13 = c(0, 0.5, 0.5, 0.5),
  port14 = c(0, 0.5, 0.5, 0.5),
  port15 = c(0, 0.5, 0.5, 0.5),
  port16 = c(0, 0.5, 0.5, 0.5),
  port17 = c(0, 0.5, 0.5, 0.5),
  port18 = c(0, 0.5, 0.5, 0.5),
  port19 = c(0, 0.5, 0.5, 0.5),
  port20 = c(0.5, 0.5, 0.5, 0.5)
)
colnames(passability_values_df) <- names(abundance_data) #rename columns

# select just one row to start (starting simple)
temp <- index_choice_df[1, ]

## start small
smol_test = data.frame(
  pop1 = c(0.5, 0.5, 0.5, 0.5),
  pop2 = c(0.5, 0.5, 0.5, 0.5),
  pop3 = c(0.5, 0.5, 0.5, 0.5))

smol_index = data.frame(
  pop1 = 1,
  pop2 = 2,
  pop3 = 1)

## create a function for improve passability values using the index_choice
b_passage_index_fcn <- function(index_choice){
  passability_values[1:index_choice, ] <- 1
  return(passability_values)
}

# trying to use purrr but not working. help
invest_passability_df = passability_values %>% 
  mutate(passage = map_df(.x=temp,~b_passage_index_fcn(.x))) ## its working ahhhh

## this currently works, but only apply the first value in the index_choice_df across the whole passability df
passability_values[1:index_choice, ] <- 1


######
matching_columns <- colnames(passability_values_df) %in% colnames(temp) # match names of columns

# Apply the operation only to matching columns
passability_values[, matching_columns][1, ] <- 1

# Print the updated bpassage2 DataFrame
print(bpassage2)
######
passability_values[1:temp, ] <- 1


###################
library(dplyr)

# Create two sample dataframes
df1 <- data.frame(
  ID = c(1, 2, 3, 4),
  Value_df1 = c(10, 20, 30, 40),
  OtherValue_df1 = c(100, 200, 300, 400)
)

df2 <- data.frame(
  ID = c(2, 3, 4, 5),
  Value_df2 = c(200, 300, 400, 500),
  OtherValue_df2 = c(1000, 2000, 3000, 4000)
)

# Define the subset of rows (1:temp) and the value to replace (1)
temp <- 2

# Define a function to replace values in matching columns
replace_values <- function(df, rows, value) {
  df %>%
    mutate_at(vars(intersect(names(.), names(df1))), 
              funs(ifelse(row_number() <= rows, value, .)))
}

replace_values <- function(df, rows, value) {
  smol_test %>%
    mutate_at(vars(intersect(names(.), names(smol_index))), 
              funs(ifelse(row_number() <= rows, value, .)))
}

# Apply the function to df2
smol_test <- replace_values(smol_test, temp, 1)

# Print the result
print(df2)
###################



passability_values[, colnames(temp)] <- temp

passability_values[1, ] <- temp

# Print the updated bpassage2 DataFrame
print(bpassage2)

# Print the updated bpassage2 DataFrame
print(bpassage2)


for (i in seq_along(temp)) {
  bpassage2[temp[i], ] <- i  # Replace 'i' with your desired value or calculation
}


for (i in (temp)) {
  for (col in colnames(bpassage2)) {
    bpassage2[temp[i], col] <- i  # Replace 'i' with your desired value or calculation
  }
}

# Print the updated bpassage2 DataFrame
print(bpassage2)



# make new function
budget_index_df = budget_df %>% # dummy budget dataframe from above ## this will need to be 'budget_allocated_df' but all in one column named 'money'
  mutate(index_choice = map_dbl(.x=money,~while_fcn(.x))) ###oh my god it's working. I'm crying.


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



abundance_data <- data.frame(c(0, 0, 0.5, 1))

bpassage2 <- data.frame(
  port1 = c(0.5, 0.5, 0.5, 0.5),
  port2 = c(0.5, 0.5, 0.5, 0.5),
  port3 = c(0.5, 0.5, 0.5, 0.5),
  port4 = c(0.5, 0.5, 0.5, 0.5),
  port5 = c(0.5, 0.5, 0.5, 0.5),
  port6 = c(0.5, 0.5, 0.5, 0.5),
  port7 = c(0.5, 0.5, 0.5, 0.5),
  port8 = c(0, 0, 0.5, 0.5),
  port9 = c(0, 1, 0.5, 0.5),
  port10 = c(0, 0, 0.5, 1),
  port11 = c(0, 0.5, 0.5, 0.5),
  port12 = c(0, 0.5, 0.5, 0.5),
  port13 = c(0, 0.5, 0.5, 0.5),
  port14 = c(0, 0.5, 0.5, 0.5),
  port15 = c(0, 0.5, 0.5, 0.5),
  port16 = c(0, 0.5, 0.5, 0.5),
  port17 = c(0, 0.5, 0.5, 0.5),
  port18 = c(0, 0.5, 0.5, 0.5),
  port19 = c(0, 0.5, 0.5, 0.5),
  port20 = c(0.5, 0.5, 0.5, 0.5)
)

# Sample index_choice_list
index_choice_list <- c(2, 3)

# Apply each value in index_choice_list to corresponding rows and columns in bpassage2
for (i in seq_along(index_choice_list)) {
  for (col in colnames(bpassage2)) {
    bpassage[1:index_choice_list, ] <- 1  # Replace 'i' with your desired value or calculation
  }
}

# Print the updated bpassage2 DataFrame
print(bpassage2)




## 2016 assessment for coho <- volatilty was too high to delist 

