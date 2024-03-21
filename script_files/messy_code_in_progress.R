
library(tidyverse)
library(purrr)
library(furrr)
install.packages(furrr)

set.seed(123)
#.......................... combinging while_loop into one.........................

## 204045.3 <- mean
## 108847.2 <- median
## budget = 3500000 and 23 mil. or find something else
budget = 3500000
budget = 13100000
budget = 23000000

## covariance (using covariance from 'coho' calulated in 'population.R')
cov_temp <- coho[2:22]
cov_rm <- cov_temp[-18] # remove tahkenitch
cov_coho <- sum(cov(cov_rm[1:20])) # the sum of the cov of esu (without tahkenitch)


# budget_allocated_fcn <- function(budget,weight){
#   weight=weight %>% unlist()
#   budget_allocated <- budget * weight
#   return(budget_allocated)
# }
# 
# cov <- sum(sigma %*% t(sigma) * weights %*% t(weights) * cov_matrix)
# #---
# temp <- 0
# 
# 
# sd <- sapply(coho[2:22], sd)
# sd_rm<-sd[-18]
# 
# # Loop over each i
# for (i in 1:n) {
#   # Calculate the sum for each i
#   cov_1 <- sum(sd_rm[i] * s_invest[i] * sd_rm[-i] * s_invest[-i] * cov_matrix[i, -i])
#   # Add this sum to the total sum
#   cov_2 <- temp + cov_1
# }

# Finally, multiply by X_n and gamma
# covariance <- X_n * gamma * cov_2
# 
# # Print the result
# print(expression2)
#---

cov_matrix <- coho[2:20]

cov_matrix <- cov(cov_matrix)

# test new variance calculation
temp_matrix <- as.matrix(cov_matrix) %*% as.matrix(temp)
out <- t(as.matrix(temp)) %*% temp_matrix


optimize_fcn <- function(weight){
  #browser()
  weight=weight %>% unlist()
  output1 <- pmap_dbl(list(budget, weight),budget_allocated_fcn) 
  output2 <- pmap(list(output1,barrier_list),while_fcn) # check to see if this is being transformed
  bpassage_invest <- unlist(map_df(.x=output2,~bpassage_invest_compute_fcn(.x)))
  c_invest <- c_invest_fcn(z_c_df$z, bpassage_invest)
  p_invest <- p_invest_fcn(z_p_df$z, bpassage_invest)
  s_invest <- ((p_invest - 1) * c_invest)
  s_baseline <- ((z_p_df$p_hat - 1) * z_c_df$c_hat)
  esu_returns_invest <- sum(s_invest)
  esu_returns_baseline <- sum(s_baseline)
  
  # var <- sapply(coho[2:22], var)
  # var_rm<-var[-18]
  # sd <- sapply(coho[2:22], sd)
  # sd_rm<-sd[-18]
  # #cov_matrix <- cov(coho[2:22])
  
  ## variance at baseline
  temp_matrix_base <- as.matrix(cov_matrix) %*% as.matrix(s_baseline)
  esu_var_baseline <- t(as.matrix(s_baseline)) %*% temp_matrix_base
  
  ## variance after investment
  temp_matrix_invest <- as.matrix(cov_matrix) %*% as.matrix(s_invest)
  esu_var_invest <- t(as.matrix(s_invest)) %*% temp_matrix_invest
  
  ## covariance of investment
  # cov_2 <- 0
  # # Loop over each i
  # for (i in 1:20) {
  #   # Calculate the sum for each i
  #   cov_1 <- sum(sd_rm[i] * s_invest[i] * sd_rm[-i] * s_invest[-i] * cov_matrix[i])
  #   # Add this sum to the total sum
  #   cov_2 <- cov_2 + cov_1
  # }
  # cov_invest <- cov_2
  # 
  # ## covariance of baseline
  # cov_2_baseline <- 0
  # # Loop over each i
  # for (i in 1:20) {
  #   # Calculate the sum for each i
  #   cov_1_baseline <- sum(sd_rm[i] * s_baseline[i] * sd_rm[-i] * s_baseline[-i] * cov_matrix[i])
  #   # Add this sum to the total sum
  #   cov_2_baseline <- cov_2_baseline + cov_1_baseline
  # }
  # cov_baseline <- cov_2_baseline
  # 
  # 
  # var_invest <- var_rm * (s_invest^2)
  # var_baseline <- var_rm * (s_baseline^2)
  # esu_var_invest <- sum(var_invest + cov_invest)
  # esu_var_baseline <- sum(var_baseline + cov_baseline)

  
  
  #return(s_baseline) # to look at single dataframe
  return(round(data.frame(esu_returns_invest, esu_returns_baseline, esu_var_invest, esu_var_baseline),3))
}

plan(multisession, workers = 4)
test = future_map_dfr(.x=grid_list_temp,~optimize_fcn(.x))

portfolios_13.1_5_ej = map_df(.x=grid_list_5_ej,~optimize_fcn(.x))

weights4_thin <- weights4[-c(445:2000), ]
weights4_test <- weights4[445, ]


# output results
write.csv(portfolios_3.5_1_map, 'portfolios_3.5_1_map.csv', row.names = FALSE)
write.csv(portfolios_3.5_2_map, 'portfolios_3.5_2_map.csv', row.names = FALSE)
write.csv(portfolios_3.5_3_map, 'portfolios_3.5_3_map.csv', row.names = FALSE)
write.csv(portfolios_3.5_4_map, 'portfolios_3.5_4_map.csv', row.names = FALSE)
write.csv(portfolios_3.5_5_map, 'portfolios_3.5_5_map.csv', row.names = FALSE)
write.csv(portfolios_3.5_6.1_map, 'portfolios_3.5_6.1_map.csv', row.names = FALSE)
write.csv(portfolios_13.1_6.1, 'portfolios_13.1_6.1_map.csv', row.names = FALSE)
write.csv(portfolios_23_6.1_map, 'portfolios_23_6.1_map.csv', row.names = FALSE)
write.csv(portfolios_23_4_thin_map, 'portfolios_23_4_thin_map.csv', row.names = FALSE)
write.csv(portfolios_23_5_ej, 'portfolios_23_5_ej_map.csv', row.names = FALSE)
write.csv(portfolios_23_6_ej, 'portfolios_23_6_ej_map.csv', row.names = FALSE)

# combine individual dataframes for each budget into one output
combined_3.5 <- rbind(portfolios_3.5_1_map,
                      portfolios_3.5_2_map,
                      portfolios_3.5_3_map,
                      portfolios_3.5_4_map,
                      portfolios_3.5_5_map,
                      portfolios_3.5_6.1_map) %>% 
  arrange(-esu_returns_invest)

combined_3.5_temp <- combined_3.5[-c(1:7), ]

combined_23 <- rbind(portfolios_23_6.1_map,
                     portfolios_23_4_thin_map)



#...................................... plots ......................................
library(ggalt)

# baseline esu returns = 187118.2
# baseline esu variance = 3.141711e+17 (this will change with updated variance calculation)
baseline_point <- data.frame(x =1.95539e+18, y = 186948.6)

# remove outliers to plot (is this okay to do?)
temp <- test[-c(277:289), ]


# portfolios and efficiency frontier
my_plot <- ggplot(combined_23, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = portfolios_23_6_ej, aes(x = esu_var_invest, y = esu_returns_invest), colour = "red") +
  #geom_point(data = portfolios_3.5_2_map, aes(x = esu_var_invest, y = esu_returns_invest)) +
  #geom_point(colour = 'gray', size = 2, alpha = .5) +
  # geom_curve(x = 3.521570e+17, y = 205623.0,
  # xend = 3.892000e+17, yend = 211781.8,
  # colour = 'red', curvature = -.3) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 3) +
  # annotate("segment",
  #          x = 1.5e+28, xend = 3.14e+27 , ## this controls how long the arrow is
  #          y = 187118.2, yend = 187118.2, ## controls where the tip of the arrow ends
  #          arrow = arrow(), color="black") +
  geom_segment(aes(x = 1.5e+19,
                   y = 187118.2,
                   xend = 4e+18,
                   yend = 187118.2),
                   color = "black",
                   linetype = "solid",
                   arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 3.6e+19, y = 187118.2, label = "Baseline Portfolio", size = 4, check_overlap = T) +
  labs(x = 'ESU Variance', y = 'ESU Abundance') +
  xlim(1.5e+18, 1.5e+20) +
  #ylim(0, 1000000) +
  ggtitle("Portfolio Results for a $23 Million Budget") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))

my_plot

ggsave("my_plot.png", plot = my_plot)



p5 +theme(axis.title = element_text(size = 15),
          axis.text.x = element_text(size = 10.5),
          axis.text.y = element_text(size = 10.5))





## ggoptimal in ggplot
## 


# plot 1 wrangling
budget_allocated_df <- read_csv(here('data', 'portfolios', 'optimal_portfolios', 'selected_portfolios_23_budget.csv'))
optimal_portfolio_1_23 <- budget_allocated_df[1, ] %>% # this is random, just using for framework for now
  rename_with(str_to_title)

names(optimal_portfolio_1_23)[names(optimal_portfolio_1_23) == 'Lower_umpqua'] <- 'Lower Umpqua'
names(optimal_portfolio_1_23)[names(optimal_portfolio_1_23) == 'Middle_umpqua'] <- 'Middle Umpqua'
names(optimal_portfolio_1_23)[names(optimal_portfolio_1_23) == 'North_umpqua'] <- 'North Umpqua'
names(optimal_portfolio_1_23)[names(optimal_portfolio_1_23) == 'South_umpqua'] <- 'South Umpqua'
 
optimal_portfolio_1_23 <- optimal_portfolio_1_23 %>% 
 pivot_longer(cols = 1:19,
               names_to = 'population',
               values_to = 'budget_allocated')


# plot 1 wrangling
optimal_portfolio_2_23 <- budget_allocated_df[2, ] %>% # this is random, just using for framework for now
  rename_with(str_to_title)
  
names(optimal_portfolio_2_23)[names(optimal_portfolio_2_23) == 'Lower_umpqua'] <- 'Lower Umpqua'
names(optimal_portfolio_2_23)[names(optimal_portfolio_2_23) == 'Middle_umpqua'] <- 'Middle Umpqua'
names(optimal_portfolio_2_23)[names(optimal_portfolio_2_23) == 'North_umpqua'] <- 'North Umpqua'
names(optimal_portfolio_2_23)[names(optimal_portfolio_2_23) == 'South_umpqua'] <- 'South Umpqua'
  
optimal_portfolio_2_23 <- optimal_portfolio_2_23 %>%  
  pivot_longer(cols = 1:19,
               names_to = 'population',
               values_to = 'budget_allocated')



# lollipop plot 1
p1 <- optimal_portfolio_1_23 %>% 
  ggplot(aes(x = fct_reorder(population, budget_allocated), #fct_reorder lets us set the order of the first value, by the second value ($ invested)
             y = budget_allocated)) +
  ggalt::geom_lollipop() +
  # labs(x = "Population", y = "Budget Allocated (USD)") +
  ggtitle("Portfolio B", subtitle = "Returns: 2,109,848\nVariance: 8.629809e+19") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # scale_x_discrete(labels = function(x) toTitleCase(x)) + # Need to fix the names with two words still
  scale_y_continuous(labels = function(x) paste0('$',x / 1000000, "M")) +
  # gghighlight::gghighlight(population == "tillamook") + # if we want to emphasize a single population
  #coord_flip() +
  labs(y = 'Budget Allocated') +
  xlab(NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 15)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 12.5)) +
  coord_flip()

ggsave("optimal_portfolio_B_plot.png", plot = p1, width = 10, height = 6, dpi = 300, bg = "white")
               
# lollipop plot 2 
p2 <- optimal_portfolio_2_23 %>% 
  ggplot(aes(x = fct_reorder(population, budget_allocated), #fct_reorder lets us set the order of the first value, by the second value ($ invested)
             y = budget_allocated)) +
  ggalt::geom_lollipop() +
  # labs(x = " ", y = "Budget Allocated (USD)") +
  ggtitle("Portfolio A", subtitle = "Returns: 1,355,534\nVariance: 1.959021e+19") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # scale_x_discrete(labels = function(x) toTitleCase(x)) + # Need to fix the names with two words still
  scale_y_continuous(labels = function(x) paste0('$',x / 1000000, "M")) +
  # gghighlight::gghighlight(population == "tillamook") + # if we want to emphasize a single population
  #coord_flip() +
  labs(y = 'Budget Allocated') +
  xlab(NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 15)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 12.5)) +
  coord_flip()
ggsave("optimal_portfolio_A_plot.png", plot = p2, width = 10, height = 6, dpi = 300, bg = "white")


p3 <- p1+p2 & xlab(NULL) & ylab(NULL)
p1 <- p1 & xlab(NULL) & ylab(NULL)
p2 <- p2 & xlab(NULL) & ylab(NULL)
  
wrap_elements(p1) +
  labs(tag = 'Budget Allocated (USD)') +
  theme(plot.tag = element_text(size = 11),
        plot.tag.position = "bottom")

  
# facet wrap or patchwork plots side by side



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

