library(tidyverse)
library(LaplacesDemon)
library(nloptr)

#..........................Create randomized weight allocations.........................

n= 60000
abundance_data <- coho[2:20] # raw population abundance data
#abundance_data <- data.frame(abundance_data[-18]) # remove Tahkenitch

raw<-rdirichlet(n,rep(1,ncol(abundance_data))) #Generate 100 portfolio weights for all 20 streams

# ej weights
# raw<-rdirichlet(n,c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 3))

colnames(raw) <- names(abundance_data) 

rounded<-round(raw,2) # round to two decimals

# Need to make sure the weights still add up to 1
diff<-as.data.frame(raw-rounded) %>% # Take the difference at each element
  mutate(sum=rowSums(across(everything()))) # Sum across rows. Add the sum to highest weighted stream

for(i in 1:nrow(abundance_data)){
  rounded[i,which.max(rounded[i,])]<-rounded[i,which.max(rounded[i,])]+diff$sum[i]
}

# Check to make sure weights still add up to 1 for rounded dataframe
check<-as.data.frame(rounded) %>% 
  mutate(sum=rowSums(across(everything()))) %>% 
  filter(sum==1.00) %>% ## filter for only portfolios that sum to 1
  select(-sum)

# Add in QC: run a few portfolios that are the same each time, so we can make sure it is running correctly. Give the entire budget to each stream and see how it responds
full<-diag(ncol(abundance_data)) # create set portfolios

# add set simulations with random weights
#weights<-rbind(check,full) ## commenting this out just for now to run it with fewer portfolios while we get function running
weights <- check


## Renaming column names and making df a list 
colnames(weights) <- names(abundance_data) 

first_thousand_ej <- weights[1:2000,]
second_thousand_ej <- weights[20001:4000, ]
third_thousand_ej <- weights[4001:6000, ]
fourth_thousand_ej <- weights[6001:8000, ]
fifth_thousand_ej <- weights[8001:10000, ]
sixth_thousand_ej <- weights[10001:10236, ]

write.csv(weights,'all_weights_ej.csv', row.names = FALSE)
write.csv(first_thousand_ej, 'weights1_ej.csv',row.names = FALSE)
write.csv(second_thousand_ej, 'weights2_ej.csv',row.names = FALSE)
write.csv(third_thousand_ej, 'weights3_ej.csv',row.names = FALSE)
write.csv(fourth_thousand_ej, 'weights4_ej.csv',row.names = FALSE)
write.csv(fifth_thousand_ej, 'weights5_ej.csv',row.names = FALSE)
write.csv(sixth_thousand_ej, 'weights6_ej.csv',row.names = FALSE)


weights5_ej <- read.csv(here('data', 'ej_weights', 'weights5_ej.csv'))
grid_list_5_ej<-split(weights5_ej,seq(nrow(weights5_ej)))


