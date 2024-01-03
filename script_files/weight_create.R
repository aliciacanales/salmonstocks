library(tidyverse)
library(LaplacesDemon) #You will need to install

#Generate 1000 portfolio weights for all 20 streams
n=1000
raw<-rdirichlet(n,rep(1,20)) #Replace 20 with relative reference e.g. ncol(abundance_data)

# I don't like the really small 0s and decimals. What is a budget of 0.0002%?
rounded<-round(raw,2)

#But now I need to make sure the weights still add up to 1. Take the difference at each element then sum. Add the sum to highest weighted stream

diff<-as.data.frame(raw-rounded) %>% 
  mutate(sum=rowSums(across(everything())))

for(i in 1:nrow(c)){
  rounded[i,which.max(rounded[i,])]<-rounded[i,which.max(rounded[i,])]+diff$sum[i]
}

# Check to make sure weights still add up to 1 for rounded dataset
check<-as.data.frame(rounded) %>% 
  mutate(sum=rowSums(across(everything())))


# I think we should always run a few portfolios that are the same each time. Mainly, give the entire budget to each stream and see how it responds.

full<-diag(20) #Replace 20 with relative reference e.g. ncol(abundance_data)

# add set simulations with random weights

weights<-rbind(rounded,full)


# From here you can make the "weights" a dataframe add the stream names in each column and anything else to make it raedy to pass into purrr map




