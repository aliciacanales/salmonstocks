###Developing amat, bvec, dmat, and dvec
## first row of amat
mean_returns_df <- data.frame(0.5195554, 0.8668412, 1.0668467, 0.8037888, 0.8741770, 0.6100251, 0.6810882, 0.4573553, 0.4978172, 0.8339032, 0.0761665, 2.5722876, 0.5931486, 0.2879635, 0.6849828, 0.9460155, 0.7222910, 0.7872894, 0.3794362, 1.0945078, 0.9048277) # making  df with just the mean returns from each population
colnames(mean_returns_df) <- c('1':'21') # renaming column headers

## second row of amat
line_2_df <- data.frame(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
colnames(line_2_df) <- c('1':'21') #Rename column headers

## bottom of amat
id_matrix <- diag(21) #Create identity matrix 21x21
colnames(id_matrix) <- c('1':'21')

amat<- rbind(mean_returns_df, line_2_df, id_matrix)

## bvec with our mean of all pop returns 
bvec <- data.frame(mean_returns,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) 
colnames(bvec) <- c('1':'23') #Rename column headers

## dmat
dmat <- cov(returns)

dmat_new <- dmat
dmat_new[12,12]=10


## making everything as a matrix for quadprog
amat_matrix <- t(as.matrix(amat)) #Create amat data frame 

bvec_matrix <- t(as.matrix(bvec))

dvec_matrix <- t(as.matrix(rep(0,21)))
dvec <- rep(0,21)