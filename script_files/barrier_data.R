
# .................................compute passability with updated equation 2/28/24..............................
bpassage_base_compute_fcn <- function(df) {
  # browser()
  
  #prep data for calculation
  df <- df
  
  # Identify number of stream levels in population
  r <- length(unique(df$strm_lev)) # number of levels in a population
  r_true_lev <- unique(df$strm_lev) # a vector of the values of each of those levels
  
  # create a vector for level and level_weight (wr) that is the length of r (will fill in with data below)
  bpass_strm_lev_v <- numeric(r) # seq(1:r)
  w_strm_lev_v <- numeric(r) # seq(1:r)
  
  # initialize
  bpassage <- 0
  y <- 0
  
  # Calculate y (summation) mini for loop, this needs to be defined outside of the next for loop below because it is a constant that equation
  for (i in 1:r) {
    temp_y <- sum(1/i)
    y <- y + temp_y
  }
  
  # iterate over stream level
  for (i in 1:r) {
    
    # identify the value for the true stream level for i
    true_lev <- r_true_lev[i]
    
    # filter for just the level we want
    temp_lev_df <- df %>% 
      filter(strm_lev == true_lev)
    
    # count the number of barriers within stream level i
    n_barrier_strm_lev <- temp_lev_df %>% 
      summarise(count = n()) %>%
      pull(count) # keep it a vector
    
    # identify number of stream ids within the stream level
    sr <- length(unique(temp_lev_df$strm_id)) # number of ids in the level
    sr_true_id <- unique(temp_lev_df$strm_id) # vector of the values of those stream ids
    
    # create a vector for stream_id and stream_id_weight (wsr) that is the length of sr (will fill in with data below)
    bpass_strm_id_v <- numeric(sr)
    
    # compute weight by stream level
    strm_wgt <- 1 / (i * y)
    w_strm_lev_v[i] <- strm_wgt
    
    # initialize
    prod_pass_strm_id <- 0
    w_strm_id <- 0
    strm_lev_passability <- 0
 
    # iterate over stream id within stream level
    for (j in 1:sr) {
      
      # identify the value for the true stream id for j
      true_id <- sr_true_id[j]
      
      # filter for just the data we want
      temp_id_df <- df %>% 
        filter(strm_lev == true_lev & strm_id == true_id)
      
      # identify number of barriers in the stream id
      n_barrier_strm_id <- temp_id_df %>% 
        summarise(count = n()) %>%
        pull(count) # keep it a vector
      
      # compute the product of all pass scores within the same id
      prod_pass_strm_id <- prod(temp_id_df$pass_score)
      
      # compute weight for stream id j
      w_strm_id <- n_barrier_strm_id / n_barrier_strm_lev # weight component at id level
      
      # compute passability for stream id j
      id_passability <- prod_pass_strm_id * w_strm_id
      
      # update vector for passability of j
      bpass_strm_id_v[j] <- id_passability
      
    }
    
    # sum the passabilities across all ids within a stream level
    bpass_strm_lev <- sum(bpass_strm_id_v)
    
    # compute passability for each stream level
    bpass_strm_lev_v[i] <- bpass_strm_lev * w_strm_lev_v[i] # weight component at the steam level
    
  }
  
  #bpassage for the population
  bpassage <- sum(bpass_strm_lev_v)
  
  return(bpassage)
  
}


# .................................Compute passability for each population..............................
# Apply function to each data frame in the list and combine the results into one dataframe
bpassage_base = map_df(.x=barrier_list,~bpassage_base_compute_fcn(.x))

# Pivot longer
bpassage_base <- bpassage_base %>% 
  pivot_longer(
    cols = c(1:20),
    names_to = "population",
    values_to = "bpassage"
  )


# .................................compute passability AFTER INVESTMENT with updated equation 2/28/24..............................
bpassage_invest_compute_fcn <- function(df) {
  # browser()
  
  #prep data for calculation
  df <- df
  
  # Identify number of stream levels in population
  r <- length(unique(df$strm_lev)) # number of levels in a population
  r_true_lev <- unique(df$strm_lev) # a vector of the values of each of those levels
  
  # create a vector for level and level_weight (wr) that is the length of r (will fill in with data below)
  bpass_strm_lev_v <- numeric(r) # seq(1:r)
  w_strm_lev_v <- numeric(r) # seq(1:r)
  
  # initialize
  bpassage <- 0
  y <- 0
  
  # Calculate y (summation) mini for loop, this needs to be defined outside of the next for loop below because it is a constant that equation
  for (i in 1:r) {
    temp_y <- sum(1/i)
    y <- y + temp_y
  }
  
  # iterate over stream level
  for (i in 1:r) {
    
    # identify the value for the true stream level for i
    true_lev <- r_true_lev[i]
    
    # filter for just the level we want
    temp_lev_df <- df %>% 
      filter(strm_lev == true_lev)
    
    # count the number of barriers within stream level i
    n_barrier_strm_lev <- temp_lev_df %>% 
      summarise(count = n()) %>%
      pull(count) # keep it a vector
    
    # identify number of stream ids within the stream level
    sr <- length(unique(temp_lev_df$strm_id)) # number of ids in the level
    sr_true_id <- unique(temp_lev_df$strm_id) # vector of the values of those stream ids
    
    # create a vector for stream_id and stream_id_weight (wsr) that is the length of sr (will fill in with data below)
    bpass_strm_id_v <- numeric(sr)
    
    # compute weight by stream level
    strm_wgt <- 1 / (i * y)
    w_strm_lev_v[i] <- strm_wgt
    
    # initialize
    prod_pass_strm_id <- 0
    w_strm_id <- 0
    strm_lev_passability <- 0
    
    # iterate over stream id within stream level
    for (j in 1:sr) {
      
      # identify the value for the true stream id for j
      true_id <- sr_true_id[j]
      
      # filter for just the data we want
      temp_id_df <- df %>% 
        filter(strm_lev == true_lev & strm_id == true_id)
      
      # identify number of barriers in the stream id
      n_barrier_strm_id <- temp_id_df %>% 
        summarise(count = n()) %>%
        pull(count) # keep it a vector
      
      # compute the product of all pass scores within the same id
      prod_pass_strm_id <- prod(temp_id_df$pass_score_invest)
      
      # compute weight for stream id j
      w_strm_id <- n_barrier_strm_id / n_barrier_strm_lev # weight component at id level
      
      # compute passability for stream id j
      id_passability <- prod_pass_strm_id * w_strm_id
      
      # update vector for passability of j
      bpass_strm_id_v[j] <- id_passability
      
    }
    
    # sum the passabilities across all ids within a stream level
    bpass_strm_lev <- sum(bpass_strm_id_v)
    
    # compute passability for each stream level
    bpass_strm_lev_v[i] <- bpass_strm_lev * w_strm_lev_v[i] # weight component at the steam level
    
  }
  
  #bpassage for the population
  bpassage <- data.frame(sum(bpass_strm_lev_v))
  
  return(bpassage)
  
}

# end....................................................................................................................................











## creating this max length to find the max length the passability df should be. It should be as long as the population with the most barriers which is s_umpqua.
max_length <- max(length(alsea$pass_score), length(beaver$pass_score), length(coos$pass_score),
                  length(coquille$pass_score), length(floras$pass_score), length(lower_umpqua$pass_score),
                  length(middle_umpqua$pass_score), length(necanicum$pass_score), length(nehalem$pass_score),
                  length(nestucca$pass_score), length(north_umpqua$pass_score), length(salmon$pass_score),
                  length(siletz$pass_score), length(siltcoos$pass_score), length(siuslaw$pass_score),
                  length(sixes$pass_score), length(south_umpqua$pass_score), length(tenmile$pass_score),
                  length(tillamook$pass_score), length(yaquina$pass_score))

## Passability data frame with all the barriers from each population. NA values filled in the extra spaces. Not sure how the NAs will play out in out functions. 
## This was really quick. Just a bunch of copy and paste, so no biggie if we have to get rid of it bc of all the NAs I added. It was the only way to combine all the columns together. -AC

passability_values_df <- data.frame(alsea = c(alsea$pass_score, rep(NA, max_length - length(alsea$pass_score))),
                          beaver = c(beaver$pass_score, rep(NA, max_length - length(beaver$pass_score))),
                          coos = c(coos$pass_score, rep(NA, max_length - length(coos$pass_score))),
                          coquille = c(coquille$pass_score, rep(NA, max_length - length(coquille$pass_score))),
                          floras = c(floras$pass_score, rep(NA, max_length - length(floras$pass_score))),
                          lower_umpqua = c(lower_umpqua$pass_score, rep(NA, max_length - length(lower_umpqua$pass_score))),
                          middle_umpqua = c(middle_umpqua$pass_score, rep(NA, max_length - length(middle_umpqua$pass_score))),
                          necanicum = c(necanicum$pass_score, rep(NA, max_length - length(necanicum$pass_score))),
                          nehalem = c(nehalem$pass_score, rep(NA, max_length - length(nehalem$pass_score))),
                          nestucca = c(nestucca$pass_score, rep(NA, max_length - length(nestucca$pass_score))),
                          north_umpqua = c(north_umpqua$pass_score, rep(NA, max_length - length(north_umpqua$pass_score))),
                          salmon = c(salmon$pass_score, rep(NA, max_length - length(salmon$pass_score))),
                          siletz = c(siletz$pass_score, rep(NA, max_length - length(siletz$pass_score))),
                          siltcoos = c(siltcoos$pass_score, rep(NA, max_length - length(siltcoos$pass_score))),
                          siuslaw = c(siuslaw$pass_score, rep(NA, max_length - length(siuslaw$pass_score))),
                          sixes = c(sixes$pass_score, rep(NA, max_length - length(sixes$pass_score))),
                          south_umpqua = c(south_umpqua$pass_score, rep(NA, max_length - length(south_umpqua$pass_score))),
                          tenmile = c(tenmile$pass_score, rep(NA, max_length - length(tenmile$pass_score))),
                          tillamook = c(tillamook$pass_score, rep(NA, max_length - length(tillamook$pass_score))),
                          yaquina = c(yaquina$pass_score, rep(NA, max_length - length(yaquina$pass_score))))

pass_values_na <- passability_values_df


# replace any NA with 1
passability_values_df[is.na(passability_values_df)] <- 1

## taking the avg passability 
passabilities_avg = colMeans(passability_values_df)
avg_passability <- as.data.frame(passabilities_avg)


## ................................Getting bpassage isolated................................
alsea <- read_csv(here('data', 'final_table_alsea.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(alsea = 'beta_pass') 


beaver <- read_csv(here('data', 'final_table_beaver.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(beaver = 'beta_pass')

coos <- read_csv(here('data', 'final_table_coos.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(coos = 'beta_pass')

coquille <- read_csv(here('data', 'final_table_coquille.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(coquille = 'beta_pass')

floras <- read_csv(here('data', 'final_table_floras.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(floras = 'beta_pass')

lower_umpqua <- read_csv(here('data', 'final_table_lowerump.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(lower_umpqua = 'beta_pass')

middle_umpqua <- read_csv(here('data', 'final_table_midump.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(middle_umpqua = 'beta_pass')

necanicum <- read_csv(here('data', 'final_table_necanicum.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(necanicum = 'beta_pass')

nehalem <- read_csv(here('data', 'final_table_nehalem.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(nehalem = 'beta_pass')

nestucca <- read_csv(here('data', 'final_table_nestucca.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(nestucca = 'beta_pass')

north_umpqua <- read_csv(here('data', 'final_table_northump.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(north_umpqua = 'beta_pass')

salmon <- read_csv(here('data', 'final_table_salmon.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(salmon = 'beta_pass')

siletz <- read_csv(here('data', 'final_table_siletz.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(siletz = 'beta_pass')

siltcoos <- read_csv(here('data', 'final_table_siltcoos.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(siltcoos = 'beta_pass')

siuslaw <- read_csv(here('data', 'final_table_siuslaw.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(siuslaw = 'beta_pass')

sixes <- read_csv(here('data', 'final_table_sixes.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(sixes = 'beta_pass')

south_umpqua <- read_csv(here('data', 'final_table_southumpqua.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(south_umpqua = 'beta_pass')

tenmile <- read_csv(here('data', 'final_table_tenmile.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(tenmile = 'beta_pass')

tillamook <- read_csv(here('data', 'final_table_tillamook.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(tillamook = 'beta_pass')

yaquina <- read_csv(here('data', 'final_table_yaquina.csv')) %>% 
  clean_names() %>% 
  select(beta_pass) %>% 
  drop_na() %>% 
  rename(yaquina = 'beta_pass')

bpassage_base <- as.data.frame(cbind(alsea, beaver, coos, coquille,floras,lower_umpqua, middle_umpqua, necanicum, nehalem, nestucca, north_umpqua, salmon, siletz, siltcoos, siuslaw, sixes, south_umpqua, tenmile, tillamook, yaquina)) %>% 
  pivot_longer(1:20,
               names_to = 'population',
               values_to = 'bpassage')





#### old passability functions - these have since been updated, with different equations, but keeping old code just in case.

# .................................function to calculate stream passability..............................
# strm_wgt_fcn <- function(df) {
# 
#   #prep data for calculation
#   df <- df %>%
#     select(c(pass_score, strm_lev)) %>%
#     arrange(strm_lev, pass_score) %>%
#     group_by(strm_lev) %>%
#     summarise(product_pass_lev = prod(pass_score)) %>%
#     ungroup()
# 
#   # Identify number of stream levels in population
#   r <- max(df$strm_lev) + 1 - min(df$strm_lev) # i think this fixes the error from the pops with no stream order of 1 (lower umpqua and north umpqua)
# 
#   y <- 0
# 
#   # Calculate y
#   for (i in 1:r) {
#     temp_y <- sum(1/i)
#     y <- y + temp_y
#   }
# 
#   df$strm_wgt <- numeric(nrow(df))
# 
#   # Calculate strm_wgt and add it as a new column to df
#   for (i in 1:r) {
#     strm_wgt <- 1 / (i * y)
#     df$strm_wgt[i] <- strm_wgt
#   }
# 
#   df$lev_pass <- df$product_pass_lev * df$strm_wgt
#   #df$passability <- sum(df$lev_pass)
# 
#   return(df)
# }
# 
# # Call the function and store the result in temp_output
# temp_output_alsea <- strm_wgt_fcn(alsea)
# temp_output_beaver <- strm_wgt_fcn(beaver)
# temp_output_lower_umpqua <- strm_wgt_fcn(lower_umpqua)

# .................................calculate passability for many dataframes..............................
# old_bpassage_compute_fcn <- function(df) {
#   #prep data for calculation
#   df <- df %>%
#     group_by(strm_lev) %>%
#     summarise(product_pass_lev = prod(pass_score)) #%>%
#     #ungroup()
#   
#   # Identify number of stream levels in population
#   r <- max(df$strm_lev) + 1 - min(df$strm_lev) # this corrects for populations with no stream level of 1
#   
#   y <- 0
#   
#   # Calculate y (summation)
#   for (i in 1:r) {
#     temp_y <- sum(1/i)
#     y <- y + temp_y
#   }
#   
#   # Create new column in dataframe called strm_wgt
#   df$strm_wgt <- numeric(nrow(df))
#   
#   # Compute strm_wgt in the column strm_wgt
#   for (i in 1:r) {
#     strm_wgt <- 1 / (i * y)
#     df$strm_wgt[i] <- strm_wgt
#   }
#   
#   # Compute passability by stream level
#   df$lev_pass <- df$product_pass_lev * df$strm_wgt
#   
#   # Sum to get bpassage for each population
#   bpassage <- sum(df$lev_pass)
#   
#   bpassage_base <- data.frame(population = name, bpassge_base = bpassage_base)
#   
#   return(df)
# }


