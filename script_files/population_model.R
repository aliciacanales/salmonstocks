## creating new population model


## s_t+1 = ((p_i)(S_t_i))/(1+ (S_t_i/c_i))

calculate_spawners <- function(abundance, prod, capacity){
  y = (prod * abundance)/(1 + (abundance/capacity))
  return(y)
}

## guesses for nls

guess_vec = 

## running nls with the nls wrapper function

run_nls = nls(abundance~calculate_spawners(abundance, prod, capacity),
              data = coho,
              start=list,
              trace = TRUE)



## Gauss-Newton
## Use AIC or BIC or cross fold validation to compare different model fits
    