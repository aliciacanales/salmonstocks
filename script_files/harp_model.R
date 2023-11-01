## creating new population model

## filter for a single population to start
alsea_coho <- coho %>% 
  select(year, alsea)

## initial visualization
ggplot(data=alsea_coho,aes(x=year,y=alsea))+
  geom_point(size=2,color="black")+
  theme_minimal()


## s_t+1 = ((p_i)(S_t_i))/(1+ (S_t_i/c_i))

calculate_spawners <- function(abundance, prod, capacity){
  y = (prod * abundance)/(1 + (abundance/capacity))
  return(y)
}

## guesses for nls

guess_vec = c(sample(1:16000, 21)) ## Question from Olivia: why choose 1:16000? Answer from Alicia: I didn't know what the threshold should be for the guess vector so I just looked at our returns and saw the highest value was under 16000


## running nls with the nls wrapper function

# copied original nls function that Alicia created (copied down below) and ran it with just the Alsea data as a test
run_nls = nls(alsea~calculate_spawners(alsea, prod, capacity),
              data = alsea_coho,
              start = list(prod = guess_vec[1],
                           capacity = guess_vec[2]),
              trace = TRUE)

# run_nls = nls(abundance~calculate_spawners(abundance, prod, capacity),
#               data = coho,
#               start = list(prod = guess_vec[1],
#                            capacity = guess_vec[2]),
#               trace = TRUE)

## show how the model fits the data by making a prediction based on the data
nls_predict<-alsea_coho %>% 
  mutate(predict=predict(run_nls,newdata=.))

ggplot(data=nls_predict)+
  geom_point(aes(x=year,y=alsea))+
  geom_path(aes(x=year,y=predict),color='red')+
  theme_minimal()

## add control nls model to compare
control_nls=nls(alsea~calculate_spawners(alsea, prod, capacity),
                data = alsea_coho,
                start = list(prod = guess_vec[1],
                             capacity = guess_vec[2]),
                control = nls.control(tol = 2.1e-9,minFactor=1e-10,warnOnly = TRUE)) #these values are the same from lab 4, was not sure how to edit these values

## tables of nls values for control and first run. The productivity and capacity are negative... shouldn't they be positive?
broom::tidy(control_nls) %>% 
  kable(caption = "Control NLS") %>% 
  kable_classic()

broom::tidy(run_nls) %>% 
  kable(caption = "Original NLS") %>% 
  kable_classic()


## run with purrr


## Gauss-Newton
## Use AIC or BIC or cross fold validation to compare different model fits

## logs of 1... update with random numbers for vectors. We will then get individual pi and ci