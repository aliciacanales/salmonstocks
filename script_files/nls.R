## creating new population model

## filter for a single population to start
alsea_coho <- coho %>% 
  select(year, alsea) %>% 
  mutate(return = lead(alsea)) %>% 
  drop_na() %>% #drops year 2019
  mutate(return_flip = 1/return,
         flip_abundance = 1/alsea)
  
pre_hat <- lm(return_flip~flip_abundance, data = alsea_coho)
pre_hat

# taking it out of flipped form
alsea_guess <- c(1 / pre_hat$coefficients[2],
                    1 / (pre_hat$coefficients[1] * (1/pre_hat$coefficients[2])))
alsea_guess

800 *5 / (1 + 800/345) #check output to make sure coefficients make sense. The output is the stock equilibrium abundance using our guess vectors


## initial visualization of population size over time
ggplot(data=alsea_coho,aes(x=year,y=alsea))+
  geom_point(size=2,color="black")+
  theme_minimal()


## s_t+1 = ((p_i)(S_t_i))/(1+ (S_t_i/c_i))
calculate_spawners <- function(abundance, prod, capacity){
  y = (prod * abundance)/(1 + (abundance/capacity))
  return(y)
}

## optimized/informed guesses for nls
guess_vec = alsea_guess


## running nls with the nls wrapper function

# copied original nls function that Alicia created (copied down below) and ran it with just the Alsea data as a test
run_nls = nls(return~calculate_spawners(alsea, prod, capacity),
              data = alsea_coho,
              start = list(prod = guess_vec[1],
                           capacity = guess_vec[2]),
              trace = TRUE)

# Official outputs for p hat and c hat
broom::tidy(run_nls)

nls_predict<-alsea_coho %>% 
  mutate(predict=predict(run_nls,newdata=.))

ggplot(data=nls_predict)+
  geom_point(aes(x=year,y=alsea))+
  geom_path(aes(x=year,y=predict),color='red')+
  theme_minimal()

## tables of nls values for control and first run. The productivity and capacity are negative... shouldn't they be positive?
broom::tidy(control_nls) %>% 
  kable(caption = "Control NLS") %>% 
  kable_classic()

broom::tidy(run_nls) %>% 
  kable(caption = "Original NLS") %>% 
  kable_classic()
