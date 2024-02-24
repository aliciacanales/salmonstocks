
## Calculate S for population after investment
s_fun <- function(delta_p, delta_c){
  s_invest <- (delta_p-1)*delta_c
  return(s_invest)
}

## Calculate change in p_hat after investment
p_hat_fun <- function(p_hat,p_change, weight){ 
  p <- p_hat * (1 + p_change * weight) 
}

## Calculate change in c_hat after investment
c_hat_fun <- function(c_hat, c_change, weight){ 
  c <- c_hat * (1 + c_change * weight)
}

## create 10 portfolios with weights that sum to 100
wgt_1 <- data.frame(weight=c(30,70,0,160,10,90,0,20,0,40,0,130,90,50,20,80,40,50,0,120))
wgt_2 <- data.frame(weight=c(8,1,2,7,4,0,2,5,12,1,14,3,7,5,8,3,1,8,3,6))
wgt_3 <- data.frame(weight=c(0,9,4,2,7,8,2,3,0,4,3,5,8,14,6,3,4,5,10,3))
wgt_4 <- data.frame(weight=c(4,5,3,11,4,1,6,6,7,2,9,8,1,0,6,7,2,9,5,4))
wgt_5 <- data.frame(weight=c(7,8,4,0,3,4,11,8,2,1,6,3,9,3,7,5,1,3,8,7))
wgt_6 <- data.frame(weight=c(10,2,5,1,7,4,9,2,3,0,2,14,3,10,2,3,5,1,9,8))
wgt_7 <- data.frame(weight=c(5,6,3,4,8,7,5,5,4,6,3,4,8,7,5,4,0,9,5,2))
wgt_8 <- data.frame(weight=c(1,2,12,6,3,9,4,2,8,0,3,0,7,7,5,11,3,6,3,8))
wgt_9 <- data.frame(weight=c(3,7,1,16,4,2,7,8,8,6,0,3,8,2,11,1,1,5,0,7))
wgt_10 <- data.frame(weight=c(9,6,3,5,1,3,8,0,9,13,2,1,7,2,1,12,8,3,2,5))
# sum(wgt_1$weight)

## create 10 portfolios with weights that sum to 1000
wgt_1 <- data.frame(weight=c(30, 70, 0, 160, 10, 90, 0, 20, 0, 40, 0, 130, 90, 50, 20, 80, 40, 50, 0, 120))
wgt_2 <- data.frame(weight=c(0, 0, 60 , 10, 40, 70 , 200, 120, 50, 40, 30, 10, 10, 90, 0, 140, 10, 20, 30, 70))
wgt_3 <- data.frame(weight=c(100, 60, 70, 20, 20, 10, 50, 0, 0, 30, 0, 90, 70, 40, 60, 140, 120, 30, 80, 10))
wgt_4 <- data.frame(weight=c(200, 130, 0, 10, 40, 30, 50, 0, 0, 30, 40, 70, 80, 20, 0, 180, 90, 10, 20, 0))
wgt_5 <- data.frame(weight=c(70, 20, 30, 30, 20, 60, 70, 100, 50, 0, 130, 70, 60, 0, 10, 50, 110, 90, 30, 0))
wgt_6 <- data.frame(weight=c(0, 30, 20, 70, 200, 10, 10, 40, 0, 60, 30, 10, 50, 70, 90, 140, 20, 20, 30, 100))
wgt_7 <- data.frame(weight=c(40, 80, 100, 40, 30, 10, 140, 20, 20, 10, 60, 30, 40, 30, 10, 200, 80, 20, 10, 30))
wgt_8 <- data.frame(weight=c(10, 40, 50, 0, 40, 210, 120, 10, 50, 100, 100, 30, 10, 10, 70, 70, 0, 50, 10, 20))
wgt_9 <- data.frame(weight=c(190, 40, 70, 0, 60, 20, 70, 0, 30, 140, 40, 30, 40, 10, 20, 80, 120, 0, 40, 0))
wgt_10 <- data.frame(weight=c(40, 150, 10, 80, 50, 0, 40, 150, 10, 90, 50, 20, 20, 30, 110, 60, 20, 10, 20, 40))
# sum(wgt_1$weight)

# variance data from population
pop_var <- sapply(coho[2:22], var) 
clean_var <- data.frame(var=pop_var[-18]) # remove tahkenitch because removed from above

################
# Running our defined portfolio scenarios from above (will automate this)
################

## wgt_1
invested_stock_wgt1 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_1) %>% 
  mutate(p_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in p
         c_change = .001, # $10 investment leads to a .01 increase, so a $1 investment leads to a .001 increase in c
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), # calculate s for each population before investment
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) # calculate s for each population after investment using new p and c

variance_wgt1 <- invested_stock_wgt1 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% #percent change in variance from baseline
  mutate(square = return_investment^2)
# mutate(port_var = var / s_baseline *(return_investment^2))

## wgt_2
invested_stock_wgt2 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_2) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt2 <- invested_stock_wgt2 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_3
invested_stock_wgt3 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_3) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt3 <- invested_stock_wgt3 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_4
invested_stock_wgt4 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_4) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt4 <- invested_stock_wgt4 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_5
invested_stock_wgt5 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_5) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt5 <- invested_stock_wgt5 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_6
invested_stock_wgt6 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_6) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt6 <- invested_stock_wgt6 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_7
invested_stock_wgt7 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_7) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt7 <- invested_stock_wgt7 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_8
invested_stock_wgt8 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_8) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt8 <- invested_stock_wgt8 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_9
invested_stock_wgt9 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_9) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt9 <- invested_stock_wgt9 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

## wgt_10
invested_stock_wgt10 <- equilibrium_all %>% 
  mutate(p_hat = map_dbl(coeff, ~.[['p_hat']]),
         c_hat = map_dbl(coeff, ~.[['c_hat']])) %>%
  select(population, p_hat, c_hat) %>%
  cbind(wgt_10) %>% 
  mutate(p_change = .001, 
         c_change = .001, 
         delta_p = map_dbl(p_hat, ~p_hat_fun(.x,p_change, weight)),
         delta_c = map_dbl(c_hat, ~c_hat_fun(.x,c_change, weight)),
         s_baseline = pmap_dbl(list(p_hat,c_hat),s_fun), 
         s_invest = pmap_dbl(list(delta_p,delta_c),s_fun)) 

variance_wgt10 <- invested_stock_wgt10 %>% 
  group_by(population) %>% 
  mutate(return_investment = s_invest - s_baseline) %>%
  cbind(clean_var) %>% 
  mutate(base_var = var * (s_baseline^2)) %>% 
  mutate(invest_var = var *(s_invest^2)) %>% 
  mutate(var_percent_change = ((invest_var-base_var)/base_var)*100) %>% 
  mutate(square = return_investment^2)

#############
# ESU Portfolio return and variance
#############

portfolio_invest_s <- invested_stock_wgt1 %>% 
  select(population, s_invest) %>% 
  rename("s_invest_wgt1" = "s_invest") %>% 
  cbind(s_invest_wgt_2=invested_stock_wgt2$s_invest,
        s_invest_wgt_3=invested_stock_wgt3$s_invest,
        s_invest_wgt_4=invested_stock_wgt4$s_invest,
        s_invest_wgt_5=invested_stock_wgt5$s_invest,
        s_invest_wgt_6=invested_stock_wgt6$s_invest,
        s_invest_wgt_7=invested_stock_wgt7$s_invest,
        s_invest_wgt_8=invested_stock_wgt8$s_invest,
        s_invest_wgt_9=invested_stock_wgt9$s_invest,
        s_invest_wgt_10=invested_stock_wgt10$s_invest)

total_invest_s <- data.frame(total = colSums(portfolio_invest_s[,-1]))


portfolio_var <- variance_wgt1 %>% 
  select(population, invest_var) %>% 
  rename("invest_var_wgt1" = "invest_var") %>% 
  cbind(invest_var_wgt2 = variance_wgt2$invest_var,
        invest_var_wgt3 = variance_wgt3$invest_var,
        invest_var_wgt4 = variance_wgt4$invest_var,
        invest_var_wgt5 = variance_wgt5$invest_var,
        invest_var_wgt6 = variance_wgt6$invest_var,
        invest_var_wgt7 = variance_wgt7$invest_var,
        invest_var_wgt8 = variance_wgt8$invest_var,
        invest_var_wgt9 = variance_wgt9$invest_var,
        invest_var_wgt10 = variance_wgt10$invest_var)

## calculated population at baseline
sum(invested_stock_wgt1$s_baseline)
sum(variance_wgt1$base_var)

  
esu_stock_var <- data.frame(total = colSums(portfolio_var[,-1])) %>% 
  rename("total_var" = "total") %>% 
  cbind("total_stock_invest" = total_invest_s$total)


## Variance percent change
portfolio_var_percent_change <- variance_wgt1 %>% 
  select(population, var_percent_change) %>% 
  rename("var_percent_change_wgt1" = "var_percent_change") %>% 
  cbind(var_percent_change_wgt2 = variance_wgt2$var_percent_change,
        var_percent_change_wgt3 = variance_wgt3$var_percent_change,
        var_percent_change_wgt4 = variance_wgt4$var_percent_change,
        var_percent_change_wgt5 = variance_wgt5$var_percent_change,
        var_percent_change_wgt6 = variance_wgt6$var_percent_change,
        var_percent_change_wgt7 = variance_wgt7$var_percent_change,
        var_percent_change_wgt8 = variance_wgt8$var_percent_change,
        var_percent_change_wgt9 = variance_wgt9$var_percent_change,
        var_percent_change_wgt10 = variance_wgt10$var_percent_change)

var_percent_change_and_invest_stock <- data.frame(total = colMeans(portfolio_var_percent_change[,-1])) %>% 
  rename("mean_percent_change_var" = "total") %>% 
  cbind("total_stock_invest" = total_invest_s$total)


## Combine the sum return and variance for the ESU and plot

id <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9","P10")

ggplot(esu_stock_var, aes(x = total_var, y = total_stock_invest)) +
  geom_point(colour = 'darkcyan', size = 2) + 
  geom_curve(x = 3.521570e+17, y = 205623.0,
             xend = 3.892000e+17, yend = 211781.8,
             colour = 'red', curvature = -.3) +
  theme_minimal() +
  labs(x = 'Variance', y = 'ESU Abundance') +
  scale_y_continuous(labels = scales::comma) +
  ggrepel::geom_text_repel(aes(label = id,
            size = 2)) +
  theme(legend.position = "none")

# Baseline stock abundance: 187118.2
# Baseline variance: 3.141711e+17

## Add baseline population and baseline variance
ggplot(esu_stock_var, aes(x = total_var, y = total_stock_invest)) +
  geom_point(colour = 'darkcyan', size = 2) + 
  geom_curve(x = 3.521570e+17, y = 205623.0,
             xend = 3.892000e+17, yend = 211781.8,
             colour = 'red', curvature = -.3) +
  geom_hline(yintercept=187118, linetype="dashed", color = "gray") +
  geom_vline(xintercept=3.141711e+17, linetype="dashed", color = "gray") +
  theme_minimal() +
  labs(x = 'Variance', y = 'ESU Abundance') +
  scale_y_continuous(labels = scales::comma) +
  ggrepel::geom_text_repel(aes(label = id,
                               size = 2)) +
  theme(legend.position = "none")


## Percent Change of Variance
ggplot(var_percent_change_and_invest_stock, aes(x = mean_percent_change_var, y = total_stock_invest)) +
  geom_point(colour = 'darkcyan', size = 2) +
  geom_curve(x = 26.25655, y = 209264.2,
             xend = 31.44946, yend = 211477.7,
             colour = 'red', curvature = -.3) +
  theme_minimal() +
  labs(x = 'Percent Change of Variance from Baseline with Investment', y = 'ESU Abundance') + 
  ggrepel::geom_text_repel(aes(label = id,
                               size = 2)) +
  theme(legend.position = "none")


## Create table to show weights for portfolio 5
weights_p5 <- invested_stock_wgt5 %>% 
  select(population, weight) %>% 
  pivot_wider(names_from = population, values_from = weight) # pivot wider

rownames(weights_p5) <- c("Portfolio_5")

table_weights_p5 <- weights_p5 %>% 
  round(3) %>% 
  kable(align = "c") %>% 
  kable_styling(bootstrap_options = "striped",
                position = "center", full_width = FALSE)

table_weights_p5


################

n=1000
nvar=20
set.seed(1)
rand<-matrix(0,nrow=n,ncol=nvar)
for(i in 1:n){
  pull=runif(nvar)
  rand[i,1:nvar]<-pull/sum(pull)
  
}


#############
## Visualizations
#############
# ggplot(new_stock, aes(x=population, y=p_hat)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# ggplot(new_stock, aes(x=population, y=c_hat)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# ggplot(new_stock, aes(x=population, y=s_invest)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) #not working
.