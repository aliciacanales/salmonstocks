library(ggplot2)
<<<<<<< HEAD

baseline_point <- data.frame(x =1.95539e+18, y = 186948.6)

## BUDGET 23
portfolios_23_1 <- read_csv(here('data', 'portfolios', '23_million', 'portfolios_23_1_map.csv')) 
portfolios_23_2 <- read_csv(here('data', 'portfolios', '23_million', 'portfolio_2_23.csv'))
portfolios_23_3 <- read_csv(here('data', 'portfolios', '23_million', 'portfolios_23_3_map.csv')) 
portfolios_23_4 <- read_csv(here('data', 'portfolios', '23_million', 'portfolios_23_4_map.csv')) # only 444 observations, because error at portfolio 445 - so only ran portfolios 1-444 in weights 4
portfolios_23_5 <- read_csv(here('data', 'portfolios', '23_million', 'portfolios_23_5_map.csv')) 
portfolios_23_6 <- read_csv(here('data', 'portfolios', '23_million', 'portfolios_23_6.1_map.csv'))

combined_23 <- rbind(portfolios_23_1,
                     portfolios_23_2,
                     portfolios_23_3,
                     portfolios_23_4,
                     portfolios_23_5,
                     portfolios_23_6)

combined_23 <- combined_23 %>%
  mutate('Budget' = '$23 million')

eff_front_23 <-combined_23 %>% 
  arrange(esu_var_invest) %>% 
  subset(esu_returns_invest==cummax(esu_returns_invest))

ej_portfolios_23_5 <- read_csv(here('data', 'portfolios', '23_million', 'portfolios_23_5_ej_map.csv'))
ej_portfolios_23_6 <- read_csv(here('data', 'portfolios', '23_million', 'portfolios_23_6_ej_map.csv'))
ej_combined_13 <- rbind(ej_portfolios_23_5,
                     ej_portfolios_23_6)

ej_plot_23 <- ggplot(combined_23, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = ej_combined_13, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red')+
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 1.58e+19,
                   y = 187118.2,
                   xend = 4.1e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 3.95539e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14)) +
  theme_minimal()

baseline_point <- data.frame(x =1.95539e+18, y = 186948.6)

## BUDGET 13.1

portfolios_13_1 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios1.csv'))
portfolios_13_2 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios2.csv'))
portfolios_13_3 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios3.csv'))
portfolios_13_4 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios4.csv'))
portfolios_13_5 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios5.csv'))
portfolios_13_6 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios_13.1_6.1_map.csv'))


combined_13 <- rbind(portfolios_13_1,
                      portfolios_13_2,
                      portfolios_13_3,
                      portfolios_13_4,
                      portfolios_13_5,
                      portfolios_13_6) 

combined_13$rank <- cummax(rank(combined_13$esu_returns_invest))

combined_13 <- combined_13 %>% 
  mutate('Budget' = '$13.1 million')

eff_front_13 <-combined_13 %>% 
  arrange(esu_var_invest) %>% 
  subset(esu_returns_invest==cummax(esu_returns_invest))

combined_13 %>% 
  arrange(esu_var_invest)

ej_portfolios_13_5 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios_13.1_5_ej_map.csv'))
ej_portfolios_13_6 <- read_csv(here('data', 'portfolios', '13.1_million', 'portfolios_13.1_6.1_map.csv'))
ej_combined_13 <- rbind(ej_portfolios_13_5,
                     ej_portfolios_13_6)

## BUDGET 3.5

portfolios_3.5_1 <- read_csv(here('data', 'portfolios', '3.5_million', 'portfolios_3.5_1_map.csv'))
portfolios_3.5_2 <- read_csv(here('data', 'portfolios', '3.5_million', 'portfolios_3.5_2_map.csv'))
portfolios_3.5_3 <- read_csv(here('data', 'portfolios', '3.5_million', 'portfolios_3.5_3_map.csv'))
portfolios_3.5_4 <- read_csv(here('data', 'portfolios', '3.5_million', 'portfolios_3.5_4_map.csv'))
portfolios_3.5_5 <- read_csv(here('data', 'portfolios', '3.5_million', 'portfolios_3.5_5_map.csv'))
portfolios_3.5_6 <- read_csv(here('data', 'portfolios', '3.5_million', 'portfolios_3.5_6.1_map.csv'))

combined_3.5 <- rbind(portfolios_3.5_1,
                      portfolios_3.5_2,
                      portfolios_3.5_3,
                      portfolios_3.5_4,
                      portfolios_3.5_5,
                      portfolios_3.5_6) 


combined_3.5 <- combined_3.5 %>% 
  mutate('Budget' = '$3.5 million')

eff_front_3.5 <- combined_3.5 %>% 
  arrange(esu_var_invest) %>% 
  subset(esu_returns_invest==cummax(esu_returns_invest))

combined_3.5_temp <- combined_3.5[-c(1:7), ]

eff_front_3.5_without_outliers <- combined_3.5_temp %>% 
  arrange(esu_var_invest) %>% 
  subset(esu_returns_invest==cummax(esu_returns_invest))


ej_portfolios_3.5 <- read_csv(here('data', 'portfolios', '3.5_million', 'portfolios_3.5_5_ej_map.csv'))


##........................Density plot......................................
# my_plot_3.5 + 
#   geom_density(aes(x = esu_var_invest, y = esu_returns_invest)) +
#   geom_jitter()
##.........................................................................


## PLOTS FOR BUDGET 23

plot_23 <- ggplot(combined_23, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  geom_line(data = eff_front_23, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red') +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 2e+19,
                   y = 187118.2,
                   xend = 5e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 5e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T) +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))
plot_23


## PLOTS FOR BUDGET 13.1

plot_13 <- ggplot(combined_13, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  geom_line(data = eff_front_13, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red') +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 1.58e+19,
                   y = 187118.2,
                   xend = 4.1e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 3.95539e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14)) +
  theme_minimal()

plot_13

ggplot(combined_13, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = ej_combined_13, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red')+
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 1.58e+19,
                   y = 187118.2,
                   xend = 4.1e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 3.95539e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14)) +
  theme_minimal()

## PLOTS FOR BUDGET 3.5
outliers_plot_3.5 <- ggplot(combined_3.5, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_line(data = eff_front_3.5, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red') +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 5e+18,
                   y = 187118.2,
                   xend = 2.6e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 1e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 3) +
  labs(x = 'ESU Variance', y = 'ESU Abundance') +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))+
  theme_minimal()

outliers_plot_3.5

no_outliers_plot_3.5 <- ggplot(combined_3.5_temp, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_line(data = eff_front_3.5_without_outliers, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red') +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 3e+18,
                   y = 187118.2,
                   xend = 2.1e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 4.4e+18, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 3) +
  labs(x = 'ESU Variance', y = 'ESU Abundance') +
  xlim(1e+18, 1e+19) +
  scale_y_continuous(limits = c(150000, 500000), labels = scales::comma) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))+
  theme_minimal()

no_outliers_plot_3.5

ej_plot_3.5<- ggplot(data = combined_3.5, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_line(data = eff_front_3.5_without_outliers, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red') +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 3) +
  geom_segment(aes(x = 3e+18,
                   y = 187118.2,
                   xend = 2.1e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 4.4e+18, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Abundance') +
  xlim(1e+18, 1e+19) + ## this can be changed
  scale_y_continuous(limits = c(150000, 500000), labels = scales::comma) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))+
  theme_minimal()


no_outliers_plot_3.5

ej_plot_3.5<- ggplot(data = combined_3.5, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = ej_portfolios_3.5, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red')+
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  geom_segment(aes(x = 3e+18,
                   y = 187118.2,
                   xend = 2.1e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 4.4e+18, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  xlim(1e+18, 1e+19) +
  scale_y_continuous(limits = c(150000, 500000), labels = scales::comma) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14)) +
  theme_minimal()

## ALL BUDGETS

all_portfolios_and_budgets <- rbind(combined_23,
                                    combined_13,
                                    combined_3.5)

ggplot(all_portfolios_and_budgets, aes(x = esu_returns_invest, y = ..density.., fill = Budget))+
  geom_density(aes(color = Budget), position = 'stack') +
  labs(x = 'ESU Returns', y = 'Density') +
  theme_minimal()


plot_all +
  geom_segment(aes(x = 1.58e+19,
                   y = 187118.2,
                   xend = 3.8e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14)) +
  geom_text(x = 3.95539e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  ggtitle('All Portfolio Results Under 3 Different Budgets')

