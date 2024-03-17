library(ggplot2)
# eff_frontier <- ggplot(test, aes(x = , y = s_invest)) +
#   geom_point(colour = 'darkcyan', size = 2) + 
#   geom_curve(x = 3.521570e+17, y = 205623.0,
#              xend = 3.892000e+17, yend = 211781.8,
#              colour = 'red', curvature = -.3) +
#   geom_hline(yintercept=187118, linetype="dashed", color = "gray") +
#   geom_vline(xintercept=3.141711e+17, linetype="dashed", color = "gray") +
#   theme_minimal() +
#   labs(x = 'Variance', y = 'ESU Abundance') +
#   scale_y_continuous(labels = scales::comma) +
#   ggrepel::geom_text_repel(aes(label = id,
#                                size = 2)) +
#   theme(legend.position = "none")

baseline_point <- data.frame(x =1.95539e+18, y = 186948.6)

## BUDGET 23
portfolios_23_1 <- read_csv('portfolios_23_1_map.csv') 
portfolios_23_2 <- read_csv('portfolio_2_23.csv')
portfolios_23_3 <- read_csv('portfolios_23_3_map.csv') 
portfolios_23_4 <- read_csv('portfolios_23_4_map.csv') # only 444 observations, because error at portfolio 445 - so only ran portfolios 1-444 in weights 4
portfolios_23_5 <- read_csv('portfolios_23_5_map.csv') 
portfolios_23_6 <- read_csv('portfolios_23_6.1_map.csv')

combined_23 <- rbind(portfolios_23_1,
                     portfolios_23_2,
                     portfolios_23_3,
                     portfolios_23_4,
                     portfolios_23_5,
                     portfolios_23_6)

# combined_23 <- combined_23 %>% 
#   mutate('Budget' = '$23 million')

eff_front_23 <-combined_23 %>% 
  arrange(esu_var_invest) %>% 
  subset(esu_returns_invest==cummax(esu_returns_invest))

## BUDGET 13.1
portfolios_13_1 <- read_csv(here('data', 'portfolios1.csv'))
portfolios_13_2 <- read_csv(here('data', 'portfolios2.csv'))
portfolios_13_3 <- read_csv(here('data', 'portfolios3.csv'))
portfolios_13_4 <- read_csv(here('data', 'portfolios4.csv'))
portfolios_13_5 <- read_csv(here('data', 'portfolios5.csv'))
portfolios_13_6 <- read_csv('portfolios_13.1_6.1_map.csv')

combined_13 <- rbind(portfolios_13_1,
                      portfolios_13_2,
                      portfolios_13_3,
                      portfolios_13_4,
                      portfolios_13_5,
                      portfolios_13_6) 
combined_13 <- combined_13 %>% 
  mutate('Budget' = '$13.1 million')

ej_portfolios_13_5 <- read_csv('portfolios_13.1_5_ej_map.csv')

## BUDGET 3.5
portfolios_3.5_1 <- read_csv('portfolios_3.5_1_map.csv')
portfolios_3.5_2 <- read_csv('portfolios_3.5_2_map.csv')
portfolios_3.5_3 <- read_csv('portfolios_3.5_3_map.csv')
portfolios_3.5_4 <- read_csv('portfolios_3.5_4_map.csv')
portfolios_3.5_5 <- read_csv('portfolios_3.5_5_map.csv')
portfolios_3.5_6 <- read_csv('portfolios_3.5_6.1_map.csv')

combined_3.5 <- rbind(portfolios_3.5_1,
                      portfolios_3.5_2,
                      portfolios_3.5_3,
                      portfolios_3.5_4,
                      portfolios_3.5_5,
                      portfolios_3.5_6) 
combined_3.5_temp <- combined_3.5[-c(1:7), ]

ej_portfolios_3.5 <- read_csv('.csv')

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
  xlim(0, 1.5e+20) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2250000)) +
  # annotate("segment",
  # x = 1.5e+19, xend = 3.14e+27 , ## this controls how long the arrow is
  # y = 187118.2, yend = 187118.2, ## controls where the tip of the arrow ends
  # arrow = arrow(), color="black") +
  geom_segment(aes(x = 1.58e+19,
  y = 187118.2,
  xend = 3.8e+18,
  yend = 187118.2),
  color = "black",
  linetype = "solid",
  arrow = arrow(length = unit(0.3, "cm"))) +
geom_text(x = 3.95539e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T) +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  ggtitle("Portfolios Results for a $13.1 million Budget") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))

plot_13

ej_plot_13 <- ggplot(combined_13, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = ej_portfolios_13_5, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red')+
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
outliers_plot_3.5<- ggplot(combined_3.5, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
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


outliers_plot_13.5

ej_plot_3.5<- ggplot(data = combined_3.5, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  # geom_point(data = ej_portfolios_3, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red')+
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 5e+18,
                   y = 187118.2,
                   xend = 2.6e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 1e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14)) +
  theme_minimal()

## PLOTS FOR ALL BUDGETS

plot_all <- ggplot(all_portfolios_and_budget, aes(x = esu_var_invest, y = esu_returns_invest, color = Budget)) +
  geom_jitter(aes(color = Budget), alpha = .3) +
  # geom_density_2d(aes(color = Budget)) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5, alpha = .5) +
  xlim(0, 1.25e+20) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2050000)) +
  scale_color_discrete(limits = c('$23 million', '$13.1 million', '$3.5 million'),
                       labels = c('$23 million', '$13.1 million', '$3.5 million')) +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
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

