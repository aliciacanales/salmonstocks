library(ggplot2)
library(ggforce)

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
  mutate('portfolio' = paste("portfolio", 1:nrow(combined_23), sep = " "),
         'Budget' = '$23 million')

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
  mutate('portfolio' = paste("portfolio", 1:nrow(combined_23), sep = " "),
         'Budget' = '$13 million')

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
  mutate('portfolio' = paste("portfolio", 1:nrow(combined_23), sep = " "),
         'Budget' = '$3.5 million')

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
optimial_port <- eff_front_23 %>% 
  filter(portfolio %in% c('portfolio 1012', 'portfolio 5923')) %>% 
  mutate('radius' = c(1, 1))

plot_23 <- ggplot(combined_23, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_line(data = eff_front_23, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red', size = 1.2) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  geom_point(data = optimial_port, aes(x = esu_var_invest, y = esu_returns_invest),color = "blue", size = 5, pch = 1, stroke = 1.5) +
  geom_point(data = optimial_port, aes(x = esu_var_invest, y = esu_returns_invest),color = "#3b3b3b", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = c('0', '5.0e+19', '1.0e+20', '1.5e+20', '2.0e+20')) +
  geom_segment(aes(x = 2e+19,
                   y = 187118.2,
                   xend = 5e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 4e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  theme_minimal()
plot_23

ggsave("plot_23_circles.png", plot = plot_23, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("plot_23.png", plot = plot_23, width = 10, height = 6, dpi = 300, bg = "white")

## PLOTS FOR BUDGET 13.1

plot_13 <- ggplot(combined_13, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_line(data = eff_front_13, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red', size = 1.2) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = c('0', '5.0e+19', '1.0e+20', '1.5e+20')) +
  geom_segment(aes(x = 1.58e+19,
                   y = 187118.2,
                   xend = 4.1e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 3e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Returns') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15)) +
  theme_minimal()
plot_13

ggsave("plot_13.png", plot = plot_13, width = 10, height = 6, dpi = 300, bg = "white")

ej_plot_13 <- ggplot(combined_13, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = .5) +
  geom_point(data = ej_combined_13, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red', size = 1.2)+
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
  geom_line(data = eff_front_3.5, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red', size = 1.2) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  geom_segment(aes(x = 5e+18,
                   y = 187118.2,
                   xend = 2.6e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 0.84e+19, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Abundance') +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))+
  theme_minimal()

outliers_plot_3.5
ggsave("outliers_plot_3.5.png", plot = outliers_plot_3.5, width = 10, height = 6, dpi = 300, bg = "white")

no_outliers_plot_3.5 <- ggplot(combined_3.5_temp, aes(x = esu_var_invest, y = esu_returns_invest)) +
  geom_point(colour = 'gray', size = 2, alpha = 1/10) +
  geom_line(data = eff_front_3.5_without_outliers, aes(x = esu_var_invest, y = esu_returns_invest), color = 'red', size = 1.2) +
  geom_point(data = baseline_point, aes(x, y), color = "black", size = 3) +
  geom_segment(aes(x = 2.21e+18,
                   y = 187118.2,
                   xend = 2.001e+18,
                   yend = 187118.2),
               color = "black",
               linetype = "solid",
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x = 2.4e+18, y = 187118.2, label = "Baseline Portfolio", size = 5, check_overlap = T, color = 'black') +
  labs(x = 'ESU Variance', y = 'ESU Abundance') +
  xlim(1.95e+18, 3.5e+18) +
  scale_y_continuous(limits = c(150000, 430000), labels = scales::comma) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14))+
  theme_minimal()

no_outliers_plot_3.5
ggsave("no_outliers_plot_3.5.png", plot = no_outliers_plot_3.5, width = 10, height = 6, dpi = 300, bg = "white")



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

## DENSITY PLOT - ALL BUDGETS

all_portfolios_and_budgets <- rbind(combined_23,
                                    combined_13,
                                    combined_3.5)

plot_all <- ggplot(temp2_combined_df, aes(x = esu_returns_invest, y = ..density.., fill = Budget))+
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

## Density Plot

# data wrangling for density plot
temp_combined_23 <- mutate(combined_23, Budget = "$23 Million Budget")
temp_combined_13 <- mutate(combined_13, Budget = "$13 Million Budget")
temp_combined_3.5 <- mutate(combined_3.5, Budget = "$3.5 Million Budget")
temp2_combined_df <- bind_rows(temp_combined_23, temp_combined_13)

# as factor
temp2_combined_df$Budget <- factor(temp2_combined_df$Budget, levels = c("$3.5 Million Budget", "$13 Million Budget", "$23 Million Budget"))

density_plot <- ggplot(temp2_combined_df, aes(x = esu_var_invest, y = esu_returns_invest)) +
  stat_density_2d(data = combined_23, aes(color = "$23 Million Budget"), size = 1) +
  stat_density_2d(data = combined_13, aes(color = "$13 Million Budget"), size = 1) +
  stat_density_2d(data = combined_3.5, aes(color = "$3.5 Million Budget"), size = 1) +
  scale_color_manual(name = "", 
                     values = c("$23 Million Budget" = "#69b3a2", 
                                "$13 Million Budget" = "coral1", 
                                "$3.5 Million Budget" = "#F1D83B")) + #"#404080"
  labs(x = "Variance", y = "Returns") +
  scale_x_continuous(breaks = c(1.5e+18, 1.3e+19, 2.5e+19, 3.7e+19, 4.9e+19, 6.1e+19),
                     labels = c("1.5e+18", "1.3e+19", "2.5e+19", "3.7e+19", "4.9e+19", "6.1e+19")) +
  scale_y_continuous(breaks = seq(175000, 1500000, length.out = 4),
                     labels = c("175,000", "500,000", "1,000,000", "1,500,000")) +
  guides(color = guide_legend()) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),  # Increase text height to 12
    axis.text = element_text(size = 12),  # Increase axis labels to 12
    axis.title = element_text(size = 14),  # Increase axis titles to 14
    legend.text = element_text(size = 12)  # Increase legend names to 12
  )

