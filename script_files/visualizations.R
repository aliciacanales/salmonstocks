library(ggplot2)
eff_frontier <- ggplot(test, aes(x = , y = s_invest)) +
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