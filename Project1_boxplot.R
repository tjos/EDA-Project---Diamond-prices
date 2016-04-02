# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

library(ggplot2)

data(diamonds)

d<-ggplot(diamonds,aes(color,price/carat, fill=color))

#Plot 1: Diamond price per carat across different color
d+
  ggtitle("Diamond price/carat and color study") +
  theme_minimal() +
  ylim(0,7500) + 
  geom_boxplot() + 
  ggsave("Diamond color chart.png")

#Plot 2: Diamond price per carat across different color and clarity
d+
  ggtitle("Diamond price, color and clarity study") +
  theme_minimal() +
  ylim(0,7500) +
  geom_boxplot() + 
  facet_wrap(~clarity) +
  ggsave("Diamond clarity chart.png")  
