library(ggplot2)

data(diamonds)

#Plot1: Scatter plot for price vs x
d <- ggplot(data = diamonds, aes(x=x, y=price))
d+geom_point()
ggsave("EDA2_ScatterPlot_PriceVsX.png")

# Corelation between price and x, y, z
cor.test(diamonds$price,diamonds$x)
cor.test(diamonds$price,diamonds$y)
cor.test(diamonds$price,diamonds$z)

# Plot2: Scatter plot for price vs depth
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. 

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=1/100) +
  scale_x_continuous(breaks=seq(43,79,by=2))

ggsave("EDA2_ScatterPlot_PriceVsDepth.png")

# Corelation between price and depth
cor.test(diamonds$depth,diamonds$price)

# Plot3: scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

ggplot(data = diamonds, aes(x = carat, y = price)) +
  xlim(0,quantile(diamonds$carat,0.99)) +
  ylim(0,quantile(diamonds$price,0.99)) +
  geom_point() 

ggsave("EDA2_ScatterPlot_PriceVscarat.png")

# Plot4: Create a scatterplot of price vs. volume (x * y * z).
vol <- (diamonds$x*diamonds$y*diamonds$z)
cbind(vol,diamonds)
ggplot(data = diamonds, aes(x = vol, y = price)) +
  geom_point() 

ggsave("EDA2_ScatterPlot_PriceVsVolume.png")

#Correlation of price vs volume
library(dplyr)

vol <- (diamonds$x*diamonds$y*diamonds$z)

diamonds2 <- diamonds %>%
  mutate(vol=x*y*z) %>%
  filter(vol > 0 & vol <=800)

cor.test(diamonds2$vol,diamonds2$price)                    

# Plot5: Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot.

v <- ggplot(data = diamonds2, aes(x = vol, y = price)) +
  geom_point(alpha=1/100,color="orange") 

v1 <- v + 
  geom_smooth() + 
  ggtitle("Default Smoother")

v2 <- v + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1) + 
  ylim(0,20000) + 
  ggtitle("Linear model")

v3 <- v +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
  ylim(0,20000) +
  ggtitle("Linear model with Vol^2")

v4 <- v + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1) + 
  ylim(0,20000) +
  ggtitle("Linear model with Vol^3")

library(gridExtra)

v5 <- grid.arrange(v1,v2,v3,v4,ncol =2)

ggsave("EDA2_Linearmodel_PriceVsVolume.png", plot = v5)


# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

diamondsByClarity <-
  diamonds %>%
    group_by(clarity) %>%
    summarise(mean_price=mean(price),
              median_price=median(price),
              min_price=min(price), 
              max_price = max(price),
              n=n())  %>%
    arrange(clarity)

# Plot6: We’ve created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

head(diamonds_mp_by_clarity)
head(diamonds_mp_by_color)

dcl <- ggplot(diamonds_mp_by_clarity, aes(x=clarity,y=mean_price, fill=clarity)) + 
  geom_bar(stat = "identity") +
  ggtitle("Diamonds Mean Price by Clarity")

dco <- ggplot(diamonds_mp_by_color, aes(x=color,y=mean_price, fill=color)) + 
  geom_bar(stat = "identity") +
  ggtitle("Diamonds Mean Price by Color")

dc <- grid.arrange(dcl,dco)

ggsave("EDA2_DiamondMPbyColorClarity.png", plot = dc)


