library(ggplot2)
library(dplyr)

data(diamonds)

# Plot1: Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

ggplot(data = diamonds, aes(x=price, fill=cut)) + 
  scale_x_log10() + ylim(0,700) +
  scale_fill_brewer(type = 'qual') +
  geom_histogram() +
  facet_wrap(~color) +
  theme_minimal()

ggsave("EDA3_Histogram_Price_Cut.png")

# Plot2: Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

ggplot(data = diamonds, aes(table, price)) + 
  geom_point(aes(color=cut)) +
  scale_color_brewer(type = 'qual') + 
  coord_cartesian(xlim=c(50,80)) +
  scale_x_continuous(breaks = seq(50,80,by=2)) +
  theme_minimal()

ggsave("EDA3_ScatterPlot_Price_table.png")

# Plot3: Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

vol <- (diamonds$x*diamonds$y*diamonds$z)
cbind(vol,diamonds)
ggplot(data = diamonds, aes(x = vol, y = price)) +
  geom_point(aes(color=clarity)) +
  scale_y_log10() +
  xlim(0,quantile(vol,0.99)) + 
  scale_color_brewer(type = 'div') +
  theme_minimal()

ggsave("EDA3_ScatterPlot_Price_Volume.png")

# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

pf <- read.delim('C:/Users/USTOJOS/Documents/R/pseudo_facebook.tsv', sep="\t")

head(pf)

prop_initiated <- pf$friendships_initiated / pf$friend_count

summary(prop_initiated)

# Plot4: Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

pf$year_joined <- floor(2014- pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined, c(2004,2009,2011,2012,2014))

ggplot(data = pf, aes(x = tenure, y = prop_initiated)) +
  geom_line(stat='summary', fun.y = median, aes(color=year_joined.bucket)) +
  theme_minimal()

ggsave("EDA3_LineGragp_PropInit_tenure.png")

# Plot5: Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

ggplot(data = pf, aes(x = tenure, y = prop_initiated)) +
  geom_line(stat='summary', fun.y = median, aes(color=year_joined.bucket)) +
  theme_minimal() +
  geom_smooth()

ggsave("EDA3_SmoothLineGragph_PropInit_tenure.png")

# Plot6: Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

ggplot(data = diamonds, aes(x = cut, y = price/carat)) +
  geom_jitter(aes(color=color)) +
  scale_color_brewer(type = 'div') +
  facet_wrap(~clarity, scales="free_x") +
  theme_minimal()

ggsave("EDA3_ScatterPlot_PriceperCarat_cut.png")
