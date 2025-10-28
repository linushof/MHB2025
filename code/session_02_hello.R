# Exercise 4 

n_obs <- 5
dat <- data.frame(v1 = rnorm(n_obs, mean=1),
                  v2 = rnorm(n_obs, mean=2),
                  v3 = rnorm(n_obs, mean=3),
                  v4 = rnorm(n_obs, mean=4),
                  v5 = rnorm(n_obs, mean=5))

c(mean(dat[,1]), mean(dat[,2]), mean(dat[,3]), mean(dat[,4]), mean(dat[,5]))
sapply(dat, mean)
?sapply()
colMeans(dat)


# Exercise 5

sequence <- 1:5
output <- vector('numeric', length(sequence))

for (i in sequence){ 
  
  output[i] <- mean(dat[,i])
  
}

output



# Exercise 6

# install.packages("tidyverse")
library(tidyverse)

# visualization -----------------------------------------------------------

# Build dataset with 2 groups and different distributions
group_n <- 1000 # number of observations per group
group <- c( rep("Group 1", group_n), rep("Group 2", group_n) ) # create grouping variable
value <-  c( rnorm(group_n, mean = 0), rnorm(group_n, mean=4)) # generate random values for both groups
data <- data.frame(group, value) # store variables in a data frame

# Represent it
ggplot(data, mapping = aes(x=value, fill = group)) + # data that should be plotted
  geom_histogram(position = "identity", alpha = .5, color = "gray") + # histogram & settings
  scale_fill_manual(values=c("#69b3a2", "#404080")) + #fill color of histogram bars
  theme_dark() # plot theme


# Exercise 7

ggplot(data, mapping = aes(x=value, fill = group)) + # data that should be plotted
  geom_histogram(position = "identity", alpha = .5, color = "gray") + # histogram & settings
  scale_fill_manual(values=c("#69b3a2", "#404080")) + # fill color of histogram bars
  theme_dark() + # plot theme
  labs(title = "Distribution of Group 1 vs. Group 2",
       x = "Value", 
       y = "Frequency",
       fill = "Group") + 
  facet_wrap(~group, nrow = 2)



# diamonds example --------------------------------------------------------

#install.packages("pacman")
pacman::p_load(tidyverse, ggdist, gghalves, ggpubr)


diamonds$clarity <- as.numeric(diamonds$clarity)
diamonds$cut <- as.numeric(diamonds$cut)
diamonds$color <- as.numeric(diamonds$color)


ggplot(diamonds, aes(y = price, x = as.factor(clarity))) + 
  stat_halfeye(adjust = 1.5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .15, outliers = FALSE, alpha = .3) +
  geom_half_point(side = "l", range_scale = .4, alpha = .1, size =.1) +
  labs(x="Clarity", 
       y="Price") +
  theme_minimal(base_size = 20) 

ggplot(diamonds, aes(y = price, x = as.factor(cut))) + 
  stat_halfeye(adjust = 1.5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .15, outliers = FALSE, alpha = .3) +
  geom_half_point(side = "l", range_scale = .4, alpha = .1, size =.1) +
  labs(x="Cut", 
       y="Price") +
  theme_minimal(base_size = 20) 

ggplot(diamonds, aes(y = price, x = as.factor(color*-1))) + 
  stat_halfeye(adjust = 1.5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .15, outliers = FALSE, alpha = .3) +
  geom_half_point(side = "l", range_scale = .4, alpha = .1, size =.1) +
  labs(x="Color", 
       y="Price") +
  theme_minimal(base_size = 20) 

ggplot(diamonds, aes(y = price, x = carat)) + 
  geom_jitter(alpha = .1, size =.1) +
  theme_minimal()


dat <- list(
  p = scale(diamonds$price), 
  cl = scale(diamonds$clarity),
  ca = scale(diamonds$carat)
)

# Statistical Models

m1 <- lm(p ~ cl, data = dat)
summary(m1)

m2 <- lm(p ~ ca, data = dat)
summary(m2)

m3 <- lm(ca ~ cl, data = dat)
summary(m3)

m4 <- lm(p ~ cl + ca, data = dat)
summary(m4)


