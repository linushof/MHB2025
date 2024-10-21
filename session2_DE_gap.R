
# preparation -------------------------------------------------------------

# install packages and load packages

'
install.packages(c("tidyverse", "magrittr", "readxl"))
library(tidyverse)
library(magrittr)
library(readxl)
'

#install.packages("pacman")
pacman::p_load(tidyverse, magrittr, readxl)

# Demo 1 ------------------------------------------------------------------

# price

diamonds %>% 
  ggplot(aes(x=cut, y = price, fill = cut)) + 
  geom_boxplot(alpha = .5)

diamonds %>% 
  ggplot(aes(x=clarity, y = price, fill = clarity)) + 
  geom_boxplot(alpha = .5)

diamonds %>% 
  ggplot(aes(x=color, y = price, fill = color)) + 
  geom_boxplot(alpha = .5)


# carat

diamonds %>% 
  ggplot(aes(x=cut, y = carat, fill = cut)) + 
  geom_boxplot(alpha = .5)

diamonds %>% 
  ggplot(aes(x=clarity, y = carat, fill = clarity)) + 
  geom_boxplot(alpha = .5)

diamonds %>% 
  ggplot(aes(x=color, y = carat, fill = color)) + 
  geom_boxplot(alpha = .5)

# price and carat

diamonds %>% 
  ggplot(aes(x=carat, y = price)) + 
  geom_point(alpha = .05) + 
  geom_smooth()


diamonds %>% 
  ggplot(aes(x=carat, y = price, fill = cut, color = cut)) + 
  geom_point(alpha = .05) + 
  geom_smooth()


# Demo 2 ------------------------------------------------------------------

# set working directory
#setwd(directory with data in it, e.g., "C:/user/documents")

# load data

Edat <- read_xlsx("data/Experience.xlsx") # data from decisions from experience
Ddat <- read_xlsx("data/Description.xlsx") # data from decisions from description

# prepare data

Ddat %<>%  
  mutate(mode = "Description") %>% # add mode variable to data set
  select(mode, everything()) # sort columns: mode first, then everything else

Edat %<>%  
  distinct(problem, subject, .keep_all = T) %>% # only keep decision data (1 row per person and subject)
  select(!c(trial, option, outcome)) %>%  # drop columns with trial-level sampling data
  mutate(mode = "Experience") %>%  # add mode variable
  select(mode, everything()) # sort columns 

dat <- bind_rows(Edat, Ddat) # merge data frames

dat %<>% 
  mutate(higherEV = 0 , # add column indicating the option with the higher EV, always B (see Hertwig et al. 2004)
         max = higherEV == choice # add column indicating if EV was maximized (returns TRUE or FALSE)
         )

Edat2 <- dat %>% filter(mode == "Experience") # filters data for Experience mode

mean(Edat2$max) # compute mean of column max to obtain percentage


Edat2 %>% 
  group_by(problem) %>% # group data by problem
  summarise(percentage = mean(max)) # compute group-/problem-wise percentage

# Task 1 ------------------------------------------------------------------

dat %<>% select(!c(higherEV, max)) # delete higherEV and max column

# compute expected values for each choice trial (row)

dat %<>% 
  mutate(evA = outA1 * probA1 + outA2 * probA2 , # expected value option A/0
         evB = outB1 * probB1 + outB2 * probB2) # expected value option B/1

# evaluate choices
dat %<>% 
  mutate(higherEV = if_else(evA > evB, 0, 1) , # determine option with higher EV
         max = higherEV == choice) # determine if option with higher EV was chosen

# results (Table 1 from Hertwig, 2004)

gap <- dat %>% 
  group_by(mode, problem) %>% # to calculate percentages separately for mode and problem
  summarise(percentage = 100*mean(max)) %>% # calculate percentages 
  ungroup() %>% 
  pivot_wider(names_from = mode, values_from = percentage) # distribute data from both modes into separate columns

print(gap)


# Task 2 ------------------------------------------------------------------

# Example below: People choose options with higher sampled mean

Edat3 <- read_xlsx("data/Experience.xlsx")
Edat3 %<>%    
  select(problem:choice) %>% # select only relevant variables (not necessary, but for better overview)
  pivot_wider(names_from = option, values_from = outcome, names_prefix = "Option_") %>% # split sampled outcomes from both options into separate columns
  group_by(problem, subject, choice) %>% # group data by choice trials 
  summarise(mean_0 = mean(Option_0, na.rm = TRUE) , # calculate sampled mean for option 1
            mean_1 = mean(Option_1, na.rm = TRUE) # calculate sampled mean for option 2
            ) %>% 
  mutate(prediction = if_else(mean_0 > mean_1, 0, if_else(mean_0 < mean_1, 1, NA)), # predict choice (option with higher sampled mean)
         correct = choice == prediction) %>% # check if prediction fits observed choice
  ungroup()

# Compare predictive accuracy of EV choice and SM choice

mean(Edat3$correct, na.rm = TRUE) # 79% SM maximization 
mean(Edat2$max, na.rm = TRUE) # 41% NM heuristic

