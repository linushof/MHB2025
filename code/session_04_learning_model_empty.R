library(tidyverse)

# Task 1: Define the models --------------------------------------------------------

expo <- function(A , # additive constant 
                 B , # multiplicative constant / scale
                 alpha , # learning rate
                 N # number of learning trials
                 ){
  
  # write the exponential law model here using the values above

  }


power <- function(A, B, beta, N){
  
  # write the power law model here using the values above
  
}


# Task 2: Simulate data -----------------------------------------------------------
# use the expo/power function to simulate response times (RT)
# for N= 1, 2, ..., 100 learning trials 

# Use the following values for simulations
# A = 0, B = 1, alpha/beta = .5

RT_expo <- expo(# enter function arguments here
  )

RT_power <- power(# enter function arguments here
)


# Task 3: Plot the data ----------------------------------------------------------------

RT_data <- data.frame(N = 1:100 ,
                      expo = RT_exp ,
                      power = RT_power)

RT_data_long <- RT_data |> pivot_longer(cols=expo:power, names_to = "model", values_to = "RT")

# complete the code block below to show the simulated learning curve for each model
?ggplot()
ggplot(data=RT_data_long, mapping=aes()) +
  geom_point() + 
  geom_line()


# Task 4 ------------------------------------------------------------------
# Discuss: What does the code below do?
# What do you learn from this about the underlying assumptions of the model? 

diff_exp <- rep(NA, times = length(N)-1)
diff_power <- rep(NA, times = length(N)-1)

for(i in 1:99){
  
  diff_exp[i] <-  (RT_exp[i])/(RT_exp[i+1])
  diff_power[i] <-  (RT_power[i])/(RT_power[i+1])
  
}

RT_diff_long <- data.frame(N = 1:99 ,
                      expo = diff_exp ,
                      power = diff_power) |> 
  pivot_longer(cols = expo:power, names_to = "model", values_to = "diff")

ggplot(RT_diff_long, aes(N, diff, color = model))  +
  geom_point() +
  geom_line()

# Task 5 ------------------------------------------------------------
# Discuss: What is the function of each parameter?
# Change the parameters in the simulation to test your intuitions

# add your code here

# Task 6 ------------------------------------------------------------------
# Change the parameters in the simulation such that two learning curves align better with each other?
# Discuss with a partner: What we would be a more efficient way to select parameters?

# add your code here



