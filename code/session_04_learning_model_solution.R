library(tidyverse)

# Task 1: Define the models --------------------------------------------------------

expo <- function(A , # additive constant 
                 B , # multiplicative constant / scale
                 alpha , # learning rate
                 N # number of learning trials
){
  
  pred <- A + B * exp(-alpha*N)
  return(pred)
  
}


power <- function(A, B, beta, N){
  
  pred <- A + B * (N+1)^(-beta)
  return(pred)
  
}


# Task 2: Simulate data -----------------------------------------------------------
# use the expo/power function to simulate response times (RT)
# for N= 1, 2, ..., 100 learning trials 

# Use the following values for simulations
# A = 0, B = 1, alpha/beta = .5

N <- 1:100

RT_exp <- expo(A=0, B=1, alpha=.5, N)
RT_power <- power(A=0, B=1, beta=.5, N)


# Task 3: Plot the data ----------------------------------------------------------------

RT_data <- data.frame(N = 1:100 ,
                      expo = RT_exp ,
                      power = RT_power)

RT_data_long <- RT_data |> pivot_longer(cols=expo:power, names_to = "model", values_to = "RT")

# complete the code block below to show the simulated learning curve for each model

ggplot(data=RT_data_long, mapping=aes(x=N, y=RT, color=model)) +
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

'
Solution: The code computes the relative change in performance for each additional trial.
According to the exponential law, the relative improvement in performance compared to a previous trial is a constant proportion. 
According to the power law, the relative improvement decreases over time, i.e., the relative effectiveness of training decreases with each additional trial.
'

# Task 5 ------------------------------------------------------------
# Discuss: What is the function of each parameter?
# Change the parameters in the simulation to test your intuitions


# Task 6 ------------------------------------------------------------------
# Change the parameters in the simulation such that two learning curves align better with each other?
# Discuss with a partner: What we would be a more efficient way to select parameters?

'Solution: One way to make the graphs align more would be to fit the power model to the data simulated from an exponential model.
The predictions of the power model with the fitted parameters should capture the data generated from the exponential model relatively well.
'

# predict data from the exponential model
N <- 1:100
RT_exp <- expo(A=0, B=1, alpha=.1, N)
RT_exp 


# define a objective/loss function which quantifies the distance between observed and predicted data for model fitting
# here we use the sum of squared residual approach

ssr_power <- function(param, N, RT){
  
  A <- param[1]
  B <- param[2]
  beta <- param[3]
  pred <- power(A, B, beta, N)
  error <- RT - pred
  ssr <- sum(error^2)
  return(ssr)
  
}

# we now minimize the ssr. I.e., we find the parameters of the power model, which lead to predictions 
# that have the minimal distance to the data generated from the exponential model
start_vals = c(1,1,1)
power_fit <- optim(
  par = start_vals , 
  fn = ssr_power ,
  N = N ,
  RT = RT_exp
)


# We generate new data from the power model with the fitted parameters
est <- power_fit$par
RT_power <- power(A=est[1], B=est[2], beta=est[3], N)

# the generated curve should align (more or less) with the curve from the exponential model
RT_data <- data.frame(N = 1:100 ,
                      expo = RT_exp ,
                      power = RT_power)

RT_data_long <- RT_data |> pivot_longer(cols=expo:power, names_to = "model", values_to = "RT")


ggplot(data=RT_data_long, mapping=aes(x=N, y=RT, color=model)) +
  geom_point() + 
  geom_line()

