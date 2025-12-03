
# Task 1: Linear model and sum of squared residuals  ------------------------------------------------------------------


# Data simulation ---------------------------------------------------------

## simulate data from a regression model
set.seed(123)
X <- rnorm(100, mean=0, sd=1)
Y <- 3 + 2*X + rnorm(100, mean=0, sd=.5)
plot(X,Y)



## model fitting -----------------------------------------------------------

# complete the function below, which should estimate the squared sum of residuals
ssr <- function(params, # vector with candidate values for the parameters 
                X, # observed x value (simulated data)
                Y # observed y values (simulated data)
                ){  
  
  # get candidate parameter values from param 
  intercept <- params[1] 
  slope <- params[2]
  
  # compute the predicted values from the model using the intercept and slope object
  Y_pred <- 
    
  # compute the individual residuals based on Y_pred
  residuals <- 
  
  # compute total error (sum of squared residuals) based on residuals
  error <- 
  
  return(error)
  
}


#3 Check if the ssr() works (returns a sum of squared residuals)
# using the candidate parameter values intercept=2 and slope=2
ssr(c(2,2), X, Y)


#4 Use the optim function to find the parameter values that minimize the SSR for the simulated data (X and Y)
# Read the optim help page and complete the missing arguments

?optim # see help page

fit <- optim(
  par =  c(1,0) ,
  fn = ssr, 
  ... 
)
fit$par



# Task 2: Linear model with maximum likelihood ------------------------------------------------------------------


#5 complete the function below to estimate the negative log-likelihood
LL <- function(params, X, Y){
  
  intercept <- params[1]
  slope <- params[2]
  sigma <- params[3]
  
  # compute the predicted mean value from the model
  Y_pred <- 
 
  # compute the log-likelihood for each data point
  # dnorm gives you the probability of each value in a vector given a mean and sigma 
  LL <- dnorm(... , log=TRUE)
  
  # compute the negative log likelihood over the entire data
  nLL <- 
  return(nLL)
  
}

# Use the optim function to find the parameter values that maximize the likelihood

fit <- optim(

)
fit$par


# Task 3: Reinforcement learning model with softmax (binomial likelihood) --------------------------------------------

## data simulation ---------------------------------------------------------


### preparation  ------------------------------------------------------------

# select model parameters for simulation
alpha <- 0.25    # learning rate
beta  <- 3     # temperature (sensitivity)
n_trials <- 100

# initialize data objects
Q <- c(0, 0)          
choices <- rep(0, n_trials) 
rewards <- rep(0, n_trials)

# choice rule (computes choice probability based on Q values)
softmax <- function(Q, beta) {
  exp(beta * Q) / sum(exp(beta * Q))
}


### run simulation ----------------------------------------------------------

set.seed(123)

for (t in 1:n_trials) {# run model for n_trials trials
  
  # compute choice probability for t-th choice based on the current Q values
  p <- softmax(Q, beta)
  
  # simulate t-th choice based on the choice probability
  choices[t] <- sample(1:2, 1, prob = p) 
  
  # generate reward for chosen option
  rewards[t] <- ifelse(choices[t] == 1, rbinom(1, 1, 0.7), rbinom(1, 1, 0.3))
  
  # update the Q value based on the prediction error
  pe <- rewards[t] - Q[choices[t]]
  Q[choices[t]] <- Q[choices[t]] + alpha * pe
}

# safe simulated data
data <- data.frame(trial = 1:n_trials, choice = choices, reward = rewards)



### fit the model -----------------------------------------------------------


# complete the function to obtain the negative log-likelihood for the entire model

negLL <- function(par, data) {
  alpha <- par[1]
  beta  <- par[2]
  
  # keep params within bounds
  if(alpha < 0 || alpha > 1 || beta < 0) return(1e10)
  
  Q <- c(0, 0)
  ll <- 0
  
  for (t in 1:nrow(data)) {
   
    # compute the negative log likelihood for each choice based on the Q values (which must be updated)
    
  }
  
  return(nll)   
}


# Estimate the parameters and see whether they are recovered
start_vals <- c(alpha = 0.5, beta = 1.0)

fit <- optim(
  par = start_vals,
  fn = negLL,
  data = data,
  method = "L-BFGS-B",
  lower = c(0, 0),
  upper = c(1, 20)
)

fit$par


# Change N, alpha and beta in the model simulation to see how it affects parameter recovery
