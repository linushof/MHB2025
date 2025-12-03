#1 simulate data

set.seed(123)
X <- rnorm(100, mean=0, sd=1)
Y <- 3 + 2*X + rnorm(100, mean=0, sd=.5)

#2 write residuals function
ssr <- function(params, X, Y){
  
  intercept <- params[1]
  slope <- params[2]
  Y_pred <- intercept + slope*X
  #2.2 compute residuals
  residuals <- Y_pred - Y
  #2.3 compute sum of squared residuals
  error <- sum(residuals^2)
  
  return(error)
  
}

# compute residuals

#2.4 use ssr() to compute the model fit for different parameter values

ssr(c(2,2), height, weight)

# 3. Minimize SSR with model fitting

# Use optim() to fit the model by minimizing the SSR
?optim # help

fit <- optim(
  par =  c(1,0) ,
  fn = ssr, 
  X = height ,
  Y = weight
)
fit


# likelihood function -----------------------------------------------------

#2 write residuals function
LL <- function(params, X, Y){
  
  intercept <- params[1]
  slope <- params[2]
  Y_pred <- intercept + slope*X
  #2.2 compute likelihood
  nLL <- -1*sum(dnorm(Y, mean=Y_pred, sd=params[3], log=TRUE))
  #2.3 compute sum of squared residuals
  

  return(nLL)
  
}


fit <- optim(
  par =  c(1,0,1) ,
  fn = LL, 
  X = height ,
  Y = weight , 
  lower = c(0,0,0) , 
  method = 'L-BFGS-B'
)

# reinforcement learning model --------------------------------------------


set.seed(765)

# True parameters
alpha_true <- 0.25    # learning rate
beta_true  <- 3.0     # inverse temperature

# Task setup: 100 trials, two actions
n_trials <- 1000
Q <- c(0, 0)          # initial action values
choices <- rewards <- numeric(n_trials)

softmax <- function(Q, beta) {
  exp(beta * Q) / sum(exp(beta * Q))
}

for (t in 1:n_trials) {
  
  # choice probabilities
  p <- softmax(Q, beta_true)
  
  # simulate choice
  choices[t] <- sample(1:2, 1, prob = p)
  
  # generate reward (arm 1 is better)
  rewards[t] <- ifelse(choices[t] == 1, rbinom(1, 1, 0.7), rbinom(1, 1, 0.3))
  
  # prediction error and update
  pe <- rewards[t] - Q[choices[t]]
  Q[choices[t]] <- Q[choices[t]] + alpha_true * pe
}

data <- data.frame(trial = 1:n_trials, choice = choices, reward = rewards)


### ---------------------------------------------
### 2. Log-likelihood function for optim()
### ---------------------------------------------

negLL <- function(par, data) {
  alpha <- par[1]
  beta  <- par[2]
  
  # keep params within bounds
  if(alpha < 0 || alpha > 1 || beta < 0) return(1e10)
  
  Q <- c(0, 0)
  ll <- 0
  
  for (t in 1:nrow(data)) {
    p <- exp(beta * Q) / sum(exp(beta * Q))
    ll <- ll + log(p[data$choice[t]])
    
    pe <- data$reward[t] - Q[data$choice[t]]
    Q[data$choice[t]] <- Q[data$choice[t]] + alpha * pe
  }
  
  return(-ll)   # optim minimizes
}


### ---------------------------------------------
### 3. Recover parameters using optim()
### ---------------------------------------------

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
