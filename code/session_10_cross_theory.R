
# example decision problem --------------------------------------------------------

problem <- data.frame(s = 10 , 
                      r_high = 12 ,
                      r_low = 8 , 
                      p_r_high = .50 ,
                      p_r_low = .50)
print(problem)


# function to simulate the aDDM -------------------------------------------------------------------------

simulate_aDDM <- function(theta=1 , # decision threshold
                          bias=.5 , # attentional bias (.5 = no bias, >(<) .5 safe (risky) bias) 
                          scaling=.1 , # scaling of outcome values
                          agents , # number of simulated agents
                          iter, # maximum length of a decision process (number of iterations)
                          problems){ # data frame with choice problems  
  
  # storage space
  maxiter <- iter
  D_safe <- matrix(nrow=(maxiter+1), ncol = agents) # evidence safe option 
  D_risky <- matrix(nrow=(maxiter+1), ncol = agents) # evidence risky option 
  D_diff <- matrix(nrow=(maxiter+1), ncol = agents) # relative evidence (safe-risky) 
  choice <- numeric(agents)
  nsample <- numeric(agents)
  
  # get information on the choice problem 
  
  safe <- problems[1,1] # safe outcome
  outcomes <- as.numeric(problems[1,c(2:3)]) # risky outcomes
  probs <- as.numeric(problems[1,c(4:5)]) # probabilities of risky outcomes
  
  for (j in 1:agents){ # loop to run the decision process multiple times
    
    D_safe[1,j] <- 0 # at the start of the decision process, no evidence is collected
    D_risky[1,j] <- 0
    D_diff[1,j] <- 0
    
    # start of the accumulation process
    for (i in 1:maxiter){
      
      # option attended in an iteration
      att <- rbinom(1,1,bias) # bias = probability of 1 (safe option)
      
      # collect and integrate evidence
      
      ## evidence safe option 
      D_safe[i+1,j] <- D_safe[i,j] + safe*ifelse(att==1, 1,1)*scaling + rnorm(1)*scaling
      
      ## evidence risky option
      smp <- sample(outcomes, 1, prob = probs ) # draws outcome
      D_risky[i+1,j] <- D_risky[i,j] + smp*ifelse(att==0, 1,1)*scaling + rnorm(1)*scaling
      
      ## relative evidence
      D_diff[i+1,j] <- D_safe[i+1,j] - D_risky[i+1,j]
      
      # evaluate evidence / check if threshold is reached - if yes, make a choice
      
      if(D_diff[i+1,j] >= theta) {
        
        # positive threshold --> choose safe (1)
        choice[j] <- 1
        nsample[j] <- i
        
        break
      }
      if(D_diff[i+1,j] <= -theta) {
        
        # negative threshold --> choose risky option (0)
        choice[j] <- 0
        nsample[j] <- i
        
        break
      }
      
    }
    
    
  }
  
  return(list(D=D_diff, choice=choice, smp=nsample))
}


# function to fit CPT -----------------------------------------------------


logLikelihoodFunction=function(parameters, # CPT parameters that should be fitted
                               parameterNames,
                               choice, # simulated choices
                               problem) { # problems underlying the choices
  
  names(parameters) <-  parameterNames
  
  # get parameter values

  alpha <-  parameters["alpha"]
  gamma <-  parameters["gamma"]
  delta <-  parameters["delta"]
  rho <- parameters["rho"]
  
  # get information on the choice problem 
  safe <- problem$s
  r_high <- problem$r_high
  r_low <- problem$r_low
  p_r_high <- problem$p_r_high
  p_r_low <- problem$p_r_low
  
  # compute CPT with the given problem information and parameter values
  
  # value function is applied to all outcomes 
  v_safe <-  safe^alpha
  v_r_high <- r_high^alpha 
  v_r_low <- r_low^alpha
  
  
  # weighting function (only needs to be applied to the higher risky outcome, see Zilker and Pachur, p. 954)
  # weighting function based on the specification of Goldstein and Einhorn (high delta = high elevation)
  pi_safe <-  1
  pi_r_high <- (delta * p_r_high^gamma) / ((delta * p_r_high^gamma)+p_r_low^gamma)
  pi_r_low <- 1-pi_r_high
  
  # additive-multiplicative combination of subjective values and probabilities
  Val_safe = pi_safe*v_safe
  Val_risky = pi_r_low*v_r_low + pi_r_high*v_r_high
  
  # logit choice rule (computes choice probability for safe, given the CPT valuations and free parameter rho)
  Val_diff = Val_safe - Val_risky
  prob_safe = 1/(1+exp((-1*rho)*Val_diff))
  
  # compute log likelihood based on predicted and observed choice
  # value that should be minimized by optim
  out <- sum(log(abs(choice-prob_safe)))
  
  return(out)
  
}



# simulate decision processes ---------------------------------------------

theta <- 5
bias <- .5
iter <- 1e3
# set.seed(1252)
data <- simulate_aDDM(theta=theta, bias=bias, agents=100, iter=1e3, problems = problem)

data$D

# results -----------------------------------------------------------------

# visualize accumulation trajectories

plot(1:(iter+1), data$D[, 1], type = "l", ylim = c(-theta, theta), 
     xlab = "Iterations", ylab = "D_diff", col="lightgray")
for (j in 2:ncol(data$D)) {
  lines(1:(iter+1), data$D[, j], col="lightgray")
}
abline(h = c(-theta, 0, theta), lty = "dashed") 


# fit choices from aDDM with CPT

## allowed ranges for CPT parameters
lowerBounds = c(0.01, 0.01, 0.01, 0.01)  # All parameters >= 0
upperBounds = c(1, 2, 5, 5)  # Upper limit for each parameter
startPar = c(1,1,1,1)
parameterNames = c('alpha', 'gamma', 'delta', 'rho')

fit = optim(par = startPar, 
            fn = logLikelihoodFunction, 
            parameterNames = parameterNames, 
            choice = data$choice, 
            problem = problem, 
            method = "L-BFGS-B",  # Use the bounded optimization method
            lower = lowerBounds, 
            upper = upperBounds)




# visualize

gamma <- fit$par[2]
delta <- fit$par[3]
p <- seq(0,1,.001)
wp <- (delta * p^gamma) / ((delta * p^gamma)+(1-p)^gamma)
wp
plot(p, wp, type = "l", xlim = c(0, 1), ylim = c(0, 1))
abline(a=0, b=1, lty="dashed")
