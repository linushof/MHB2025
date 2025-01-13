# simulate aDDM behavior

simulate_aDDM <- function(theta=1, bias=.5, scaling=.1, agents, iter){
  
  maxiter <- iter
  D_safe <- matrix(nrow=(maxiter+1), ncol = agents) # evidence safe option 
  D_risky <- matrix(nrow=(maxiter+1), ncol = agents) # evidence risky option 
  D_diff <- matrix(nrow=(maxiter+1), ncol = agents) # relative evidence (safe-risky) 
  
  outcomes <- as.numeric(problem[1,c(2:3)])
  
  for (j in 1:agents){
    
    D_safe[1,j] <- 0 
    D_risky[1,j] <- 0
    D_diff[1,j] <- 0
    
    for (i in 1:maxiter){
      
      # attention
      att <- rbinom(1,1,bias)
      
      # collect and integrate evidence
      
      ## evidence safe option 
      D_safe[i+1,j] <- D_safe[i,j] + problem[1,1]*ifelse(att==1, 1.5,1)*scaling + rnorm(1)*scaling
      
      ## evidence risky option
      smp <- sample(outcomes, 1, prob = problem[1,c(4:5)] ) # draws outcome
      D_risky[i+1,j] <- D_risky[i,j] + smp*ifelse(att==0, 1.5,1)*scaling + rnorm(1)*scaling
      
      ## relative evidence
      
      D_diff[i+1,j] <- D_safe[i+1,j] - D_risky[i+1,j]
      
      # evaluate evidence
      
      if(D_diff[i+1,j] >= theta) {
        
        # choose safe
        choice[j] <- 1
        nsample[j] <- i
        
        break
      }
      if(D_diff[i+1,j] <= -theta) {
        
        # choose risky
        choice[j] <- 0
        nsample[j] <- i
        
        break
      }
      
    }
    
    
  }
  
  return(list(D=D_diff, choice=choice, smp=nsample))
}


# simulate data

## define gambles


# model data with CPT 

logLikelihoodFunction=function(parameters,
                               parameterNames,
                               choice, 
                               problem) {
  
  names(parameters) <-  parameterNames
  
  alpha <-  parameters["alpha"]
  gamma <-  parameters["gamma"]
  delta <-  parameters["delta"]
  rho <- parameters["rho"]
  
  safe <- problem$s
  r_high <- problem$r_high
  r_low <- problem$r_low
  p_r_high <- problem$p_r_high
  p_r_low <- problem$p_r_low
  
  # value function 
  v_safe <-  safe^alpha
  v_r_high <- r_high^alpha 
  v_r_low <- r_low^alpha
  
  
  # weighting function 
  pi_safe <-  1
  pi_r_high <- (delta * p_r_high^gamma) / ((delta * p_r_high^gamma)+p_r_low^gamma)
  pi_r_low <- 1-pi_r_high
  
  # valuation
  Val_safe = pi_safe*v_safe
  Val_risky = pi_r_low*v_r_low + pi_r_high*v_r_high
  
  Val_diff = Val_safe - Val_risky
  
  # stochastic choice rule
  prob_safe = 1/(1+exp((-1*rho)*Val_diff))
  
  
  out <- sum(log(abs(choice-prob_safe)))
  
  return(out)
  
}


# simulate data

problem <- data.frame(s = 10 , 
                      r_high = 12 ,
                      r_low = 8 , 
                      p_r_high = .50 ,
                      p_r_low = .50)


theta <- 8

data <- simulate_aDDM(theta=theta, bias=.53, agents=100, iter=1e3)



# visualize
plot(1:(maxiter+1), data$D[, 1], type = "l", ylim = c(-theta, theta), 
     xlab = "Iterations", ylab = "D_diff", col="lightgray")
for (j in 2:ncol(data$D)) {
  lines(1:(maxiter+1), data$D[, j], col="lightgray")
}
abline(h = c(-theta, 0, theta), lty = "dashed") 


# fit 
data$choice
lowerBounds = c(0, 0, 0, 0)  # Example: All parameters >= 0
upperBounds = c(1, 2, 5, 5)  # Example: Upper limit for each parameter

fit = optim(par = startPar, 
            fn = logLikelihoodFunction, 
            parameterNames = parameterNames, 
            choice = data$choice, 
            problem = problem, 
            method = "L-BFGS-B",  # Use the bounded optimization method
            lower = lowerBounds, 
            upper = upperBounds)
