# define gambles

problem <- data.frame(s = 10 , 
                      r_high = 12 ,
                      r_low = 8 , 
                      p_r_high = .60 ,
                      p_r_low = .40)


# simulate aDDM behavior


simulate_aDDM <- function(theta=1, bias=1, scaling=.1, iter){
  
  maxiter <- iter
  D_safe <- c(0, rep(NA, maxiter)) # evidence safe option 
  D_risky <- c(0, rep(NA, maxiter)) # evidence risky option 
  D_diff <- c(0, rep(NA, maxiter)) # relative evidence (safe-risky) 
  
  
  for (i in 1:maxiter){
    
    # attention
    att <- rbinom(1,1,bias)
    
    # collect and integrate evidence
    
    ## evidence safe option 
    D_safe[i+1] <- D_safe[i] + problem[1,1]*ifelse(att==1, 1.1,1)*scaling + rnorm(1)*scaling
    
    ## evidence risky option
    outcome <- sample(problem[1,c(2:3)], 1, prob = problem[1,c(4:5)] ) # draws outcome
    D_risky[i+1] <- D_risky[i] + outcome*ifelse(att==0, 1.1,1)*scaling + rnorm(1)*scaling
    
    ## relative evidence
    
    D_diff[i+1] <- D_safe[[i+1]] - D_risky[[i+1]]
    
    # evaluate evidence
    
    if(D_diff[i+1] >= theta) {
      
      # choose safe
      choice <- 1
      nsample <- i
      
      break
    }
    if(D_diff[i+1] <= -theta) {
      
      # choose risky
      choice <- 0
      nsample <- i
      
      break
    }
    
  }
  
  
  
  
  return(list(D=D_diff, choice=choice, smp=nsample))
}

test <- simulate_aDDM(iter = 1000)


plot(1:(maxiter+1), D_diff, type = "l", ylim=c(-theta,theta))
abline(h = c(-theta, 0, theta), lty = "dashed")  

