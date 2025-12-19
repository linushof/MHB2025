# Model 1: EV Maximization and Expected Utility  ---------------------------------------

# Function for creating choice problems 

# Function for generating 2-alternative choice problems with at-most 2 outcomes
generate_problems <- function(n, safe = TRUE, lower, upper) {
  problems <- vector("list", n)
  
  # Safe vs. risky gambles
  if (safe == TRUE) {
    for (i in 1:n) {
      # Generate random outcomes for the safe and risky options
      outcomes <- c(runif(3, min = lower, max = upper))
      outcomes <- round(sort(outcomes), 2)
      probability <- round(runif(1, min = .01, max = .99),2)
      
      
      # Create a list for each gamble
      gamble <- list(
        a1_x1 = outcomes[1],
        a1_x2 = outcomes[3],
        a2_safe = outcomes[2], 
        a1_p1 = probability,
        a1_p2 = round(1 - probability,2)
      )
      
      problems[[i]] <- gamble
    }
  } else {
    # Risky vs. risky gambles
    for (i in 1:n) {
      # Generate random outcomes for the risky options
      outcomes <- c(runif(3, min = lower, max = upper)) 
      outcomes <- round(sort(outcomes), 2)
      outcomes <- c(outcomes, round( runif(1, min = lower, max = upper), 2 )) 
      probabilities <- round(runif(2, min = .01, max = .99), 2)
      
      
      # Create a list for each gamble
      gamble <- list(
        a1_x1 = outcomes[1],
        a1_x2 = outcomes[3],
        a2_x1 = outcomes[2],
        a2_x2 = outcomes[4],
        a1_p1 = probabilities[1],
        a1_p2 = round(1-probabilities[1], 2),
        a2_p1 = probabilities[2],
        a2_p2 = round(1-probabilities[2],2)
      )
      
      problems[[i]] <- gamble
    }
  }
  
  # Convert the list of problems to a data frame
  problems_df <- as.data.frame(do.call(rbind, problems))
  
  return(problems_df)
}


# Set parameters
n <- 500
set.seed(3425)
problems <- generate_problems(n, safe = FALSE, 0, 100)
head(problems, 10)



# EV maximization 

# define expected value function
ev <- function(p1, x1, x2) {
  round(p1*x1 + (1-p1)*x2, 2) 
}


dat1 <- problems

dat1$ev_a1 <- rep(NA, n)
dat1$ev_a2 <- rep(NA, n)
dat1$better_ev <- rep(NA, n)
dat1$ev_ratio <- rep(NA, n)
dat1$choice <- rep(NA, n)
dat1$max <- rep(NA, n)


for(i in seq_along(1:n)) {
  
  # Evaluation process
  
  # expected values
  dat1[[i, "ev_a1"]] <- ev(problems[[i, "a1_p1"]], problems[[i, "a1_x1"]], problems[[i, "a1_x2"]])
  dat1[[i, "ev_a2"]] <- ev(problems[[i, "a2_p1"]], problems[[i, "a2_x1"]], problems[[i, "a2_x2"]])
  dat1[[i, "better_ev"]] <- ifelse(dat1[[i, "ev_a1"]] > dat1[[i, "ev_a2"]], 1, 
                                   ifelse(dat1[[i, "ev_a1"]] < dat1[[i, "ev_a2"]], 2, NA))
  
  # Comparison
  
  dat1[[i, "ev_ratio"]] <- dat1[[i, "ev_a1"]] / dat1[[i, "ev_a2"]]
  dat1[[i, "choice"]] <- ifelse(dat1[[i, "ev_ratio"]] > 1, 1, ifelse(dat1[[i, "ev_ratio"]] < 1, 2,NA)) 
  dat1[[i, "max"]] <- ifelse(dat1[[i, "choice"]] == dat1[[i, "better_ev"]], 1, 0)
}

head(dat1, 10)



# EU maximization and stochastic choice 

# define expected value function
eu <- function(p1, x1, x2, beta) {
  round(p1*(x1^beta) + (1-p1)*(x2^beta), 2) 
}

dat2 <- problems


dat2$ev_a1 <- rep(NA, n)
dat2$ev_a2 <- rep(NA, n)
dat2$better_ev <- rep(NA, n)
dat2$eu_a1 <- rep(NA, n)
dat2$eu_a2 <- rep(NA, n)
dat2$eu_diff <- rep(NA, n)
dat2$choice_prob <- rep(NA, n)
dat2$choice <- rep(NA, n)
dat2$max <- rep(NA, n)


beta <- .4
phi <- 1

for(i in seq_along(1:n)) {
  
  # Evaluation process
  
  # expected values
  dat2[[i, "ev_a1"]] <- ev(problems[[i, "a1_p1"]], problems[[i, "a1_x1"]], problems[[i, "a1_x2"]])
  dat2[[i, "ev_a2"]] <- ev(problems[[i, "a2_p1"]], problems[[i, "a2_x1"]], problems[[i, "a2_x2"]])
  dat2[[i, "better_ev"]] <- ifelse(dat2[[i, "ev_a1"]] > dat2[[i, "ev_a2"]], 1, ifelse(dat2[[i, "ev_a1"]] < dat2[[i, "ev_a2"]], 2, NA))
  
  # expected utilities 
  dat2[[i, "eu_a1"]] <- eu(problems[[i, "a1_p1"]], problems[[i, "a1_x1"]], problems[[i, "a1_x2"]], beta)
  dat2[[i, "eu_a2"]] <- eu(problems[[i, "a2_p1"]], problems[[i, "a2_x1"]], problems[[i, "a2_x2"]], beta)
  
  # Comparison
  
  dat2[[i, "eu_diff"]] <- dat2[[i, "eu_a1"]] - dat2[[i, "eu_a2"]]
  dat2[[i, "choice_prob"]] <- round(1 / (1 + exp(-phi*  dat2[[i, "eu_diff"]])), 2)
  dat2[[i, "choice"]] <- ifelse(rbinom(1,1,dat2[[i, "choice_prob"]]) == 1, 1, 2)
  dat2[[i, "max"]] <- ifelse(dat2[[i, "choice"]] == dat2[[i, "better_ev"]], 1, 0)
}

head(dat2, 10)
mean(dat1$max)
mean(dat2$max)

