# Function for creating choice problems   ---------------------------------------

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


#1 use generate_problems() to generate choice problems
n <- 500
set.seed(3425)
problems <- generate_problems(n, safe = FALSE, 0, 100)
head(problems, 10)



#2 compute and compare expected values

#2.1 define expected value function
ev <- function() {
  #compute expected values 
}


#2.2 prepare data set with empty columns that should be filled 
dat1 <- problems
dat1$ev_a1 <- rep(NA, n)
dat1$ev_a2 <- rep(NA, n)
dat1$ev_ratio <- rep(NA, n)
dat1$better_ev <- rep(NA, n)

head(dat1)

#2.1 for each problem (row) at a time, compute fill the missing columns

for(i in seq_along(1:n)) {
  
  # Evaluation process
  
  # expected values
  dat1[[i, "ev_a1"]] <- # ev option 1
  dat1[[i, "ev_a2"]] <- # ev option 2
  
  # Comparison
  
  dat1[[i, "ev_ratio"]] <- # compute ration of ev
  dat1[[i, "better_ev"]] <- ifelse(, 1, 2) # determine which option maximizes the EV (using ev_ratio)
}

head(dat1, 10)



#3 compute expected utility and choice probabilities 

#3.1 define expected utility function function
eu <- function() {

}


#3.2 prepare data set with empty columns that should be filled 
dat1$eu_a1 <- rep(NA, n)
dat1$eu_a2 <- rep(NA, n)
dat1$eu_diff <- rep(NA, n)
dat1$choice_prob <- rep(NA, n)
dat1$choice <- rep(NA, n)
dat1$max <- rep(NA, n)


beta <- # set parameter for beta (between 0 and 1)
phi <- #  set parameter for beta (larger or equal to 0 and 1)

for(i in seq_along(1:n)) {
  
  
  # expected utilities 
  dat2[[i, "eu_a1"]] <- # compute expected utility of option 1
  dat2[[i, "eu_a2"]] <- # compute expected utility of option 2
  
  # Comparison
  
  dat2[[i, "eu_diff"]] <- # compute difference in expected utilities
  dat2[[i, "choice_prob"]] <- # use the logit choice rule to obtain the probability of choosing A1 over A2
  dat2[[i, "choice"]] <- ifelse(rbinom(1,1,dat2[[i, "choice_prob"]]) == 1, 1, 2) # simulate choice based on choice_prob
  dat2[[i, "max"]] <- ifelse(, 1, 0) # check if the expected value is maximized
}


