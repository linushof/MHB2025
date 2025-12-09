# Task 1: Simulate a diffusion process without threshould -----------------


# Step 1: Write a diffusion process function

simDiffusion=function(v , # drift rate
                      dt , # delta t (time step size)
                      e , # Gaussian noise  
                      maxiter # number of iterations aka. duration of the process, 
                      ) {
  
  # vector to store the accumulated evidence at each time step (starts with 0)
  x <- rep(NA, maxiter+1)
  x[1] <- 0
  
  ####  Evidence accumulation starts here #####
  for (i in 1:maxiter) {# in each iteration of the loop, evidence should be updated (i.e., fill the remaining values of x vector)
    
    # define the diffusion model here, using v, dt, e
    
  }
  
  return(x)
}

# Step 2: Simulate the diffusion process using 1000 iterations
v <- 1
dt <- .1
e <- 1
maxiter <- 1000
evidence <- simDiffusion(v, dt, e, maxiter)

# Step 3: Plot the evidence accumulation trajectory


# Step 4: simulate with different values of v, e, dt to see how the diffusion process changes
# What do you observe? 



# Task 2: Simulate a Diffusion Process with Thresholds and a starting point  ------------------------------------------------------------------

# Step 1: Write a function for simulation a diffusion process 

simDiffusion=function(v , # drift rate
                      a , # threshold separation (upper threshold)
                      ter , # non-decision time
                      z , # starting point
                      e , # Gaussion noise
                      dt , # delta t (time step size)
                      maxiter # maximum number of iterations (if threshold was not hit earlier)
                      ) {
  
  ##### set initial settings #### 
  resp = -1 # resp is the final choice; should be 1 if upper threshold is hit, 2 if lower threshold is hit (-1 is just a placeholder)
  rt = -1 # rt is the response time (the time when a theshold is hit): -1 is also a placeholder
  x <-  rep(NA,maxiter+1) # accumulated evidence
  x[1] <- # start point
  
  #####  Evidence accumulation starts here #####
  for (i in 1:maxiter) {
    
    # define the diffusion model to update the evidence
    
    
    # after each iteration, check if sampled evidence has reached upper threshold 
    if (x[i+1] > a) {# when upper threshold is hit
      
      #1 update response 
      
      #2 calculate response time
      
      #3 stop the loop (diffusion process)
      
    } # if upper threshold was not hit, check if sampled evidence has reached lower threshold 
    if (x[i+1] < 0) {# when lower threshold is hit
      
      #1 update response if threshold was hit
      
      #2 calculate response time
      
      #3 stop the loop
    }
  }
  return(list(resp=resp,rt=rt,x=x)) # return key metrics of the process
}


# Step 2: Simulate the diffusion process with using 1000 iterations
v=1
a=5
ter=0.3
z=a/2
s=1
dt=0.01
maxiter=1000
evidence <- simDiffusion(v=v,a=a,ter=ter,z=z,s=s,dt=dt,maxiter=maxiter)

# Step 3: Plot the evidence accumulation trajectory and look at the response and response time

# Step 4: Vary the parameters of the model and observe how it behaves
