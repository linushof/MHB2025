# Step 1: Simulate a Diffusion Process 

simDiffusion=function(v,s,dt,maxiter) {
  
  x = rep(NA,maxiter+1)  # creates a vector to store the collected evidence for each time step
  
  x[1] = 0 # before the beginning of the accumulation process, no evidence is accumulated (can be interpreted as starting point)
  
  for (i in 1:maxiter) {# loops over each time step between 1, ..., maxiter
    
    x[i+1] = # to obtain the updated evidence at time i+1 ...
      x[i] + # we take the evidence accumulated until time i ...
      v*dt + # add new evidence according to the drift rate v (dt is a scaling parameter for the size of each time step)
      s*sqrt(dt)*rnorm(1) # scaled noise: s is the unscaled standard deviation
    
  }
  
  return(x)
}

# Simulation

# set parameters for simulation 
v=1 
s=1
dt=.01 # scales the time steps to small values (here: leads to a more continuous diffusion process; also helps with model fitting)
maxiter=1000

# simulate the data
tmp <- simDiffusion(v=v,s=s,dt=dt,maxiter=maxiter)

# plot results
plot(dt*1:(maxiter+1),tmp,xlab="Time",ylab="Evidence",type="l")
tmp[maxiter+1]


# Step 2: Simulate a Diffusion Process with Thresholds and a starting point 


simDiffusion=function(v,a,ter,z,s,dt,maxiter) {
  
  resp = -1 # - 1 is not associated to one of the two options - indicates that no decision is made yet
  rt = -1 # -1 is an impossible response time - also indicates that no decision is made yet
  
  x = rep(NA,maxiter+1)
  
  x[1] = z # z is the starting point
  
  for (i in 1:maxiter) {
    
    x[i+1] = x[i] + v*dt + s*sqrt(dt)*rnorm(1) # evidence accumulation
    
    if (x[i+1] > a) {# at each time step, check if evidence is hit/crossed upper threshold
      resp = 2 # if upper threshold is hit/crossed, choose option 2
      rt = ter + i*dt - dt/2 # calulate response time; ter is the non-decision time; i*dt is the decision time; dt/2 indicates that threshold could be crossed between 2 time points
      break # stops loop if threshold is crossed
    } 
    if (x[i+1] < 0) {
      resp = 1 # if lower threshold is hit/crossed, choose option 1
      rt = ter + i*dt - dt/2
      break
    }
  } # loop continues until threshold is crossed
  return(list(resp=resp,rt=rt,x=x))
}


# set parameters for simulation 
v=1
a=5 # threshold separation
ter=0.3
z=a/2 # no biased starting point
s=1
dt=0.01
maxiter=1000

# simulate data
tmp=simDiffusion(v=v,a=a,ter=ter,z=z,s=s,dt=dt,maxiter=maxiter)

# plot the results
plot(dt*1:(maxiter+1),tmp$x,xlab="Time",ylab="Evidence",type="l",ylim=c(-a/2,a+a/2))
abline(h=a,col="red")
abline(h=0,col="red")

cat("Response =",ifelse(tmp$resp==2,"Correct","Error"),"\n")
cat("Response Time =",tmp$rt,"\n")


