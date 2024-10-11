# code adopted from https://choisy.github.io/DMo2019/sir.html

# load packages
## if required, install package first using install.packages("package")
library(pacman)
library(deSolve)
library(tidyverse)


# simulation
# function sim_change simulates the SIR model using transition rates and initial values as inputs
sim_change <- function(beta, gamma, S0, I0, R0, times) {
  
  
  # SIR model equations
  model <- function(t, groups, rates) {
    with(as.list(c(groups, rates)), {
      dS <- -beta * I * S # susceptibles
      dI <-  beta * I * S - gamma * I   # infected
      dR <-  gamma * I # recovered
      return(list(c(dS, dI, dR)))
    })
  }
  
  # specify transition rates (beta and gamma) as inputs to sim_change()
  rates <- c(beta  = beta, gamma = gamma)
  
  # specify group sizes at time = 0 (S0, I0, R0) as inputs to sim_change()
  inits <- c(S = S0, I = I0, R = R0)
  
  # ode() is a function that solves the system of differential equations of the SIR model
  # takes model equations and inputs to sim_change to solve the equations
  # returns as output group sizes for a series of time points
  out <- ode(inits, times, model, rates)
  
  # output
  as.data.frame(out)
}


# run sim_change (with medium transmissibility)
out_med <- sim_change(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10, .1))
out_med <- out_med %>% mutate(Transmission = "Medium")


# visualize results

out_med %>% 
  pivot_longer(cols = S:R, names_to = "group", values_to = "size") %>% 
  ggplot(aes(x=time, y=size, color=group)) + 
  geom_line(linewidth = 2) + 
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_color_viridis_d(option = "E") +
  labs(x = "Time",
       y = "Agents",
       color = "Group") +
  theme_minimal(base_size = 20)
#ggsave("figures/sir_med.png", width = 10, height = 6)



# Comparison of curves for different transmission rate


# run sim_change (with low transmissibility)
out_low <- sim_change(beta = 0.002, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10, .1))
out_low <- out_low %>% mutate(Transmission = "Low")


# run sim_change (with high transmissibility)
out_high <- sim_change(beta = 0.006, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10, .1))
out_high <- out_high %>% mutate(Transmission = "High")

out <- bind_rows(out_low, out_med, out_high)


# visualize results

out %>% 
  pivot_longer(cols = S:R, names_to = "group", values_to = "size") %>% 
  ggplot(aes(x=time, y=size, color=group)) +
  facet_wrap(~~factor(Transmission,levels=c("Low","Medium","High")), nrow = 1) + 
  geom_line(linewidth = 2) + 
  geom_hline(yintercept = 500, linewidth = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_color_viridis_d(option = "E") +
  labs(x = "Time",
       y = "Agents",
       color = "Group") +
  theme_minimal(base_size = 20)
# ggsave("figures/sir_comparison.png", width = 12, height = 4)

