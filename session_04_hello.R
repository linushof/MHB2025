# Exercise 1

x <- 10
(((x + 2) * 3) - 6) / 3

# Exercise 2

o1 <- 10
o2 <- o1 + 2
o3 <- o2 * 3
o4 <- o3 - 6
o5 <- o4 / 3

o5*3 == o4
o4 + 6 == o3
o3 / 3 == o2
o2 - 2 == o1

# Exercise 3

x <- 1:5
(((x + 2) * 3) - 6) / 3

# Exercise 4 

n_obs <- 5
dat <- data.frame(v1 = rnorm(n_obs, mean=1),
                  v2 = rnorm(n_obs, mean=2),
                  v3 = rnorm(n_obs, mean=3),
                  v4 = rnorm(n_obs, mean=4),
                  v5 = rnorm(n_obs, mean=5))

c(mean(dat[,1]), mean(dat[,2]), mean(dat[,3]), mean(dat[,4]), mean(dat[,5]))
sapply(dat, mean)
?sapply()
colMeans(dat)


