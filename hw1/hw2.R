library(MASS)
require(foreign)
require(dplyr)

sigma <- matrix(data = c(2^2, 0.5*10*2, 0.5*10*2, 10^2), nrow = 2, ncol = 2, byrow = T)
sigma_sqrt <- chol(sigma)
mu <- c(0, 10)

sim_data <- sapply(1:1000, function(i) {
    x <- rnorm(2, mean = 0, sd = 1)     
    as.vector(x %*% sigma_sqrt + mu)
    })
sim_data <- data.frame(t(sim_data))
names(sim_data) <- c("x", "y")

z <- kde2d(x = sim_data$x, y = sim_data$y, n = 100)
plot(sim_data$x, sim_data$y, pch=19, cex=0.4)
contour(z, nlevels = 11, add = T)

x <- c(-0.86, -0.3, -0.05, 0.73)
n <- rep(5, 4)
y <- c(0,1,3,5)






ggplot(sim_data, aes(x=x, y=y)) + geom_point() + stat_density2d(n = 100)

var(sim_data)
