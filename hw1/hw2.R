library(MASS)
require(foreign)
require(dplyr)
require(ggplot2)
require(mvtnorm)
require(logitnorm)

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

y/n
qlogis(y/n)
lm(qlogis(y/n)[2:3]~x[2:3])


glm(qlogis(y/n)~x )

ggplot(sim_data, aes(x=x, y=y)) + geom_point() + stat_density2d(n = 100)

var(sim_data)

new_y <- c(rep(0,5), c(1,0,0,0,0), c(1,1,1,0,0), rep(1,5))
new_x <- unlist(lapply(x, function(x){
    rep(x,5)
}))

glm(formula = new_y~new_x, family=binomial(logit))


#### messing around with number is done, work on the real problem set.

# set up the prior of normal N{ mu=(0,10), var = (2^2, 2*10*0.5, 2*10*0.5, 10^2)
# sample from the 2-D normal prior
mu <- c(0, 10)
sigma <- matrix(data = c(2^2, 0.5*10*2, 0.5*10*2, 10^2), 
                nrow = 2, ncol = 2, byrow = T)

norm_sim <- mvrnorm(n = 1000, mu = mu, Sigma = sigma)
norm_sim <- data.frame(norm_sim)
names(norm_sim) <- c("alpha", "beta")

ggplot(norm_sim, aes(x=alpha, y=beta)) + geom_point() + stat_density2d(n = 100)


# sampling from the joint posterior density ---Uniform 
post_den1 <- function(a, b ,y ,n, x){
    prod((plogis(a+b*x))^y*(1-plogis(a+b*x))^(n-y))
}

post_den2 <- function(alpha, beta, y, n, x){
    prod(dbinom(x = y, size = n, prob = invlogit(a+b*x)))
}

log_post_den <- function(a,b,y,n,x){
    sum(dbinom(y, n, invlogit(a+b*x), log = T))
}

a <- 0.8
b <- 7.7
beta *x 
post_den1(a,b,y,n,x)
post_den2(a,b,y,n,x)
exp(log_post_den(a,b,y,n,x))

alpha <- seq(from = -5, to = 10, by = 0.1)
beta <- seq(from = -10, to = 40, by = 0.1)

# crate grid of alpha & beta 
sim <- expand.grid(alpha, beta)

# posterior density with simulated prior on the grid
post_sim <- apply(sim, MARGIN = 1, function(row){
     log_post_den(a = row[1], b = row[2], y = y, n = n, x = x)
})

# scaling the sensity so that it's from 0 to 1 scale
dens <- exp(post_sim - max(post_sim))
# turn the density into matrix so that contour plot fucntion can consume
dens <- matrix(dens, nrow = 151, byrow = F)

# here comes the contuour plot with uniform prior
contour(x= alpha,
        y= beta,
        z = dens,
        levels = seq(from = 0.05, to=0.95, by = 0.05))


## sample from the joint posterior 
# numerical summing over beta to get marginal density regarding alpha
adj_post_den <- dens/sum(dens)
alpha_marg_dens <- rowSums(adj_post_den)

# sample from p(alpha|n,x,y)
alpha_s <- sample(x = alpha, size = 1000, prob = alpha_marg_dens, replace = T)

# calculate pmf of beta given alpha
condi_beta_dens <- adj_post_den/rowSums(adj_post_den)

# every row sums to 1 now
rowSums(condi_beta_dens)

# index of row in density matrix
idx <- sapply(alpha_s, function(x){
    which(alpha == x)
})
# getting conditonal probability of beta given alph and sample correspondingly
beta_s <- sapply(idx, function(x){
    sample(beta, size = 1, prob =  condi_beta_dens[x,])
})

# adding unif jitter
alpha_jitter <- runif(1000, min = -0.1, max = 0.1)
beta_jitter <- runif(1000, min = -0.1, max = 0.1)

alpha_s <- alpha_s + alpha_jitter
beta_s <- beta_s + beta_jitter

plot(alpha_s, beta_s)



## NORMAL EXERCISE
# set up the prior of normal N{ mu=(0,10), var = (2^2, 2*10*0.5, 2*10*0.5, 10^2)
# sample from the 2-D normal prior
mu <- c(0, 10)
sigma <- matrix(data = c(2^2, 0.5*10*2, 0.5*10*2, 10^2), 
                nrow = 2, ncol = 2, byrow = T)

norm_sim <- mvrnorm(n = 1000, mu = mu, Sigma = sigma)
norm_sim <- data.frame(norm_sim)
names(norm_sim) <- c("alpha", "beta")

ggplot(norm_sim, aes(x=alpha, y=beta)) + geom_point() + stat_density2d(n = 100)

alpha <- seq(from = -5, to = 10, by = 0.1)
beta <- seq(from = -10, to = 40, by = 0.1)

# crate grid of alpha & beta 
sim_data <- expand.grid(alpha, beta)

# sampling from the joint posterior density ---NORMAL 
log_post_den <- function(alpha,beta, mu, sigma, y,n,x){
    sum(dmvnorm(x = c(alpha,beta), mean = mu, sigma, log = T) + 
        dbinom(y, n, invlogit(alpha + beta*x), log = T))
}

# posterior density with simulated prior on the grid
post_sim <- apply(sim_data, MARGIN = 1, function(row){
    log_post_den(a = row[1], b = row[2], mu =mu, sigma = sigma,
                 y = y, n = n, x = x)
})

# scaling the sensity so that it's from 0 to 1 scale
dens <- exp(post_sim - max(post_sim))
# turn the density into matrix so that contour plot fucntion can consume
dens <- matrix(dens, nrow = length(alpha), byrow = F)


# here comes the contuour plot with uniform prior
contour(x= alpha,
        y= beta,
        z = dens,
        levels = seq(from = 0.05, to=0.95, by = 0.05))
