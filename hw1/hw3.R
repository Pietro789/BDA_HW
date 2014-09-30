# theory
post_log_dens <- function(theta, y){
    (-1) * sum(log(1 + (y-theta)^2))    
}

y <- c(-2, -1, 0, 1.5, 2.5)

theta <- seq(0, 1, by = 0.001)

post_log_dens(theta = 0.5, y)

sum(sapply(y, function(yy){
    post_log_dens(theta = 0.5, yy)
}))

theta_post_den <- sapply(theta, function(t){
    post_log_dens(t, y)
})

theta_post_den_adj <- exp(theta_post_den-max(theta_post_den))
plot(theta, theta_post_den_adj)

mean(y)

#2 
x <- 1:10
u <- rep(c(0.1, 0.5), c(5,5))
alpha <- 1
beta <- 1.3

# simulate from model
lambda <- u * exp(alpha + beta*x)
y <- sapply(lambda, function(l){
    rpois(n = 1, lambda = l)    
})

# plot y/u vs x
plot(x, y/u)

# set up post density function
post_log_dens <- function(alpha, beta, x, u, y){
    sum(y*log(u) + y*(alpha + beta*x) - u*exp(alpha+beta*x))
}

post_log_dens <- function(alpha, beta, x, u, y){
    sum(dpois(y, u*exp(alpha + beta*x), log = T))
}


alpha <- seq(0.95, 1.05, by = 0.0005)
beta <- seq(1.292, 1.305, by = 0.00005)

# crate grid of alpha & beta 
sim <- expand.grid(alpha, beta)

# posterior density with simulated prior on the grid
post_sim <- apply(sim, MARGIN = 1, function(row){
    post_log_dens(a = row[1], b = row[2], x = x, u = u, y = y)
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
# alpha_jitter <- runif(1000, min = -0.1, max = 0.1)
# beta_jitter <- runif(1000, min = -0.1, max = 0.1)

# alpha_s <- alpha_s + alpha_jitter
# beta_s <- beta_s + beta_jitter

plot(alpha_s, beta_s)
