require(foreign)
require(dplyr)
pew <- read.dta("./pew_research_center_june_elect_wknd_data (1).dta")
vote_share <- read.csv("2008ElectionResult.csv")
names(pew)
table(pew$ideo)
table(pew$state)

# data cranking 
table(pew$state, pew$ideo)
aggs <- pew %.% group_by(state) %.% 
            summarise(n_surv = n(),
                      n_lib = sum(ideo == "very liberal", na.rm = T),
                      raw_rate = n_lib / n_surv) %.% 
            filter(! state %in% c("hawaii", "alaska")) %.% 
            mutate(state = tolower(state))
                                


hist(aggs$n_lib)
mean(aggs$n_lib/aggs$n_surv)
var(aggs$n_lib/aggs$n_surv)
mean(1/aggs$n_surv)
# follow formular 2.18 on pg 51
# mean(aggs$n_lib/aggs$n_surv) = alpha / beta (1)
# var(aggs$n_lib/aggs$n_surv) = alpha / beta * (1/agg$n_surv) + alpha / beta^2 (1)
# subsitute  (1/agg$n_surv) with mean(1/agg$n_surv) as gelman we have
# beta = A/(B-A*C)  A = mean(aggs$n_lib/aggs$n_surv), B = var(aggs$n_lib/aggs$n_surv), C = mean(1/aggs$n_surv)

# plug in formula and get alpha & beta estimate
A <- mean(aggs$n_lib/aggs$n_surv)
B <- var(aggs$n_lib/aggs$n_surv)
C <- mean(1/aggs$n_surv)
beta <- A/(B-A*C)
alpha <- beta * A

# simulate gamma data
x <- seq(from =0, to = 2, by = 0.001)
sim_gamma <- data.frame(x = x, prob = dgamma(x, shape=alpha, rate=beta))

# calculate bayesian posterior mean for liberal rate 
aggs$bayesian_mean <- (alpha + aggs$n_lib )/(beta + aggs$n_surv)

# simple plots
hist(aggs$n_lib/aggs$n_surv, breaks = 20)
lines(x, dgamma(x, shape = alpha, rate = beta))


require(ggplot2)
ggplot(data = aggs, aes(x = raw_rate)) + geom_histogram(binwidth = 0.01) + 
    geom_line(data = sim_gamma, aes(x = x, y= prob, color="red")) + xlim(0,0.25)


# data normalization in state name
vote_share <- vote_share %.% left_join(data.frame(state = state.name, state_abb = state.abb), by = "state") %.% 
                mutate(state = doBy::recodeVar(tolower(state), 
                                     src = c("district of columbia"), 
                                     tgt = "washington dc"),
                       vote_Obama_pct = vote_Obama_pct / 100, 
                       state_abb = doBy::recodeVar(state_abb, 
                                                   src=NA,
                                                   tgt="DC"))

compare_data <- aggs %.% inner_join(vote_share, by = "state")


# viz raw liberal mean vs oboma pct
ggplot(compare_data, aes(x= raw_rate, y=vote_Obama_pct)) + geom_point() +
    geom_text(aes(x= raw_rate, y=vote_Obama_pct, label=state_abb))

# viz bayesian mean vs obama pct
ggplot(compare_data, aes(x= bayesian_mean, y=vote_Obama_pct)) + geom_point() +
    geom_text(aes(x= bayesian_mean, y=vote_Obama_pct, label=state_abb))
