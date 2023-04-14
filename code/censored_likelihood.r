library(tidyverse)
library(INLA)

logit = function(x) log(x/(1-x))
invlogit = function(x) exp(x)/(1+exp(x))

# Toy example for testing censored likelihood

n = 250

df.ex <- tibble(
  x = rbinom(n,1,.5),
  Truncated = rbinom(n,1,.25),
  N = rpois(n, lambda = 500)
  ) %>% 
  mutate(
    # a = -1; B = 1
    meanVSR = exp(-1 + x),
    Y = rpois(n, N * (meanVSR) ),
    y = round(Y - Y * runif(n,.5, .9) * Truncated )
  )



# # Binomial
# out <- inla(formula = Y ~ 1 + x, family = "binomial", Ntrials = N, data = df.ex)
# out$summary.fixed
# 
# invlogit(out$summary.fixed[1,])

# Poisson
out1 <- inla(formula = Y ~ 1 + x, family = "poisson", E = N, data = df.ex)
out1$summary.fixed


# Poisson com o dado censurado
out2 <- inla(formula = y ~ 1 + x, family = "poisson", E = N, data = df.ex)
out2$summary.fixed


# Poisson censurada

inla.mdata(cbind(df.ex$y, low = ifelse(df.ex$Truncated == 1, df.ex$y, 0), high = df.ex$N))

out3 <- inla(formula = inla.mdata(cbind(y, low, high)) ~ 1 + x, 
             family = "cenpoisson2", 
             E = N, 
             data = df.ex %>% 
               mutate(
                 low = ifelse(Truncated == 1, y, -1),
                 high = Inf
               ),
             )

out3$summary.fixed
out2$summary.fixed
out1$summary.fixed



