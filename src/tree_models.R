source("tree.R")
source("utilities.R")
library("extraDistr")

# agents:
# 0. random agent
# 1. bayesian mean tracker (BMT)
# 2. gaussian process (GP)

# All of them follow the following functional interface:
# input: the new and old state, the reward, the previous prediction and parameters
# output: an updated prediction of the statistics of each vertex in the graph


# random agent
ragent <- function(g, v, obs_reward, old_pred, params){
  pred <- old_pred
  pred$mean[v] <- rnorm(1, mean=0, sd=3)
  pred$var[v]  <- rnorm(1, mean=1, sd=1)
  return(pred)
}

# Bayesian mean tracker
bmt <- function(g, v, obs_reward, old_pred, params){
  pred_error <- obs_reward - old_pred$mean[v]
  pred <- old_pred # leave predictions for all other nodes the same
  kalman_gain <- old_pred$var[v] / (old_pred$var[v] + params$noise^2)
  
  
  pred$mean[v] <- old_pred$mean[v] + kalman_gain * pred_error
  pred$var[v]  <- (1 - kalman_gain) * old_pred$var[v]
  pred$ucb[v]  <- pred$mean[v] + params$beta * sqrt(pred$var[v])
  pred$n[v]    <- pred$n[v] + 1
  return(pred)
}

# GP agent
gp <- function(g, v, obs_reward, old_pred, params){
  # the GP does not learn from prediction errors, only from generalization
  # still the old_pred is used to keep track of which nodes were observed and their mean
  N <- g$N
  pred <- old_pred
  pred$n[v] <- pred$n[v] + 1
  
  # store new observation
  # GP implicitly averages multiple y for same x
  pred$x <- c(pred$x, v)
  pred$y <- c(pred$y, obs_reward)
  
  # GP exploits knowing the whole transition structure, however, it may not know the true generating A
  # therefore, we use params$kernel = exp(-params$alpha * L)
  posterior <- gpr(1:N, pred$x, pred$y, params$kernel, mu_0=g$node_mean, var_0=g$node_var, noise=params$noise)
  pred$mean <- posterior$mu
  pred$var  <- posterior$var
  pred$ucb  <- pred$mean + params$beta * sqrt(pred$var)
  return(pred)
}

###############################################################################
# OTHER AGENTS
###############################################################################

# GPTD agent (see Engel, 2003 (ICML): Bayes meets Bellman)
gptd <- function(g, v, obs_reward, old_pred, params){
  # GPTD uses both the structural knowledge from the kernel as well as prediction errors
  pred$n[v] <- pred$n[v] + 1
  G <- V(g)[pred$n > 0]
  
  # TODO implement
}

# alternative: running average mean and variance
alt <- function(g, v, obs_reward, old_pred, params){
  pred_error <- obs_reward - old_pred$mean[v]
  pred <- old_pred # leave predictions for all other nodes the same
  
  pred$n[v] <- pred$n[v] + 1
  pred$mean[v] <- pred$mean[v] + 1/pred$n[v] * (obs_reward - pred$mean[v])
  # Welford's algorithm for running variance:
  #pred$var[v] <- (pred$var[v] * old_pred$n + (obs_reward - pred$mean[v])*(obs_reward - old_pred$mean[v])) / pred$n[v]
  # or
  pred$var[v] <- sqrt(log(sum(pred$n)) / pred$n[v]) # compare Sutton + Barto, p. 35
  pred$ucb[v]  <- pred$mean[v] + params$beta * sqrt(pred$var[v])
  return(pred)
}
