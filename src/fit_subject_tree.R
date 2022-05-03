library(dplyr)
library(DEoptim)
library(nloptr)
source("tree_io.R")
source("tree_models.R")

# load data
df_steps <- read.csv("../data_anonym/steps_prolific2.csv")

# which subject to pick (parallelized in slurm script)
subject_id <- commandArgs(trailingOnly = TRUE)

# parameters:
# alpha = kernel length scale,
# beta  = ucb weight (directed exploration)
# gamma = discount factor
# tau   = temperature (undirected exploration)
lower <- c(-5,-5,-5,-5)
upper <- c( 4, 4, 4, 4)

params <- NULL
params$beta  <- 0 # dummy, we compute UCB manually
params$noise <- 2 # sampling noise (equal to the noise_sd used in the experiment)

init_pred <- function(N){
  pred <- NULL
  pred$n <- rep(0, N)
  pred$mean <- rep(50, N) # mean prediction (initialised by mean reward of tree)
  pred$var  <- rep(30, N) # start with high uncertainty
  pred$ucb  <- pred$mean + sqrt(pred$var) # XXX beta?
  return(pred)
}

fit_subject_tree <- function(vars, group, keys){
  vars <- exp(vars)
  alpha <- vars[1]
  beta  <- vars[2]
  gamma <- vars[3]
  tau   <- vars[4]
  
  nLL <- 0
  
  g <- generateTree(keys$H, keys$B, alpha) # only for the structure, rewards don't matter
  v_leaf <- leaves(g)
  
  pred <- init_pred(g$N)

  for(w in 1:15){
    walk <- filter(group, walk_no == w)
    path <- unlist(walk$node)
    obs_reward <- unlist(walk$node_reward)
    
    ucb <- pred$mean + beta * pred$var
    s <- path_sums(g, ucb, gamma)
    s <- s - max(s)
    
    # softmax
    p <- exp(s/tau)
    p <- p / sum(p)
    
    nLL <- nLL - p[which(v_leaf == walk$path_leaf[[1]])]
    
    # update GP prediction for the next walk
    pred <- gp(g, path, obs_reward, pred, params)
  }
  
  return(nLL)
}


df_subject_fit <- df_steps %>%
  #filter(subjID != 104) %>% # TODO why exclude?
  filter(subjID == subject_id) %>% # only fit the selected subject
  group_by(subjID, H, B, A) %>% # group by tree within subject
  group_modify(function(group, keys){
    print(keys) # for logging progress
    
    if(keys$H == 2) # for H=2, gamma should always be 0
      upper[3] = -5
    else
      upper[3] = 4
    
    # DIFFERENTIAL EVOLUTION
    best_fit <- DEoptim(fit_subject_tree, lower, upper, control = list(itermax=200), group, keys)
    tmp <- best_fit$optim$bestmem
    tibble(alpha=tmp[1], beta=tmp[2], gamma=tmp[3], tau=tmp[4], nLL=best_fit$optim$bestval)
    
    # NELDER-MEAD
    # x0 = (upper + lower) / 2
    # best_fit <- neldermead(x0, fit_subject_tree, lower, upper,
    #                        nl.info = TRUE, control = list(maxeval=100), group, keys)
    # tmp <- best_fit$par
    # print(tmp)
    # tibble(alpha=tmp[1], beta=tmp[2], gamma=tmp[3], fit=1-best_fit$value) # tau=tmp[4], 
  })

write.csv(df_subject_fit, paste0("../fit/DE_tree_subject", subject_id, ".csv"), row.names = FALSE)
#write.csv(df_subject_fit, paste0("../fit/NM_tree_subject", subject_id, ".csv"), row.names = FALSE)
