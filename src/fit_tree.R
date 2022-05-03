library(dplyr)
library(DEoptim)
library(nloptr)
source("tree_io.R")
source("tree_models.R")

itermax <- 200 # needs > 100 for convergence

# load data
df_steps <- read.csv("../data_anonym/steps_prolific2.csv")

# table of trees
trees <- expand.grid(H = 2:4, B = 2:4, A = 0:1)

# which tree to pick (parallelized in slurm script)
tree_id <- commandArgs(trailingOnly = TRUE)
tree <- trees[tree_id,]

# parameters:
# alpha = kernel length scale,
# beta  = ucb weight (directed exploration)
# gamma = discount factor
# tau   = temperature (undirected exploration)
lower <- c(-5,-5,-5,-5)
upper <- c( 4, 4, 4, 4)


init_pred <- function(N){
  pred <- NULL
  pred$n <- rep(0, N)
  pred$mean <- rep(50, N) # mean prediction (initialised by mean reward of tree)
  pred$var  <- rep(30, N) # start with high uncertainty
  pred$ucb  <- pred$mean + sqrt(pred$var) # XXX beta?
  return(pred)
}


fit_tree <- function(vars, group, keys) {
  vars <- exp(vars) # exponentiate!
  alpha <- vars[1]
  beta  <- vars[2]
  gamma <- vars[3]
  tau   <- vars[4]
  
  params <- NULL # needed in the gp agent
  params$beta  <- beta
  params$noise <- 2 # sampling noise sd used in the experiment
  
  nLL <- 0 # negative log likelihood
  
  for(id in unique(group$subjID)) {
    g <- generateTree(keys$H, keys$B, alpha) # only for the structure, rewards don't matter
    params$kernel <- diffusionKernel(g, alpha = alpha) # NB: alpha will be used by GP, but A != alpha
    v_leaf <- leaves(g)
    
    pred <- init_pred(g$N)

    for(w in 1:15){
      walk <- filter(group, subjID == id, walk_no == w)
      path <- unlist(walk$node)
      obs_reward <- unlist(walk$node_reward)
      
      ucb <- pred$mean + beta * pred$var
      s <- path_sums(g, ucb, gamma)
      
      # softmax
      s <- s - max(s) # to prevent overflow
      p <- exp(s/tau)
      p <- p / sum(p)
      
      # floor and ceiling
      p <- pmax(p, .000001)
      p <- pmin(p, .999999)
      
      nLL <- nLL - log(p[which(v_leaf == walk$path_leaf[[1]])])
      
      # update GP prediction for the next walk
      pred <- gp(g, path, obs_reward, pred, params)
    }
  }
  
  return(nLL)
}


# FIT TREE LOOP
df_tree_fit <- df_steps %>%
  filter(subjID != 104) %>% # exclude, because of saving issues
  filter(H == tree$H, B == tree$B, A == tree$A) %>% # take data for only this tree
  group_by(H, B, A) %>% # redundant but convenient for splitting data in group and key columns
  group_modify(function(group, keys){
    # DIFFERENTIAL EVOLUTION
    best_fit <- DEoptim(fit_tree, lower, upper, control = list(itermax=itermax), group, keys)
    tmp <- best_fit$optim$bestmem
    tmp <- exp(tmp) # exponentiate!
    if(keys$H == 2) # for H=2, node and path are the same, gamma is meaningless, therefore set it to 0
      tmp[3] <- 0    
    tibble(alpha=tmp[1], beta=tmp[2], gamma=tmp[3], tau=tmp[4], nLL=best_fit$optim$bestval)
    
    # NELDER-MEAD
    # x0 = (upper + lower) / 2
    # best_fit <- neldermead(x0, fit_tree, lower, upper,
    #                        nl.info = TRUE, control = list(maxeval=100), group, keys)
    # tmp <- best_fit$par
    # print(tmp)
    # tibble(alpha=tmp[1], beta=tmp[2], gamma=tmp[3], tau=tmp[4], nLL=best_fit$value)
  })

write.csv(df_tree_fit, paste0("../fit/DE_tree", tree_id, ".csv"), row.names = FALSE)
#write.csv(df_tree_fit, paste0("../fit/NM_tree", tree_id, ".csv"), row.names = FALSE)
