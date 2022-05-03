library(dplyr)
library(DEoptim)
#library(nloptr) # only needed for Nelder-Mead

source("tree_io.R")
source("tree_models.R")

# read parameters from slurm cluster
subject_id <- commandArgs(TRUE)[1] # which subject to pick
recov_flag <- commandArgs(TRUE)[2] # whether to use simulated data
lesion_mod <- commandArgs(TRUE)[3] # which lesioned model to run

itermax <- 100 # how many iterations (= generations) of parameters to evolve

tags <- ""

if(recov_flag){
  df_steps <- read.csv("../sim/sim_subjects_steps.csv")
  tags <- "_recov"
  #df_steps <- read.csv("../sim/sim_mock_steps.csv")
  #tags <- "_recov_mock"
} else {
  df_steps <- read.csv("../data_anonym/steps_prolific2.csv")
}


# parameters:
# alpha = kernel length scale,
# beta  = ucb weight (directed exploration)
# gamma = discount factor
# tau   = temperature (undirected exploration)
# (default is exponential search for all parameters, assuming positive alpha)

# lesioned models:
# 0 = full
# 1 = alpha0
# 2 = beta0
# 3 = gamma0
# 4 = gamma1
# 5 = optimal (sets alpha=A and gamma=1)
if(lesion_mod == 0){
  lower <- c(-5,-5,-5,-5)
  upper <- c( 4, 4, 4, 4)
} else if(lesion_mod < 4){
  lower <- c(-5,-5,-5)
  upper <- c( 4, 4, 4)
  tags <- paste0(tags, switch(lesion_mod,
    "1" = "_alpha0",
    "2" = "_beta0",
    "3" = "_gamma0",
    "4" = "_gamma1"))
} else if(lesion_mod == 5){
  lower <- c(-5,-5)
  upper <- c( 4, 4)
  tags <- paste0(tags, "_optimal")
}

# notau (leave out softmax, instead just take p = normalized utilities)
#lower <- c(-5,-5,-5)
#upper <- c( 4, 4, 4)
#tags <- paste0(tags, "_notau")

# linexlinex (linear for alpha and gamma, and exponential for beta and tau)
#lower <- c(-1,-5, 0,-5)
#upper <- c( 2, 4, 2, 4)
#tags <- paste0(tags, "_linexlinex")


init_pred <- function(N){
  pred <- NULL
  pred$n <- rep(0, N)
  pred$mean <- rep(50, N) # mean prediction (initialised by mean reward of tree)
  pred$var  <- rep(30, N) # start with high uncertainty
  pred$ucb  <- pred$mean + sqrt(pred$var) # XXX beta?
  return(pred)
}

fit_subject <- function(vars, group, keys){
  if(lesion_mod == 0){
    alpha <- exp(vars[1])
    beta  <- exp(vars[2])
    gamma <- exp(vars[3])
    tau   <- exp(vars[4])
  } else if(lesion_mod == 1){
    alpha <- 0
    beta  <- exp(vars[1])
    gamma <- exp(vars[2])
    tau   <- exp(vars[3])
  } else if(lesion_mod == 2){
    alpha <- exp(vars[1])
    beta  <- 0
    gamma <- exp(vars[2])
    tau   <- exp(vars[3])
  } else if(lesion_mod == 3){
    alpha <- exp(vars[1])
    beta  <- exp(vars[2])
    gamma <- 0
    tau   <- exp(vars[3])
  } else if(lesion_mod == 4){
    alpha <- exp(vars[1])
    beta  <- exp(vars[2])
    gamma <- 1
    tau   <- exp(vars[3])
  } else if(lesion_mod == 5){
    alpha <- keys$A
    beta  <- exp(vars[1])
    gamma <- 1
    tau   <- exp(vars[2])
  }
  
  params <- NULL # needed in the gp agent
  params$beta  <- beta
  params$noise <- 2 # sampling noise sd used in the experiment

  
  nLL <- 0 # negative log likelihood
  
  for(b in 2:4){
    for(h in 2:4){
      g <- generateTree(h, b, 0) # we know H and B but not A (therefore, put a dummy for A)
      params$kernel <- diffusionKernel(g, alpha = alpha) # NB: alpha will be used by GP, but A != alpha
      v_leaf <- leaves(g)
      
      pred <- init_pred(g$N)
  
      for(w in 1:15){
        walk <- filter(group, B == b, H == h, walk_no == w)
        path <- unlist(walk$node)
        obs_reward <- unlist(walk$node_reward)
        
        ucb <- pred$mean + beta * sqrt(pred$var)
        s <- path_sums(g, ucb, gamma)
        
        # only for notau
        #p <- s / sum(s)
        
        # softmax
        s <- s - max(s) # to prevent overflow
        p <- exp(s/tau)
        p <- p / sum(p)
        
        # floor and ceiling
        p <- pmax(p, .000001)
        p <- pmin(p, .999999)
        
        # TEST 
        #print(-sum(p * log(p))) # entropy
        #print(max(p)) 
        
        # negative log likelihood
        nLL <- nLL - log(p[which(v_leaf == walk$path_leaf[[1]])])
        
        # update GP prediction for the next walk
        pred <- gp(g, path, obs_reward, pred, params)

      	# update BMT prediction for the next walk
        #pred <- bmt(g, path, obs_reward, pred, params)
      }
    }
  }
  
  return(nLL)
}


df_subject_fit <- df_steps %>%
  #filter(subjID != 104) %>% # TODO why exclude?
  filter(subjID == subject_id) %>% # only fit the selected subject
  group_by(subjID, A) %>% # redundant but convenient for separating group and key columns
  group_modify(function(group, keys){
    print(keys) # for logging progress
    
    # DIFFERENTIAL EVOLUTION
    best_fit <- DEoptim(fit_subject, lower, upper, control = list(itermax=itermax), group, keys)
    tmp <- best_fit$optim$bestmem
    switch(lesion_mod,
      "0" = tibble(alpha=exp(tmp[1]),
                   beta =exp(tmp[2]),
                   gamma=exp(tmp[3]),
                   tau  =exp(tmp[4]),
                   nLL  =best_fit$optim$bestval),
      "1" = tibble(alpha=0,
                   beta =exp(tmp[1]),
                   gamma=exp(tmp[2]),
                   tau  =exp(tmp[3]),
                   nLL  =best_fit$optim$bestval),
      "2" = tibble(alpha=exp(tmp[1]),
                   beta =0,
                   gamma=exp(tmp[2]),
                   tau  =exp(tmp[3]),
                   nLL  =best_fit$optim$bestval),
      "3" = tibble(alpha=exp(tmp[1]),
                   beta =exp(tmp[2]),
                   gamma=0,
                   tau  =exp(tmp[3]),
                   nLL  =best_fit$optim$bestval),
      "4" = tibble(alpha=exp(tmp[1]),
                   beta =exp(tmp[2]),
                   gamma=1,
                   tau  =exp(tmp[3]),
                   nLL  =best_fit$optim$bestval),
      "5" = tibble(alpha=keys$A,
                   beta =exp(tmp[1]),
                   gamma=1,
                   tau  =exp(tmp[2]),
                   nLL  =best_fit$optim$bestval))
    
    # NELDER-MEAD
    # x0 = (upper + lower) / 2
    # best_fit <- neldermead(x0, fit_subject, lower, upper,
    #                        nl.info = TRUE, control = list(maxeval=100), group, keys)
    # tmp <- best_fit$par
    # tibble(alpha=tmp[1], beta=tmp[2], gamma=tmp[3], tau=tmp[4], nLL=best_fit$value)
  })

write.csv(df_subject_fit, paste0("../fit/DE", tags, "_subject", subject_id, ".csv"), row.names = FALSE)
#write.csv(df_subject_fit, paste0("../fit/NM", tags, "_subject", subject_id, ".csv"), row.names = FALSE)
