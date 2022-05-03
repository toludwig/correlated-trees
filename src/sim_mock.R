source("tree_io.R")
source("sim_tree.R")

library(dplyr)


# Part of the parameter recovery.
# Create mock data with known parameters.

# each combination of alpha, beta, gamma, tau is one subject
alpha <- c(0, 1)
gamma <- c(0, 1)
beta  <- c(1,2,5,10,30)
tau   <- c(1,2,5,10,30)
nsubject <- length(alpha)*length(gamma)*length(beta)*length(tau)
H <- 2:4
B <- 2:4
A <- 0:1
ntree <- length(H)*length(B)*length(A)

df_walks <- expand.grid(alpha = alpha, gamma = gamma, beta = beta, tau = tau, H = H, B = B, A = A)
df_walks$subjID <- rep(1:nsubject, times = ntree) # 18 trees for each of 100 subjects (A varies within subject)
df_walks <- df_walks %>% relocate(subjID, .before = alpha)

df_steps_sim <- df_walks %>%
  group_by(subjID, alpha, gamma, beta, tau, H, B, A) %>% # all vars
  group_modify(function(group, keys){
    print(keys) # logging
    
    # get parameters for this subject
    params <- NULL
    params$alpha <- keys$alpha
    params$gamma <- keys$gamma
    params$beta  <- keys$beta
    params$tau   <- keys$tau
    params$noise <- 2
    
    # work on the same cities as the real subject
    version <- sample(0:9, 1) # random tree version
    g <- importTree(keys$H, keys$B, keys$A, version)
    
    df <- sim_tree(g, gp, "ucb", 15, params)
    df$v <- version
    return(df)
  })

df_steps_sim <- df_steps_sim %>% relocate(v, .after = A)

# store sim walks
write.csv(df_steps_sim, file=paste0("../sim/sim_mock_steps.csv"), row.names = FALSE)
