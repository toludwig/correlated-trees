source("tree_io.R")
source("sim_tree.R")

library(dplyr)


# Create mock data for optimal subjects, i.e.
# alpha = A, gamma = 1, no tau/softmax (but here small tau)

beta  <- c(.4, .8, 1.6, 3.2, 6.4, 12.8, 25.6, 51.2)
nsubject <- length(beta)
H <- 2:4
B <- 2:4
A <- 0:1
ntree <- length(H)*length(B)*length(A)

#df_walks <- expand.grid(alpha = alpha, gamma = gamma, beta = beta, tau = tau, H = H, B = B, A = A)
df_walks <- expand.grid(beta = beta, H = H, B = B, A = A)
df_walks$subjID <- rep(1:nsubject, times = ntree) # 18 trees for each of 100 subjects (A varies within subject)
df_walks <- df_walks %>% relocate(subjID, .before = beta)

df_steps_sim <- df_walks %>%
  group_by(subjID, beta, H, B, A) %>% # all vars
  group_modify(function(group, keys){
    print(keys) # logging
    
    # get parameters for this subject
    params <- NULL
    params$alpha <- keys$A
    params$gamma <- 1
    params$beta  <- keys$beta
    params$tau   <- .1
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
write.csv(df_steps_sim, file=paste0("../sim/sim_optimal_steps.csv"), row.names = FALSE)