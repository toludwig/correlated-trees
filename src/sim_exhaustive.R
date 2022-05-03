
source("tree.R")
source("tree_io.R")
source("sim_tree.R")

# tree parameters
H <- 2:4
B <- 2:4
A <- c(0, 1)

# models
agent  <- c(bmt, gp) # c(ragent) #
agent_lbl <- c("BMT", "GP") # c("random") # 

#policy <- c("mean", "ucb")

# algo hyperparameters (not included in the dataframe)
params <- NULL
#params$A <- 1   # diffusion kernel width needed for GP (not needed because model uses kernel directly)
params$gamma <- 1   # discounting
params$tau   <- .1  # temperature for softmax choice rule
params$noise <- .05 # sampling noise (low relative to the variance of mean rewards)

alphas <- c(0, .25, .5, .75, 1, 1.25)
betas  <- c(0, .25, .5, .75, 1, 1.25)

nsims <- 10

#######################
# SIMULATION LOOP (takes a lot of time!)
#######################

df <- data.frame()

# TODO:
H=2:4
B=2:4
A=0:1
####

for(h in H){
  for(b in B){
    for(a in A){
      for(v in 0:9){
        print(paste("H", h, "B", b, "A", a, "v", v));
        g <- importTree(h, b, a, v)
        
        for(agentid in 2){ #1:2 for bmt : gp
          for(alpha in alphas){
            params$alpha <- alpha
            for(beta in betas){
              params$beta <- beta
              for(sim in 1:nsims){
                walk <- sim_tree(g, agent[[agentid]], "ucb", 15, params)
                
                rows <- data.frame(
                  H = h, B = b, A = a, v = v, # tree parameters
                  agent   = rep(agent_lbl[agentid], 15),
                  alpha   = rep(alpha, 15),
                  beta    = rep(beta, 15),
                  sim     = rep(sim, 15),
                  walk_no = 1:15,
                  reward  = walk$reward,
                  regret  = walk$regret,
                  step_reward  = walk$reward / (h-1), # normalize by height
                  step_regret  = walk$regret / (h-1),
                  score   = walk$score,
                  zreward = walk$zreward,
                  leaves  = walk$leaves
                )
                df <- rbind(df, rows)
              }
            }
          }
        }
      
        write.csv(df, file=paste0("../sim/exhaustive_nsims=10_GP.csv"), row.names = FALSE)
      }
    }
  }
}
