# Testing different tree models (see tree_models.R).
#
# The experiment works as follows:
# In each block of trials a tree with certain parameters is generated defining the true rewards.
# In each walk rewards are sampled from the true tree.
# The H-1 steps from the root to a leaf node are defined as one walk.
# The agent tries to maximize cumulative reward during its walk on the tree.

# imports
source("tree_models.R")


# initialise prediction object
init_pred <- function(N){
  pred <- NULL
  pred$n <- rep(0, N)
  pred$mean <- rep(50, N) # mean prediction (initialised by mean reward of tree)
  pred$var  <- rep(30, N) # start with high uncertainty
  pred$obs_mean <- rep(0, N) # used by the GP only
  pred$ucb  <- pred$mean + sqrt(pred$var) # XXX
  return(pred)
}


####################
# action selection
####################

# returns child node which maximises the policy
transition <- function(g, v, pred, policy, params){
  vs <- as.integer(children(g, v)) # possible successors (only their IDs)
  if(length(vs) == 0) # if there are no children XXX
    return(NULL)
  vmax <- vs[softmax(pred[[policy]][vs], tau = params$tau)]
  return(vmax)
}

# undirected exploration
# returns mostly argmax for low temperature, 0 < tau << 1
# or a random index for higher temperatures, 1 << tau
softmax <- function(x, tau=1) {
  p <- exp(x/tau) / sum(exp(x/tau))
  rcat(1, p) # sample from categorical distribution
}


###############################################################################
# MAIN SIMULATION (formerly called "rootPlanning")
###############################################################################

sim_tree <- function(g, agent, policy, n_walk, params){
  df_steps_sim <- tibble() # store every step taken in the city
  row <- NULL
  
  # compute kernel only once for all 15 walks
  params$kernel <- diffusionKernel(g, alpha = params$alpha)
  
  pred <- init_pred(g$N)
  
  for(walk_i in 1:n_walk){
    row$walk_no <- walk_i
    
    # in each walk sample observation from the tree
    G <- sampleTree(g, params$noise)
    
    # walk down the tree
    v <- 1 # current vertex (starting at root)
    # at the root no reward is received and there is no update
    
    # root planning: which path yields the highest cumulative utility?
    v_leaf <- as.integer(leaves(G))
    cum_utility <- path_sums(g, pred[[policy]], params$gamma)
    
    # for low temperatures, softmax could overflow therefore shift utilities (softmax is shift invariant)
    cum_utility <- cum_utility - max(cum_utility)
    
    best_leaf  <- v_leaf[softmax(cum_utility, tau = params$tau)]
    best_path  <- pathToLeaf(G, best_leaf)
    obs_reward <- V(G)$reward[best_path] # observe reward at all nodes on the path
    
    row$path_leaf <- best_leaf
    row$reward    <- sum(obs_reward) # cumulative reward
    
    for(step in 1:(g$H-1)){
      row$node        <- best_path[step]
      row$node_reward <- obs_reward[step]
      row$step_no     <- step
      df_steps_sim <- rbind(df_steps_sim, row)
    }
    
    # root planning only updates at the end of the walk (less computation!)
    # note that agents can handle multiple observations at a time :-)
    pred <- agent(G, best_path, obs_reward, pred, params) # prediction update
  }
  
  return(df_steps_sim)
}




################################################################################
# OLD
# ALTERNATIVE PLANNING ALGORITHMS
################################################################################


# utility for clipping the score, TODO move somewhere else
clip01 <- function(score){ if(score > 1) 1 else if(score < 0) 0 else score; }

# Online planning means to choose the next node while walking down, in contrast to
# root planning considers all possible paths (= leaf nodes) at the root
# and picks the path with the highest cumulative reward.

onlinePlanning <- function(g, agent, policy, n_walk, params){
  walk <- NULL
  walk$reward <- rep(0, n_walk) # stores cumulative rewards over all walks
  walk$regret <- rep(0, n_walk) # stores the gap of cumulative rewards to optimal
  walk$leaves <- rep(0, n_walk) # stores for each walk, which path was taken (by leave ID)
  
  pred <- init_pred(g$N)
  
  for(walk_i in 1:n_walk){
    # in each walk sample observation from the tree
    G <- sampleTree(g, sigma=params$noise)
    
    # walk down the tree
    v <- 1 # current vertex (starting at root)
    # at the root no reward is received and there is no update
    v <- transition(G, v, pred, policy, params) # first transition
    
    while(!is.null(v)){ # go down til there are no children anymore
      #print(v) # print the path as sanity check
      #print(pred$mean)
      #print(pred$var)
      obs_reward <- V(G)$reward[v] # reward at new v
      walk$reward[walk_i] <- walk$reward[walk_i] + obs_reward # accumulate reward
      pred <- agent(G, v, obs_reward, pred, params) # prediction update
      walk$leaves[walk_i] <- v # overwrites til at the end of the while v is the leaf node
      v <- transition(G, v, pred, policy, params) # next v
    }
    
    path_stats <- calc_path_stats(g, V(g)$reward)
    walk$regret[walk_i] <- path_stats$best_reward - walk$reward[walk_i]
    walk$score[walk_i]  <- clip01((walk$reward[walk_i] - path_stats$worst_reward) /
                                  (path_stats$best_reward - path_stats$worst_reward))
    walk$zreward[walk_i] <- (walk$reward[walk_i] - path_stats$mean) / path_stats$sd
  }
  
  # print prediction as sanity check
  # plotTree(g, V(g)$reward) # plot true rewards
  # plotTree(g, pred$mean)   # plot prediction
  # plotTree(g, abs(pred$mean-V(g)$reward)) # plot prediction error
  
  #return(pred)
  
  return(walk)
}




mcts_planning <- function(g, agent, policy, n_walk, params){
  # instead of exhaustively searching the tree for the best path,
  # use simulations of the current internal model
  # to do this, we endow the agent with knowledge about the transition structure (GP uses that anyway)
  # TODO only at the root or online?
  
  # the parameter policy is the tree policy, the rollout policy is (uniform?) TODO
  
  walk <- NULL
  walk$reward <- rep(0, n_walk) # stores cumulative rewards over all walks
  walk$regret <- rep(0, n_walk) # stores the gap of cumulative rewards to optimal
  walk$leaves <- rep(0, n_walk) # stores for each walk, which path (= leaf) was taken
  
  pred <- init_pred(g$N)
  
  for(walk_i in 1:n_walk){
    # in each walk sample noise on top of the tree
    G <- sampleTree(g, sigma=params$noise)
    
    # MCTS planning performs 3 steps (selection, simulation, backup)
    # in an outer loop which can be repeated as long as time remains
    
    # outer loop:
    nsim = 10
    sim_leaf = rep(0, nsim)
    sim_reward = rep(0, nsim)
    for(sim in 1:nsim){
      v <- 1 # current vertex (starting at root)
      
      # 1. select a node of the already expanded ones using the tree policy
      repeat{ # do-while
        w <- transition(G, v, pred, policy, params) # transition according to tree policy
        
        if(pred$n[w] > 0 # walk down as long as the nodes are expanded
           && runif(1) < 1 && !is.null(w)) # and stop with a chance of 1/2 or latest at the end of the walk
          v <- w
        else
          break
      }
      #print(v)
      
      # 2. simulation using the rollout policy (same as tree policy? TODO)
      # note: rollout only makes sense with a heuristic (i.e. the agent needs to be able to generalize)
      repeat{ # do-while
        sim_leaf[sim] <- v
        w <- transition(G, v, pred, policy, params) # transition according to rollout policy
        if(!is.null(w)) # go down til there are no children anymore
          v <- w
        else
          break
      }
      
      # 3. backup: calculate the reward of the simulated path (we don't actually back it up!)
      sim_leaf[sim] <- v
      path <- pathToLeaf(G, v)
      sim_reward[sim] <- sum(pred[[policy]][path])
    }
    
    # after nsim simulations, pick the best path
    best_leaf <- sim_leaf[argmaxRandomTie(sim_reward)]
    best_path <- pathToLeaf(G, best_leaf)
    walk$leaves[walk_i] <- best_leaf
    
    obs_reward <- V(G)$reward[best_path] # observe reward at all nodes on the path
    walk$reward[walk_i] <- sum(obs_reward) # cumulative reward
    
    # like in root planning, only update at the end of the walk (less computation!)
    # note that agents can handle multiple observations at a time :-)
    pred <- agent(G, best_path, obs_reward, pred, params) # prediction update
    
    path_stats <- calc_path_stats(g, V(g)$reward)
    walk$regret[walk_i] <- path_stats$best_reward - walk$reward[walk_i]
    walk$score[walk]  <- clip01((walk$reward[walk_i] - path_stats$worst_reward) /
                                  (path_stats$best_reward - path_stats$worst_reward))
    walk$zreward[walk_i] <- (walk$reward[walk_i] - path_stats$mean) / path_stats$sd
  }
  
  return(walk)
}
