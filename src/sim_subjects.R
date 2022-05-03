source("tree_io.R")
source("sim_tree.R")

library(dplyr)


# Part of the parameter recovery.
# Given fits of subjects, simulate their actions.
# Play the same game again with the same cities.

df_fits  <- read.csv("../fit/all_DE_subject.csv")
df_walks <- read.csv("../data_anonym/walks_prolific2.csv")
  
#ids_lowperf <- c(36, 46, 59, 80, 86, 96, 100, 101)

df_sim_steps <- df_walks %>%
  filter(!subjID %in% c(104)
     # & !subjID %in% ids_lowperf
	 ) %>%
  group_by(subjID, H, B, A, v) %>% # all things that stay the same within a city
  group_modify(function(group, keys){
    print(keys) # logging
    
    # get best parameter fit for this subject
    tmp <- df_fits %>% filter(subjID == keys$subjID)
    params <- NULL # need a S3 object, can't just take tmp tibble
    params$alpha <- tmp$alpha
    params$beta  <- tmp$beta
    params$gamma <- tmp$gamma
    params$tau   <- tmp$tau
    params$noise <- 2
    
    # work on the same cities as the real subject
    g <- importTree(keys$H, keys$B, keys$A, keys$v)
    
    df <- sim_tree(g, gp, "ucb", 15, params)
    return(df)
  })

# store sim steps
write.csv(df_sim_steps, file=paste0("../sim/sim_subjects_steps.csv"), row.names = FALSE)

##############################
# make and store sim walks
##############################

# calculate path distance
path_dist <- function(p1, p2, B, H){ # TODO move this to a separate file
  # calculates the difference between two paths p1 and p2
  # taking into account how many nodes they share on which level
  # and, where they fork, how much lateral distance there is between siblings
  p1 <- c(1, p1) # add root
  p2 <- c(1, p2) # add root
  same <- c(p1 == p2)
  dist <- 0
  for(h in 2:H){
    # only if the parents (h-1) of the current node are the same, i.e. if they are siblings,
    # take the difference on that level and scale with factor that depends on level and tree height
    dist <- dist + same[h-1] * B^(H-h) * abs(p1[h] - p2[h])
  }
  return(dist)
}

df_sim_walks <- group_by(df_sim_steps, subjID, H, B, A, v) %>%
  group_modify(function(group, keys){
    n_walks <- 15
    diff <- rep(NA, n_walks) # distances between consecutive paths
    for(w in 2:n_walks){
      p1 <- group %>% filter(walk_no == w-1) %>% dplyr::select(node) %>% unlist() # previous
      p2 <- group %>% filter(walk_no == w)   %>% dplyr::select(node) %>% unlist() # current
      diff[w] <- path_dist(p1, p2, keys$B, keys$H)[1]
    }
    group <- group %>% distinct(walk_no, .keep_all = TRUE)
    group$path_dist <- diff
    group
  })

df_sim_walks <- df_sim_walks %>%
  dplyr::select(-step_no, -node, -node_reward)
  
path_stats <- read.csv(file=paste0("../study/trees/path_stats.csv"), na="")
df_sim_walks <- left_join(df_sim_walks, path_stats, by = c("H", "B", "A", "v"))

# store sim walks
write.csv(df_sim_walks, "../sim/sim_subjects_walks.csv", row.names = FALSE)
