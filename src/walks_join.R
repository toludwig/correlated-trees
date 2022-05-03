library(jsonlite)
library(dplyr)
library(tidyverse)

source("tree.R")

# find relevant files
dataset_name <- "Prolific2"
folder  <- "../data/prolific2/" #pilot2/"
fs <- list.files(folder, pattern="all_walks.*")
#m <- regexec("all_walks_worker_id=([^&]*)", fs) # prolific1
m <- regexec("all_walks_a=.&workerID=([^.]*)", fs) # prolific2
IDs <- sapply(regmatches(fs, m), function(s){s[2]})
n_cities <- 9

# lump up df_walks from all subjects
df_walks <- NULL
df_subjID <- NULL
for(i in 1:length(IDs)){
  tmp <- fromJSON(paste0(folder, fs[i]), flatten=TRUE)
  #tmp$subjID <- rep(IDs[i], 15*n_cities) # not anonymised!
  tmp$subjID <- rep(i, 15*n_cities) # to anonymise, only put index of subject, not actual ID
  df_subjID <- rbind(df_subjID, tibble(prolific = IDs[i], anonym = i))
  df_walks <- rbind(df_walks, tmp)
}

# write anonymisation keys to file
write.csv(df_subjID, paste0(folder, "anonymisation_keys.csv"), row.names = FALSE)

# HACK for now: take out double clicks (happens rarely at the end of the last walk in a city)
# EDIT: hopefully this is not needed anymore
# df_walks$valid <- lapply(df_walks$path, function(p){anyDuplicated(p)==0})
# print(paste0("all valid? ", all(df_walks$valid == 1)))
# df_walks <- df_walks[df_walks$valid == 1,]

# renaming columns
df_walks <- rename(df_walks, H = city.H, B = city.B, A = city.A, v = city.v, reward = walk_reward)

# join with path_stats
#best_worst <- read.csv(file=paste0("../data/nonMturk/bestworst_paths4.csv"), na="") # old
#best_worst <- read.csv(file=paste0("../data/nonMturk/bestworst_paths2.csv"), na="") # old
#best_worst <- read.csv(file=paste0("../study/trees/bestworst_paths.csv"), na="") # mturk
#path_stats <- read.csv(file=paste0("../study/trees_pilot/path_stats.csv"), na="") # prolific1
path_stats <- read.csv(file=paste0("../study/trees/path_stats.csv"), na="") # prolific2
df_walks <- left_join(df_walks, path_stats, by = c("H", "B", "A", "v"))

# compute total scores
relu <- function(x){ if(x < 0) 0 else x }
total_scores <- group_by(df_walks, subjID, A, city_no) %>%
  summarize(city_score = relu((mean(reward) - mean[1]) / (best_reward[1] - mean[1]))) %>%
  group_by(subjID, A) %>%
  summarize(total_score = mean(city_score))

# EXPORT BONUS (requires subjID to be prolific ID and total_scores to be grouped by A!)
# max_bonus <- 5
# bonus <- total_scores
# bonus$bonus <- round(total_scores$total_score * max_bonus, 2)
# #bonus <- bonus %>% dplyr::select(-'total_score')
# bonus0 <- bonus %>% filter(A == 0) %>% dplyr::select(-one_of('A', 'total_score'))
# bonus1 <- bonus %>% filter(A == 1) %>% dplyr::select(-one_of('A', 'total_score'))
# #write.csv(bonus,  paste0(folder, "bonus.csv"), row.names = FALSE, quote = FALSE)
# write.csv(bonus0, paste0(folder, "bonus0.csv"), row.names = FALSE, quote = FALSE)
# write.csv(bonus1, paste0(folder, "bonus1.csv"), row.names = FALSE, quote = FALSE)
 
# FILTER SUBJECTS based on total score
# ids_lowperf <- as.vector(total_scores$subjID[total_scores$total_score < .2]) # TODO cutoff
# df_walks <- df_walks[!(df_walks$subjID %in% ids_lowperf),]

# how many subjects
N <- length(unique(df_walks$subjID))


# PATH DETAILS ############################################
df_walks$path_leaf <- sapply(df_walks$path, function(p){tail(p,1)})


# STEPS ####################################################
# steps is like walks but with an own row for each step
# will be written to a CSV at the end of the file

df_steps <- df_walks
df_steps$path_timing <- df_steps %>%
  ungroup() %>%
  pull(path_timing) %>%
  map(function(timing){
    diff(timing) # to take only the difference of time points, i.e. RTs
  })
df_steps$step_no <- df_steps %>%
  ungroup() %>%
  pull(path) %>%
  map(function(p){
    1:length(p) # to make indices for each step
  })
df_steps <- df_steps %>%
  unnest(c(step_no, path, path_reward, path_timing)) %>%
  rename(node = path, node_reward = path_reward, node_rt = path_timing) %>%
  relocate(step_no, .before = node)
df_steps$node <- as.numeric(df_steps$node)



# PERFORMANCE MEASURES ####################################

# compute reward/step TODO sqrt only needed for normBy=none
df_walks$step_reward <- df_walks$reward / (df_walks$H-1)
df_walks$perc_reward <- (df_walks$reward - df_walks$mean) / (df_walks$best_reward - df_walks$mean)

# compute regret
df_walks$regret <- df_walks$best_reward - df_walks$reward
df_walks$step_regret <- df_walks$regret / (df_walks$H-1)
df_walks$perc_regret <- 1-df_walks$perc_reward

# compute score (relative to best/worst path)
df_walks$score <- (df_walks$reward - df_walks$worst_reward) / (df_walks$best_reward - df_walks$worst_reward)
clip <- function(score){ if(score > 1) 1.0 else if(score < 0) 0.0 else score; }
df_walks$score <- as.numeric(lapply(df_walks$score, clip)) # cap score in [0,1]

# z-transform
df_walks$zreward <- (df_walks$reward - df_walks$mean) / df_walks$sd
#df_walks$zregret <- (df_walks$regret - df_walks$mean) / df_walks$sd


# PATH DISTANCE ###########################################

path_dist <- function(p1, p2, B, H){
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

df_walks <- group_by(df_walks, subjID, H, B, A) %>%
  group_modify(function(group, keys){
    n_walks <- nrow(group) # = 15
    diff <- rep(NA, n_walks) # distances between consecutive paths
    for(p in 2:n_walks){
      # parse paths, length = H-1
      p1 <- sapply(group$path[p-1], as.numeric) # previous
      p2 <- sapply(group$path[p], as.numeric) # current
      diff[p] <- path_dist(p1, p2, keys$B, keys$H)[1]
    }
    group$path_dist <- diff
    group
  })


#########################################
# NON-GREEDYNESS / PLANNING
#########################################

# reconstruct what they would have seen given their previous walks
# and define greedyness relative to these last-walk rewards.
# Specifically, path-greedyness refers to picking the currently best path,
# whereas node-greedyness is the average of greedy decisions on a path.
# For 1-step decisions both values conincide.
# High path-greedyness and low node-greedyness is a sign of multistep-planning.

df_walks <- group_by(df_walks, subjID, H, B, A) %>%
  group_modify(function(group, keys){
    print(keys) # for logging progress
    path_greedy <- rep(FALSE, 15)
    node_greedy <- matrix(FALSE, nrow=15, ncol=keys$H-1)
    
    g <- generateTree(keys$H, keys$B, keys$A)
    V(g)$reward <- rep(NA, g$N) # reconstruct visible tree, initially everything is covered (NA)
    for(w in 1:15){
      walk <- filter(group, walk_no == w)
      stat <- calc_path_stats(g, V(g)$reward)
      path_greedy[w] <- walk$path_leaf == stat$best_leaf
      
      path <- c(1, as.integer(as.vector(walk$path[[1]]))) # add root
      for(step in 2:keys$H){
        v <- path[step]
        siblings <- as.vector(children(g, path[step-1]))
        node_greedy[w, step-1] <- max(unlist(V(g)$reward[siblings])) == V(g)$reward[v]
      }
      
      path <- as.integer(as.vector(walk$path[[1]])) # without root
      V(g)$reward[path] <- unlist(walk$path_reward)
    }
    
    bind_cols(group, tibble(path_greedy, node_greedy=rowMeans(node_greedy, na.rm=TRUE)))
  })


# SAVE WALKS ###############################################
# NB: cannot write columns including lists, so these are ignored
df_walks <- dplyr::select(df_walks, -one_of("path", "path_reward", "path_timing", "tooltip_nodes"))
write.csv(df_walks, file=paste0(folder, "walks_prolific2.csv"), rowrow.names = FALSE)


# SAVE STEPS ###############################################
df_steps <- dplyr::select(df_steps, -"tooltip_nodes")
write.csv(df_steps, file=paste0(folder, "steps_prolific2.csv"), row.names = FALSE)
