source("tree.R")

# TODO
# 1. are node_stats comparable with path_stats?
# 2. if first correcting for node sd, how does path sd look?
# 3.


sim_node_stats <- function(nsims=80, normBy="none"){
  # computes the mean and standard deviation of node rewards
  # for each class of trees based on nsims samples
  # and exports to a csv so they can be used for normalizing
  
  stats <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(stats) <- c("H", "B", "A", "m", "sd")
  
  for(h in 2:4)
    for(b in 2:4)
      for(a in c(1/2, 1, 2)) {
        lump <- c()
        for(sim in 1:nsims)
          lump <- c(lump, V(generateTree(h, b, a, normBy = normBy))$reward)
        
        row <- data.frame(list(h, b, a, mean(lump), sd(lump)))
        colnames(row) <- c("H", "B", "A", "m", "sd")
        stats <- rbind(stats, row)
      }
  
  filename <- paste0("../old_sim/node_stats_normBy=", normBy, ".csv") # export file
  write.csv(stats, file=filename, na="", row.names = FALSE)
  return(stats)
}



sim_path_stats <- function(nsims=80, normBy="none"){
  # computes the mean and standard deviation of path rewards
  # for each class of trees based on nsims samples
  # and exports to a csv so they can be used for normalizing
  
  stats <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(stats) <- c("H", "B", "A", "m", "sd")
  
  for(h in 2:4)
    for(b in 2:4)
      for(a in c(1/2, 1, 2)) {
        lump <- c()
        for(sim in 1:nsims){
          g <- generateTree(h, b, a, normBy = normBy)
          v_leaf <- leaves(g)
          for(i in 1:length(v_leaf)){
            path <- pathToLeaf(g, v_leaf[i])
            lump <- c(lump, sum(V(g)$reward[path]))
          }
        }
        
        row <- data.frame(list(h, b, a, mean(lump), sd(lump)))
        colnames(row) <- c("H", "B", "A", "m", "sd")
        stats <- rbind(stats, row)
      }
  
  filename <- paste0("../old_sim/path_stats_normBy=", normBy, ".csv") # export file
  write.csv(stats, file=filename, na="", row.names = FALSE)
  return(stats)
}

# normBy = "none"|"node"|"path", what = "node"|"path"|"step", stat = "m"|"sd"
tree_stats_plot <- function(normBy="none", what="path", stat="sd"){
  if(what == "step"){
    filename <- paste0("../old_sim/path_stats_normBy=", normBy, ".csv")
    data <- read.csv(filename)
    y_stat <- data[,names(data) == stat] / (data$H-1) #^(.5)
  } else{
    filename <- paste0("../old_sim/", what, "_stats_normBy=", normBy, ".csv")
    data <- read.csv(filename)
    y_stat <- data[,names(data) == stat]
  }
  
  ggplot(data) + geom_line(aes(x=log2(A),y=y_stat)) +
    facet_grid(H~B, labeller = label_both) +
    labs(title = paste0(stat, " of ", what), y = stat)
}


# TEST ============================================
# PLOTTING random agent sim
walks <- read.csv(file=paste0("../old_sim/random_agent_nsims20.csv"))
walks$reward <- transform_reward(walks$reward)
walks$regret <- transform_reward(walks$regret)
names(walks)[names(walks)=="episode"] <- "walk_no"
names(walks)[names(walks)=="alpha"] <- "A"
df <- group_by(walks, H, B, A, agent, policy, walk_no) %>% # everything but sims and reward
      summarise(reward_m = mean(reward), regret_m = mean(regret), score_m = mean(score))

df$A <- as.factor(df$A)
ggplot(df, aes(x=walk_no, y=reward_m, color=A)) +
  geom_line() +
  facet_grid(H~B, scales="free", labeller = label_both) +
  labs(title = "Random agent")
