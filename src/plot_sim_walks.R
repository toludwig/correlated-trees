library(dplyr)
library(ggplot2)

# alternatively: load simulated steps
df_sim_steps <- read.csv("../sim/sim_subjects_steps.csv")
#df_sim_steps <- read.csv("../sim/sim_mock_steps.csv")
#df_sim_steps <- read.csv("../sim/sim_optimal_steps.csv")

df_walks <- df_sim_steps %>%
  dplyr::select(-node, -node_reward, -step_no) %>%
  distinct(subjID, H, B, A, v, walk_no, .keep_all = TRUE)
  #filter(alpha == A) # important for mock
  
#path_stats <- read.csv(file=paste0("../study/trees/path_stats.csv"), na="")
#df_walks <- left_join(df_walks, path_stats, by = c("H", "B", "A", "v"))

df_walks$A <- as.factor(df_walks$A)

df_performance <- df_walks %>%
  group_by(A, walk_no) %>%
  summarise(norm_step_reward_m = mean(reward / (H-1)) - 50)

#df_performance$H <- as.factor(df_performance$H)
#df_performance$beta <- as.factor(df_performance$beta)

ggplot(df_performance, aes(x = walk_no, y = norm_step_reward_m)) +
  geom_line(aes(linetype = A)) #, color = beta))
