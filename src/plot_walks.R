library(ggplot2)

if(!exists("walks"))
  walks <- read.csv(paste0("../data_anonym/prolific1.csv"))


# summarize subjects
df <- group_by(walks, H, B, A, walk_no) %>% # everything but reward, v and subject
  summarise(step_reward_m = mean(step_reward), step_regret_m = mean(step_regret),
            regret_m = mean(regret), perc_regret_m = mean(perc_regret),
            score_m = mean(score),
            z_m = mean(zreward), z_sem = sd(zreward)/sqrt(N))
df$A <- as.factor(df$A)
df$B <- as.factor(df$B)
df$H <- as.factor(df$H)


# PLOT by alpha
ggplot(df, aes(x=walk_no, y=perc_regret_m, color=A)) +
  geom_line() +
  #geom_ribbon(alpha=.5, aes(ymin=z_m-z_sem, ymax=z_m+z_sem, color=A)) +
  facet_grid(H~B, scales="free", labeller = label_both) +
  labs(title = paste0(dataset_name, " (N=", N, ")"))

# save
#titl <- paste0("regret, N=", N)
#ggsave(paste0("../fig/", dataset_name, "/", titl, ".png"))


# PLOT by height
ggplot(df, aes(x=walk_no, y=step_regret_m, color=H)) +
  geom_line() +
  #geom_ribbon(alpha=.5, aes(ymin=z_m-z_sem, ymax=z_m+z_sem, color=A)) +
  facet_grid(A~B, scales="free", labeller = label_both) +
  labs(title = paste0(dataset_name, " (N=", N, ")"))


##############################
# PATH TIMING
##############################

walks$path_timing_onset <- sapply(walks$path_timing, function(ts){ts[1]})
timing <- group_by(walks, subjID) %>% summarise(total_minutes = (max(path_timing_onset)-min(path_timing_onset))/60000,
                                                median_path_sec = median(diff(path_timing_onset)) / 1000,
                                                longest_break_sec = max(diff(path_timing_onset)) / 1000)

############################################################
# AMOUNT OF EXPLORATION (ENTROPY and UNIQUE PATHS)
############################################################

df_entropy <- group_by(walks, subjID, H, B, A) %>%
  group_modify(function(g, ...){
    p <- count(g, g$path_leaf)[,2] / 15
    bind_cols(g, tibble(entropy = -sum(p * log2(p))))
  }) %>% summarise(entropy = first(entropy))

df_entropy <- group_by(df_entropy, H, B, A) %>%
  summarise(entropy_m = mean(entropy))


df_entropy$A <- as.factor(df_entropy$A)
ggplot(df_entropy, aes(x=A, y=entropy_m)) + geom_bar(stat = "identity") +
  facet_grid(H~B, labeller = label_both) +
  labs(title = paste0(dataset_name, " (N=", N, "): Entropy of histogram of taken paths"), y = "Entropy")


# number of unique paths
df_unique <- group_by(walks, subjID, H, B, A) %>%
  summarise(unique_paths = length(unique(path_leaf)))
df_unique <- group_by(df_unique, H, B, A) %>%
  summarise(unique_paths_m = mean(unique_paths), unique_paths_s = sd(unique_paths))
df_unique$A <- as.factor(df_unique$A)
ggplot(df_unique, aes(x=A, y=unique_paths_m)) + geom_bar(stat = "identity") +
  facet_grid(H~B, labeller = label_both) +
  labs(title = paste0(dataset_name, " (N=", N, "): Number of unique taken paths"), y = "# unique paths")



#########################################
# GREEDYNESS
#########################################

df_greedyness <- group_by(walks, H, B, A) %>%
  summarise(path_greedy_m = mean(path_greedy))
df_greedyness$A <- as.factor(df_greedyness$A)
ggplot(df_greedyness, aes(x=A, y=path_greedy_m)) + geom_bar(stat = "identity") +
  facet_grid(H~B, labeller = label_both) +
  labs(title = paste0(dataset_name, " (N=", N, "): Percentage of greedy path choice"), y = "path greedyness")


#########################################
# CITY SCORES
#########################################

city_score <- function(avg_regret, best_path, avg_path){
  city_score <- 1 - avg_regret / (best_path - avg_path)
  if(city_score < 0) city_score <- 0;
  return(city_score)
}

tmp <- group_by(walks, subjID, city_no) %>%
  summarise(A = A[1], B = B[1], H = H[1],
            city_score = city_score(mean(regret), best_reward[1], mean[1]))

group_by(tmp, subjID) %>% summarise(mean_city_score = mean(city_score), bonus = 6 * mean(city_score))
