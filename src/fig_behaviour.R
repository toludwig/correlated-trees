
library(tidyverse)
# graphics
library(patchwork)
library(latex2exp)
# testing
library(lmerTest)
library(sjPlot)
library(lme4)
#library(brms)
#library(texreg)
#library(performance)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
options(warn = -1)


colors <- c("#ff5555", "#00aaff") # red, blue


fig_behaviour <- function(walks_file, steps_file, row){
  # row1 has real data, row2 has simulated data
  sim_str <- switch(row, "1" = " (participants)", "2" = " (simulated)")
  
  df_walks <- read.csv(walks_file)
  
  df_walks$A <- as.factor(df_walks$A)
  df_walks$B_scaled <- scale(df_walks$B)
  df_walks$H_scaled <- scale(df_walks$H)
  df_walks$walk_no_scaled <- scale(df_walks$walk_no)
  
  # filter out the low-performing subjects based on scores
  relu <- function(x){ if(x < 0) 0 else x }
  total_scores <- group_by(df_walks, subjID, H, B, A) %>%
    summarize(city_score = relu((mean(reward) - mean[1]) / (best_reward[1] - mean[1]))) %>%
    group_by(subjID, A) %>%
    summarize(total_score = mean(city_score))
  
  ids_lowperf <- as.vector(total_scores$subjID[total_scores$total_score < .2]) # TODO cutoff
  
  # also filter subjID = 104 due to saving error
  df_walks <- df_walks %>% filter(!(subjID %in% ids_lowperf) & (subjID != 104))
  #df_walks %>% group_by(A) %>% summarise(n = n_distinct(subjID)) # N_A0 = 48, N_A1 = 50
  
  
  # PERFORMANCE
  df_walks$step_reward <- df_walks$reward / (df_walks$H - 1)
  # normalize step_reward by step-mean
  df_walks$norm_step_reward <- df_walks$step_reward - df_walks$mean / (df_walks$H-1)
  
  perf <- "norm_step_reward"
  lmer_performance <- lmer(df_walks[,perf] ~ walk_no_scaled + A*B_scaled + A*H_scaled + (1|subjID), df_walks)
  
  df_performance <- group_by(df_walks, A, walk_no) %>%
    summarise(norm_step_reward_m = mean(norm_step_reward), norm_step_reward_sem = sd(norm_step_reward)/sqrt(n()))
  
  p1 <- ggplot(df_performance, aes(x=walk_no, y=norm_step_reward_m, color=A)) +
    geom_line() +
    #geom_ribbon(aes(ymin=score_m-score_sem, ymax=score_m+score_sem, fill=A), alpha=.2, colour=NA) +
    geom_ribbon(aes(ymin=norm_step_reward_m-norm_step_reward_sem, ymax=norm_step_reward_m+norm_step_reward_sem, fill=A),
                alpha=.2, colour=NA) +
    geom_hline(aes(yintercept=0), linetype = "dashed") +
    labs(y = "normalized reward", x = "# trial", title = "Reward", fill = "c", color = "c") +
         #fill = element_blank(), color = element_blank()) +
    #scale_fill_manual(values = c("red", "blue"), breaks = c("c=0", "c=1")) +
    #scale_color_manual(values = c("red", "blue"), breaks = c("c=0", "c=1")) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = c(5,10,15), labels = c(5,10,15)) +
    theme_classic() +
    ylim(-1,8) +
    theme(legend.position = switch(row, "1" = c(.8,.32), "2" = "none"),
          legend.margin = unit(0,"cm"))
  
  p2 <- plot_model(lmer_performance, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE,
                   vline.color = 'grey', colors = "black", value.offset = .4, value.size = 3) +
    theme_classic() +
    scale_x_discrete(labels= rev(c('# trial', 'c', 'b', 'd', 'c * b', 'c * d'))) +
    ylim(-1.5,3) +
    labs(title=sim_str, y=TeX("$\\hat{\\beta}$"))
  
  # p_reward <- (p1 | p2) + plot_annotation(title = paste0("Reward", sim_str)) #, tag = switch(row, "1" = "A", "2" = "D"))
  
  
  # EXPLORATION
  lmer_exploration <- lmer(path_dist ~ walk_no_scaled + A*B_scaled + A*H_scaled + (1|subjID), df_walks)
  
  df_pathdist <- group_by(df_walks, A, walk_no) %>%
    summarise(path_dist_m = mean(na.omit(path_dist)),
              path_dist_s = sd(na.omit(path_dist) / sqrt(n())))
  
  df_pathdist$A <- as.factor(df_pathdist$A)
  p3 <- ggplot(df_pathdist, aes(x=walk_no, y=path_dist_m, color=A)) +
    geom_line() +
    geom_ribbon(aes(ymin=path_dist_m-path_dist_s, ymax=path_dist_m+path_dist_s, fill=A), color=NA, alpha=.2) +
    labs(x = "# trial", y = "path distance", title = "Exploration") +
    scale_x_continuous(breaks = c(5,10,15), labels = c(5,10,15)) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    theme_classic() +
    ylim(0,7) +
    theme(legend.position = "none")
  
  p4 <- plot_model(lmer_exploration, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE,
                   vline.color = 'grey', colors = "black", value.offset = .4, value.size = 3) +
    theme_classic() +
    scale_x_discrete(labels= rev(c('# trial', 'c', 'b', 'd', 'c * b', 'c * d'))) +
    ylim(-3,4.5) +
    labs(title=sim_str, y=TeX("$\\hat{\\beta}$"))
  
  # p_exploration <- (p3 | p4) + plot_annotation(title = paste0("Exploration", sim_str)) #, tag = switch(row, "1" = "B", "2" = "E"))
  
  
  # PLANNING
  df_steps <- read.csv(steps_file)
  
  df_steps <- filter(df_steps, !(subjID %in% ids_lowperf) & (subjID != 104))
  df_steps <- filter(df_steps, H > 2) # don't take 1-step decisions
  df_steps$A <- as.factor(df_steps$A)
  df_steps$B_scaled <- as.numeric(scale(df_steps$B))
  df_steps$H_scaled <- as.numeric(scale(df_steps$H))
  df_steps$walk_no_scaled <- scale(df_steps$walk_no)
  df_steps$step_no_scaled <- scale(df_steps$step_no)
  
  # random baseline
  df_layer_stats <- read.csv("../study/trees/layer_stats.csv")
  df_layer_stats$layer <- df_layer_stats$layer - 1 # recode layer = step + 1
  df_layer_stats <- df_layer_stats %>% rename(layer_m = reward_m, layer_s = reward_s, step_no = layer)
  df_layer_stats$A <- as_factor(df_layer_stats$A)
  
  df_layer_stats$layer_s[which(df_layer_stats$layer_s == 0)] <- 1 # to prevent div_by_zero in zscoring (zscore = 0)
  #print(df_layer_stats %>% filter(H > 2) %>% group_by(A, step_no) %>% summarise(layer_sm = mean(layer_s)))
  
  df_steps <- left_join(df_steps, df_layer_stats, by = c("H", "B", "A", "v", "step_no"))
  
  # correct with baseline
  df_steps$norm_node_reward <- (df_steps$node_reward - df_steps$layer_m) / df_steps$layer_s
  
  lmer_planning <- lmer(norm_node_reward ~ walk_no_scaled + A + B_scaled + H_scaled + step_no_scaled + step_no_scaled*A + (1|subjID), df_steps)
  
  df_steps_m <- df_steps %>%
    group_by(A, step_no) %>% # H
    summarise(norm_node_reward_m = mean(norm_node_reward),
              norm_node_reward_s = sd(norm_node_reward) / sqrt(n()))
  
  df_steps_m$A <- as.factor(df_steps_m$A)
  
  p5 <- ggplot(df_steps_m, aes(x = step_no, y = norm_node_reward_m)) +
    geom_line(aes(color = A)) + #, linetype = )) +
    geom_ribbon(aes(ymin = norm_node_reward_m-norm_node_reward_s, ymax = norm_node_reward_m+norm_node_reward_s, fill=A),
                color=NA, alpha=.2) +
    geom_hline(aes(yintercept=0), linetype = "dashed") +
    scale_x_discrete(labels = as.character(1:3), limits = as.character(1:3)) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(x = "# step", y = "normalized reward", title = "Planning") +
    theme_classic() +
    ylim(-.1,1) +
    theme(legend.position = "none")

  p6 <- plot_model(lmer_planning, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE,
                   vline.color = 'grey', colors = "black", value.offset = .4, value.size = 3) +
    theme_classic() +
    scale_x_discrete(labels= rev(c('# trial', 'c', 'b', 'd', '# step', '# step\n * c'))) +
    ylim(-.2, .65) +
    scale_y_continuous(limits = c(-.2, .65), breaks = c(0, .25, .5), labels = c("0", "0.25", "0.5")) +
    labs(title=sim_str, y=TeX("$\\hat{\\beta}$"))
  
  #p_planning <- (p5 | p6) + plot_annotation(title = paste0("Planning", sim_str)) #, tag = switch(row, "1" = "C", "2" = "F"))
  
  # join all panels in row
  if(row == 1){
    return((p1 + labs(tag = "A")) | p2 | (p3 + labs(tag = "B")) | p4 | (p5 + labs(tag = "C")) | p6)
  } else
    return((p1 + labs(tag = "D")) | p2 | (p3 + labs(tag = "E")) | p4 | (p5 + labs(tag = "F")) | p6)
  
  #return((p_reward + p_exploration + p_planning))
}


# two-row plot:
# real data
# simulated data
p_row1 <- suppressMessages(fig_behaviour("../data/prolific2/walks_prolific2.csv", "../data/prolific2/steps_prolific2.csv", 1))
p_row2 <- suppressMessages(fig_behaviour("../sim/sim_subjects_walks.csv", "../sim/sim_subjects_steps.csv", 2))
p_behaviour <- p_row1 / p_row2
#p_behaviour <- p_behaviour + plot_annotation(tag_levels = "A")

ggsave("../fig/behaviour.pdf", p_behaviour, width = 9, height = 5.1)
#ggsave("../fig/behaviour.png", p_behaviour, width = 9, height = 6)
