
library(tidyverse)
library(lmerTest)
library(texreg)

library(sjPlot)
library(latex2exp)
library(patchwork)
library(ggbeeswarm)
library(ggsignif)

colors <- c("#ff5555", "#00aaff") # red, blue

# helper
signif_str <- function(p){
  ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", " n.s.")))
}

# function used to plot one panel of parameters
fig_params <- function(params_file, row_label, row){
  df_fits <- read.csv(params_file)
  
  ids_lowperf <- c(36, 46, 59, 80, 86, 96, 100, 101)
  df_fits <- df_fits %>% filter(!subjID %in% ids_lowperf) # & !subjID %in% c(18,82)) # TODO 18,82 
  df_fits$A <- as.factor(df_fits$A)
  
  ## filter outliers
  nrow(df_fits)
  # df_fits <- df_fits %>%
  #   filter(alpha <= mean(alpha) + 3*sd(alpha),
  #          beta  <= mean(beta)  + 3*sd(beta),
  #          gamma <= mean(gamma) + 3*sd(gamma),
  #          tau   <= mean(tau)   + 3*sd(tau))
  # nrow(df_fits)
  
  # summary
  df_fits_m <- df_fits %>%
    group_by(A) %>%
    summarise(alpha_m = mean(alpha), alpha_se = sd(alpha) / sqrt(n()),
              beta_m  = mean(beta),  beta_se  = sd(beta)  / sqrt(n()),
              gamma_m = mean(gamma), gamma_se = sd(gamma) / sqrt(n()),
              tau_m   = mean(tau),   tau_se   = sd(tau)   / sqrt(n()))
  
  
  # sub-panels
  p <- wilcox.test(alpha~A, df_fits)$p.value
  s_str <- signif_str(p)
  p_alpha <- ggplot(df_fits_m, aes(x=A, y=alpha_m)) +
    geom_bar(stat="identity", aes(fill=A)) +
    geom_errorbar(aes(ymin=alpha_m-alpha_se, ymax=alpha_m+alpha_se), width=.1) +
    labs(title = switch(row, "1" = TeX(paste0("$\\alpha$")), "2" = element_blank()), #^{", s_str, "}$")), # \\, p=", formatC(p), "$")), #y = TeX("$\\alpha$"),
         x = element_blank(), y = row_label, tag = switch(row, "1" = "C", "2" = "D")) +
    theme_classic() +
    theme(#axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          legend.position="none",
          plot.margin = margin(t=0,b=0)) +
    scale_fill_manual(values = colors) +
    ylim(0, .85) +
    geom_quasirandom(aes(x = A, y = alpha), data = df_fits, width = .3, size = .1, alpha = .3) +
    geom_signif(y_position = .75, xmin = "0", xmax = "1", tip_length = 0, annotation = s_str)
    # geom_point(aes(x = A, y = alpha), data = df_fits, position = position_jitter(width = .1), size = .1, alpha = .3)
  
  p <- wilcox.test(beta~A, df_fits)$p.value
  s_str <- signif_str(p)
  p_beta <- ggplot(df_fits_m, aes(x=A, y=beta_m)) +
    geom_bar(stat="identity", aes(fill=A)) +
    geom_errorbar(aes(ymin=beta_m-beta_se, ymax=beta_m+beta_se), width=.1) +
    labs(title = switch(row, "1" = TeX(paste0("$\\beta$")), "2" = element_blank()), #^{", s_str, "}$")), #y = TeX("$\\beta$"),
         x = element_blank(), y = element_blank()) +
    theme_classic() +
    theme(legend.position="none",
          plot.margin = margin(t=0,b=0)) +#, axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_fill_manual(values = colors) +
    ylim(0, 1.4) +
    geom_quasirandom(aes(x = A, y = beta), data = df_fits, width = .3, size = .1, alpha = .3) +
    geom_signif(y_position = 1.2, xmin = "0", xmax = "1", tip_length = 0, annotation = s_str)
    #geom_point(aes(x = A, y = beta), data = df_fits, position = position_jitter(width = .1), size = .1, alpha = .3)
  
  p <- wilcox.test(gamma~A, df_fits)$p.value
  p_gamma <- ggplot(df_fits_m, aes(x=A, y=gamma_m, fill=A)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=gamma_m-gamma_se, ymax=gamma_m+gamma_se), width=.1) +
    labs(title = switch(row, "1" = TeX(paste0("$\\gamma$")), "2" = element_blank()), #^{", s_str, "}$")), #y = TeX("$\\gamma"),
         x = element_blank(), y = element_blank()) +
    theme_classic() +
    theme(legend.position="none",
          plot.margin = margin(t=0,b=0)) + #, axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_fill_manual(values = colors) +
    ylim(0, 2.5) +
    geom_quasirandom(aes(x = A, y = gamma), data = df_fits, width = .3, size = .1, alpha = .3) +
    geom_signif(y_position = 2, xmin = "0", xmax = "1", tip_length = 0, annotation = s_str)
    #geom_point(aes(x = A, y = gamma), data = df_fits, position = position_jitter(width = .1), size = .1, alpha = .3)
  
  p <- wilcox.test(tau~A, df_fits)$p.value
  s_str <- signif_str(p)
  p_tau <- ggplot(df_fits_m, aes(x=A, y=tau_m)) +
    geom_bar(stat="identity", aes(fill=A)) +
    geom_errorbar(aes(ymin=tau_m-tau_se, ymax=tau_m+tau_se), width=.1) +
    labs(title = switch(row, "1" = TeX(paste0("$\\tau$")), "2" = element_blank()), #^{", s_str, "}$")), # \\, p=", formatC(p), "$")), #y = TeX("$\\tau$"),
         x = element_blank(), y = element_blank()) +
    theme_classic() +
    theme(legend.position="none",
          plot.margin = margin(t=0,b=0)) +#, axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_fill_manual(values = colors) +
    ylim(0, 30) +
    geom_quasirandom(aes(x = A, y = tau), data = df_fits, width = .3, size = .1, alpha = .3) +
    geom_signif(y_position = 25, xmin = "0", xmax = "1", tip_length = 0, annotation = s_str)
    #geom_point(aes(x = A, y = tau), data = df_fits, position = position_jitter(width = .1), size = .1, alpha = .3)
  
  p_alpha | p_beta | p_gamma | p_tau
}

fig_lesions <- function(r2_file){
  df_r2 <- read.csv(r2_file)
  df_r2$A <- as_factor(df_r2$A)
  df_r2$lesion <- as_factor(df_r2$lesion)
  
  df_r2_summary <- df_r2 %>%
    filter((lesion != "gamma0") && (lesion != "optimal")) %>%
    group_by(A, lesion) %>%
    summarise(r2_m = mean(r2), r2_se = sd(r2)/sqrt(n()))
  
  # one R^2 per subject
  ggplot(df_r2_summary, aes(x = lesion, fill = A)) +
    geom_bar(aes(y = r2_m), stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = r2_m - r2_se, ymax = r2_m + r2_se), position = position_dodge(.9), width = .2, size = .3) +
    scale_x_discrete(limits = c("full", "alpha0", "beta0", "gamma1"),
                     breaks = c("full", "alpha0", "beta0", "gamma1"),
                     labels = c("full", parse(text = TeX("$\\alpha = 0$")),
                                        parse(text = TeX("$\\beta = 0$")),
                                        parse(text = TeX("$\\gamma = 1$")))) +
    labs(title = "Lesioned models", y = TeX("$R^2$"), x = element_blank(), fill = "c", tag = "A") +
    theme_classic() +
    scale_fill_manual(values = colors) +
    theme(plot.margin = margin(t=-2,b=0,l=0,r=15),
          legend.position=c(.9,1.05)) #, legend.margin = unit(0, "cm"))
}

fig_lesions_diff <- function(r2_file){
  df_r2 <- read.csv(r2_file)
  df_r2$A <- as_factor(df_r2$A)
  df_r2$lesion <- as_factor(df_r2$lesion)
  
  df_r2_summary <- df_r2 %>%
    filter((lesion != "gamma0") && (lesion != "optimal")) %>%
    group_by(A, lesion) %>%
    summarise(r2_m = mean(r2), r2_se = sd(r2)/sqrt(n()))
  
  # R2 to baseline
  p1 <- ggplot(df_r2_summary, aes(x = lesion, fill = A)) +
    geom_bar(aes(y = r2_m), stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = r2_m - r2_se, ymax = r2_m + r2_se), position = position_dodge(.9), width = .2, size = .3) +
    scale_x_discrete(limits = "full") +
    labs(title = "Model", y = TeX("$R^2$"), x = element_blank(), fill = "c", tag = "A") +
    theme_classic() +
    scale_fill_manual(values = colors) +
    theme(plot.margin = margin(t=-2,b=0,l=0,r=5), legend.position = "none")
  
  
  # R2 difference to full
  df_r2_diff <- df_r2 %>% pivot_wider(names_from = lesion, values_from = r2) %>%
    mutate(alpha0 = alpha0 - full, beta0 = beta0 - full, gamma1 = gamma1 - full) %>%
    select(subjID, A, alpha0, beta0, gamma1) %>%
    pivot_longer(c("alpha0", "beta0", "gamma1"), names_to = "lesion", values_to = "r2")
    
  df_r2_diff_summary <- df_r2_diff %>%
    group_by(A, lesion) %>%
    summarise(r2_m = mean(r2), r2_se = sd(r2)/sqrt(n()))
    
  p2 <- ggplot(df_r2_diff_summary, aes(x = lesion, fill = A)) +
    geom_bar(aes(y = r2_m), stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = r2_m - r2_se, ymax = r2_m + r2_se), position = position_dodge(.9), width = .2, size = .3) +
    scale_x_discrete(limits = c("alpha0", "beta0", "gamma1"),
                     breaks = c("alpha0", "beta0", "gamma1"),
                     labels = c(parse(text = TeX("$\\alpha = 0$")),
                                parse(text = TeX("$\\beta = 0$")),
                                parse(text = TeX("$\\gamma = 1$")))) +
    labs(title = "Lesions", y = TeX("$\\Delta R^2$ (lesion - full)"), x = element_blank(), fill = "c", tag = "B") +
    theme_classic() +
    scale_fill_manual(values = colors) +
    theme(plot.margin = margin(t=-2,b=0,l=0,r=15),
          legend.position=c(.9,.3), legend.margin = unit(0, "cm"))
  
  (p1 | p2)#  + plot_layout(widths = c(1,3), heights = c(1,1), ncol = 2, nrow = 1)
}

p_lesions <- fig_lesions_diff("../fit/all_r2.csv") # run model_comparison.R to generate this
p_lesions

p_B  <- fig_params("../fit/all_DE_subject.csv", "participants", 1)
p_C  <- fig_params("../fit/all_DE_recov_subject.csv", "recovered", 2)
p_BC <- p_B / p_C

p_model <- (p_lesions | p_BC) + plot_layout(widths = c(1, 3, 8), heights = c(1,1))

#ggsave("../fig/model.svg", p_model, width = 9, height = 3)
ggsave("../fig/model.pdf", p_model, width = 9, height = 3)
#ggsave("../fig/model.png", p_model, width = 9, height = 3)
