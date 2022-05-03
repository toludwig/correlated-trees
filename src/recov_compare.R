library(dplyr)
library(ggplot2)

# MOCK DATA
# df_mock_sim   <- read.csv("../sim/sim_mock_steps.csv")
# df_mock_recov <- read.csv("../fit/all_DE_recov_mock_notau_subject.csv") %>%
#   rename(alpha_r = alpha, beta_r = beta, gamma_r = gamma) #, tau_r = tau)
# df_both <- df_mock_sim %>%
#   distinct(subjID, .keep_all = TRUE) %>%
#   dplyr::select(subjID, alpha, gamma, beta, tau) %>%
#   left_join(df_mock_recov, by = "subjID")

# real data
df_data_params  <- read.csv("../fit/all_DE_subject.csv")
df_recov_params <- read.csv("../fit/all_DE_recov_subject.csv") %>%
  rename(alpha_r = alpha, beta_r = beta, gamma_r = gamma, tau_r = tau, nLL_r = nLL)
df_both <- df_data_params %>%
  left_join(df_recov_params, by = c("subjID", "A"))

# exclude low_perf and outliers
df_both <- df_both %>%
  filter(!subjID %in% ids_lowperf) %>%
  filter(alpha < mean(alpha) + 3*sd(alpha),
         beta  < mean(beta)  + 3*sd(beta),
         gamma < mean(gamma) + 3*sd(gamma),
         tau   < mean(tau)   + 3*sd(tau))

p1 <- ggplot(df_both, aes(x = alpha, y = alpha_r)) + geom_point() +
  labs(title = paste0("r =", cor(df_both$alpha, df_both$alpha_r)))
p2 <- ggplot(df_both, aes(x = beta, y = beta_r)) + geom_point() +
  labs(title = paste0("r =", cor(df_both$beta, df_both$beta_r)))
p3 <- ggplot(df_both, aes(x = gamma, y = gamma_r)) + geom_point() +
  labs(title = paste0("r =", cor(df_both$gamma, df_both$gamma_r)))
p4 <- ggplot(df_both, aes(x = tau, y = tau_r)) + geom_point() +
  labs(title = paste0("r =", cor(df_both$tau, df_both$tau_r)))

(p1 / p2 | p3 / p4)

#+
#  geom_abline(slope = )