library(ggplot2)
library(tidyverse)
source("tree.R")

# hyperparameters
#H <- 3 # height
#B <- 4 # branching factor
#alpha     <- 1 # width of the kernel

### find hyperparameters by fitting a GP and computing the error ####
error <- data.frame()
hmax <- 5 # 4 levels
bmax <- 5 # 4 levels
amax <- 3 # 7 levels
sims <- 20

for (h in 2:bmax) {
  for (b in 2:hmax) {
    for (a in -3:amax) {
      alpha <- 2^a # log scale base 2
      # for (sigma in 0:2) # noise is no hyperparameter yet
      ### Prior ####
      for (sim in 1:sims) {
        g <- generateTree(h, b, widthCorr = 1, depthCorr = 1, alpha = alpha)
        N <- length(V(g))
        reward <- V(g)$reward #sampleTree(g, sigma=0) # zero noise
        
        x = NULL
        for (trial in 1:N) { # each trial one more node gets unveiled
          x <- c(x, sample(setdiff(1:N, x), 1)) # "open" one random new node
          y <- reward[x]
          posterior <- gpr(1:N, x, y, g$kernel, mu_0=0, noise=1e-4) # predict all nodes TODO noise=0?
          # posterior has two objects $mu and $var
          mse = mean((posterior$mu - reward)^2) # mean squared error
          
          row <- NULL
          row$h <- h
          row$b <- b
          row$alpha <- alpha
          row$sim <- sim
          #row$sigma <- 0 # constant for now
          row$trial <- trial
          row$mse <- mse
          error <- rbind(error, row)
        }
      }# break;
    cat("h", h, "b", b, "alpha", alpha, "\n")
    save(error, file="../data/hyperparams_gp.RData")
    }# break;
  }# break;
}

# make factors
error$b <- as.factor(error$b)
error$h <- as.factor(error$h)
error$alpha <- as.factor(error$alpha)

# average over sims
error1 <- error %>%
  group_by(b, h, alpha, trial) %>%
  summarise(mse_m = mean(mse), mse_s = sd(mse))

ggplot(error1, aes(x=trial, y=mse_m, colour=alpha)) +
  geom_line() + geom_errorbar(aes(ymin=mse_m-mse_s, ymax=mse_m+mse_s)) +
  facet_grid(rows=vars(h), cols=vars(b), scales="free") +
  labs(title = "Hyperparameters GP")
ggsave("../fig/hyperparams_gp.pdf")

