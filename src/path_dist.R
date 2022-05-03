
# NB:
# have to run walks_join.R before this such that walks is loaded
#source("walks_join.R")

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

tmp <- group_by(walks, subjID, city_no) %>%
  summarise(A = A[1], B = B[1], H = H[1])

tmp$path_diff <- group_by(walks, subjID, H, B, A) %>%
  group_map(function(group, keys){
    n_walks <- nrow(group) # = 15
    diff <- rep(0, n_walks-1) # distances between consecutive paths
    for(p in 1:(n_walks-1)){
      # parse paths, length = H-1
      p1 <- sapply(group$path[p], as.numeric) # previous
      p2 <- sapply(group$path[p+1], as.numeric) # current
      diff[p] <- path_dist(p1, p2, keys$B, keys$H)[1]
    }
    return(mean(diff))
  })


tmp$path_diff <- as.numeric(tmp$path_diff)
df_pathdist <- group_by(tmp, A, B, H) %>%
  summarize(path_diff_m = mean(path_diff))


# PLOT
df_pathdist$A <- as.factor(df_pathdist$A)
ggplot(df_pathdist, aes(x=A, y=path_diff_m)) + geom_bar(stat = "identity") +
  facet_grid(H~B, labeller = label_both) +
  labs(title = paste0(dataset_name, " (N=", N, "): Average distance of consecutive paths"), y = "path_dist")


# path dist over walks within city #################

tmp1 <- group_by(walks, subjID, city_no) %>%
  summarise(A = A[1], B = B[1], H = H[1])

tmp1$path_diff <- group_by(walks, subjID, city_no) %>%
  group_map(function(g, ...){
    B <- g$B[1]
    H <- g$H[1]
    n_walks <- nrow(g) # = 15
    diff <- rep(0, n_walks-1) # distances between consecutive paths
    for(p in 1:(n_walks-1)){
      # parse paths, length = H-1
      p1 <- sapply(g$path[p], as.numeric) # previous
      p2 <- sapply(g$path[p+1], as.numeric) # current
      diff[p] <- path_dist(p1, p2, B, H)
    }
    return(diff)
  })

tmp2 <- unnest(tmp1, cols=path_diff)
tmp2$walk_no <- rep(1:14, nrow(tmp2)/14)
tmp2$A <- as.factor(tmp2$A)
ggplot(tmp2, aes(x=walk_no, y=path_diff)) +
  geom_line() +
  facet_grid(H~B, scales="free", labeller = label_both) +
  labs(title = paste0("Mturk pilot2 (N=", N, ")"))



