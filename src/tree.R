library(igraph)
library(colorspace)
source("utilities.R")


generateTree <- function(H, B, A, node_mean = 50, node_sd = 25, widthCorr = 1, depthCorr = 1){
  N <- (B^H - 1) / (B-1) # number of nodes
  g <- make_tree(N, B, mode="undirected") # tree without lateral edges
  E(g)$lateral <- FALSE    # indicate that all edges so far are not lateral
  E(g)$weight <- depthCorr # all tree edges get the depthCorr as weight
  
  # add edges to connect siblings (same level within same subtree)
  n <- 1
  for(h in 1:(H-1)){ # through each level (without root)
      for(i in 1:(B^h-1)) # for each node on that level
          if(i%%B != 0)
              g <- add_edges(g, c(n+i,n+i+1), weight=widthCorr, attr=list("lateral"=TRUE)) # add edge
      n <- n + B^h
  }
  
  # compute diffusion kernel
  K = diffusionKernel(g, alpha=A)
  
  # sample a reward distribution from the prior
  V(g)$unscaled_reward <- mvrnorm(1, rep(0, N), K) # sample with zero mean
  
  # scaling
  V(g)$reward <- transform_reward(V(g)$unscaled_reward, m = node_mean, s = node_sd)
  g$node_mean <- node_mean
  g$node_var  <- node_sd^2
  
  g$H <- H
  g$B <- B
  g$A <- A
  g$N <- N
  
  # optimization: store node indices for each path for quicker access
  npaths <- B^(H-1)
  g$paths <- matrix(nrow=npaths, ncol=H-1) # number of paths x path-length
  v_leaf <- as.integer(leaves(g))
  i = 0
  for(v in v_leaf){
    i = i + 1
    g$paths[i,] <- pathToLeaf(g, v)
  }
  
  return(g)
}


plotTree <- function(g, labels=V(g)$reward, #marker=
                        colors=V(g)$reward, mask=NULL, hue="dark red",
                        lateralEdges=TRUE, circularLayout=FALSE,
                        title=NULL, filename=NULL, filetype=NULL){
  #' draws a graph `g` as a tree with `labels` as labels
  #' and `colors` as vertex color
  #' where node indexes in `mask` are grayed out
  #' specify both filename and filetype to save a plot
  #' available filetypes are e.g. png and svg (not 'png', don't put them in quotes!)
  
  cscale <- rev(heat_hcl(100))
  colors <- cscale[rescaleMinMax(colors, newmin=1, newmax=100, oldmin = min(colors), oldmax = max(colors))]
  colors[mask] <- "gray"
  labels[mask] <- ""
  #labels <- rep("", g$N)
  
  G <- g # makes a copy
  if(!lateralEdges){
    G <- delete.edges(g, which(E(g)$lateral == TRUE))
  }
  
  plot(G, main=title, layout=(if(circularLayout) NULL else layout_as_tree(G, root=1)),
       vertex.color=colors, vertex.label=labels,
       vertex.size = 20,
       #E(g)$lateral==lateralEdges, # TODO special edge color for laterals?
       vertex.label.color="black", edge.color="black", circular=circularLayout)
  
  if(!is.null(filename) && !is.null(filetype)){
    #png(filename, width=450, height=400, units="px")
    #savePlot(filename = filename, type = "png")
    dev.copy(filetype, filename)
    dev.off()
  }
}


sampleTree <- function(g, noise_sd){
  # returns a noisy sample of the underlying g
  # rewards have iid additive noise
  G <- g # copy tree
  noise <- round(rnorm(g$N, sd=noise_sd))
  V(G)$reward <- V(g)$reward + noise
  return(G)
}


transform_reward <- function(r, m = 50, s=25){
  # inverse z-transform
  r <- r * s + m
  
  # round and clip values
  # NB: If all nodes would be i.i.d. (A=0), 10% of them would be clipped
  # qnorm(.95, m=50, sd=25) = 91.12
  # qnorm(.05, m=50, sd=25) = 8.88
  r <- round(r)
  r[r < 8] <- 8
  r[r > 92] <- 92
  return(r)
}

inverse_transform_reward <- function(sr, m=50, s=25){
  r <- (sr - m) / s
  return(r)
}


####################
# HELPER FUNCTIONS
####################

children <- function(g, v){
  # returns the children vertexes of vertex v
  # children are those neighbors with higher ID than v (except the lateral neighbors)
  G <- delete.edges(g, which(E(g)$lateral == TRUE))
  n <- neighbors(G, v)
  return(n[n > v])
}

parent <- function(g, v){
  # returns the parent of the vertex v (in a tree there is only one for each v)
  n <- neighbors(g, v)
  return(n[n < v][1])
}

leaves <- function(g, v){
  # returns a list with the leaves of the (full) tree
  # there are always B^H leaves
  return(V(g)[(g$N - g$B^(g$H-1) + 1):g$N])
}

pathToLeaf <- function(g, v){
  # returns the list of vertexes on the path from the root to v (only one path possible)
  # not including the root itself (root = 1)
  path <- c(v)
  while(v != 1){
    p <- parent(g, v)
    path <- rbind(path, p)
    v <- p
  }
  path <- setdiff(path, 1)
  path <- rev(path) # bring into the right order, from root to leaf
  return(path)
}

path_sums <- function(g, values, discounting = 1){
  # g is a tree
  # values is the array to sum over with g$N elements, e.g. V(g)$reward
  # discounting = 1 for normal sums, 0 < discounting < 1 for less weight on future values
  v_leaf <- as.integer(leaves(g))
  sum_values <- rep(0, length(v_leaf))
  disc <- NULL
  for(i in 0:(g$H-2))
    disc <- c(disc, discounting^i)
  for(i in 1:length(v_leaf)){
    path <- g$paths[i,]
    disc_values <- unlist(values[path]) * disc
    sum_values[i] <- sum(na.omit(disc_values))
  }
  return(sum_values)
}


calc_path_stats <- function(g, values){
  # returns a dataframe with a bunch of statistics about the paths in g:
  # the max (best) and min (worst) path reward and corresponding leaves (argmax, argmin)
  # the mean and sd over path rewards
  v_leaf <- as.integer(leaves(g))
  cum_reward <- path_sums(g, values)  
  
  stat <- NULL
  
  stat$mean   <- mean(cum_reward)
  stat$median <- median(cum_reward)
  stat$sd     <- sd(cum_reward)
  
  idx <- argmaxRandomTie(cum_reward)
  stat$best_leaf   <- v_leaf[idx]
  stat$best_reward <- cum_reward[idx]
  
  idx <- argmaxRandomTie(-cum_reward)
  stat$worst_leaf   <- v_leaf[idx]
  stat$worst_reward <- cum_reward[idx]
  
  return(stat)
}
