source("tree.R")

exportTree <- function(g, filename=""){
  # returns a table representation, and if filename is not NULL, saves it as CSV
  # the table has three columns: name, reward, parent
  
  # works recursively, starting at the root node (if v=1), expanding children
  make_table <- function(g, v=1, parent=NA){
    table <- data.frame(list(v, V(g)$reward[v], parent))
    colnames(table) <- c("name", "reward", "parent")
    ws <- as.integer(children(g, v))
    for(w in ws){
      row <- make_table(g, v=w, parent=v)
      table <- rbind(table, row)
    }
    return(table)
  }
  
  table <- make_table(g)
  
  if(!is.null(filename))
    write.csv(table, file=filename, na="", row.names = FALSE)
  
  return(table)
}


# for the special purpose of creating multiple versions (samples) from each tree and store them
# two kinds of trees: structured (A=1) and unstructured (shuffled version matching the path stats of the structured)
exportManyTrees01 <- function(){
  folder <- "../study/trees/"
  a = 1 
  
  # for storing the path statistics:
  path_stats <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(path_stats) <- c("H", "B", "A", "v", "mean", "sd", "best_reward", "best_leaf", "worst_reward", "worst_leaf")
  
  for(h in 2:4)
    for(b in 2:4)
      for(v in 0:9){
        repeat{
          g <- generateTree(h, b, a)
          
          # calc best and worst cumulative reward and path for each tree
          row <- calc_path_stats(g, V(g)$reward)
          
          # only take cities in which the path difference is not too small
          if(row$best_reward - row$worst_reward > 2) # adhoc limit
            break;
        }
        
        filename <- paste0(folder, "H", h, "B", b, "A", a, "v", v, ".csv")
        exportTree(g, filename)
        
        row$H <- h
        row$B <- b
        row$A <- a
        row$v <- v
        path_stats <- rbind(path_stats, row)
        
        # create an unstructured (A=0) tree with the same path stats
        g1 <- generateTreeWithSamePathSums(g)
    
        filename <- paste0(folder, "H", h, "B", b, "A", 0, "v", v, ".csv")
        exportTree(g1, filename)
        
        row$A <- 0
        path_stats <- rbind(path_stats, row)
      }
  
  write.csv(path_stats, file=paste(folder, "path_stats.csv", sep=""), na="", row.names = FALSE)
  
}


generateTreeWithSamePathSums <- function(g){
  # create a tree matching g in that it has the same path sums
  sums <- path_sums(g, V(g)$reward)
  # shuffle sums (such that the lateral order is broken)
  sums <- sample(sums, length(sums))
  
  g1 <- g # new tree is copy of old one 
  
  # for the non-leaves (and non-root), fill random values
  for(i in 2:(g$N - g$B^(g$H-1)))
    V(g1)$reward[i] <- rnorm(1, mean=50, sd=25)
  
  # fill up the leaves with the difference to the original path-sum
  v_leaf <- as.integer(leaves(g))
  pre_leaf_sums <- path_sums(g1, V(g)$reward)
  V(g1)$reward[v_leaf] <- sums - pre_leaf_sums
  
  V(g1)$unscaled_reward <- inverse_transform_reward(V(g1)$reward)
  return(g1)
}


# for the special purpose of creating multiple versions (samples) from each tree and store them
# DEPRECATED! use exportManyTrees01
exportManyTrees <- function(){
  folder <- "../study/trees/" # export directory (include trailing slash!)
  
  # for storing the path statistics:
  path_stats <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(path_stats) <- c("H", "B", "A", "v", "mean", "sd", "best_reward", "best_leaf", "worst_reward", "worst_leaf")
  
  for(h in 2:4)
    for(b in 2:4)
      for(a in c(1/2, 1, 2))
        for(v in 0:9){
          filename <- paste0(folder, "H", h, "B", b, "A", a, "v", v, ".csv")
          
          repeat{
            g <- generateTree(h, b, a)
            
            # save best and worst cumulative reward and path for each tree
            row <- calc_path_stats(g, V(g)$reward)
            
            # only take cities in which the path difference is not too small
            if(row$best_reward - row$worst_reward > 2) # adhoc limit
              break;
          }
          exportTree(g, filename)
          
          row$H <- h
          row$B <- b
          row$A <- a
          row$v <- v
          path_stats <- rbind(path_stats, row)
        }
  
  write.csv(path_stats, file=paste(folder, "path_stats.csv", sep=""), na="", row.names = FALSE)
}


# read a tree from csv
importTree <- function(H, B, A, v){
  filename <- paste0("../study/trees/H", H, "B", B, "A", A, "v", v, ".csv")
  data <- read.csv(filename)
  data <- data[order(data$name),] # order by name
  g <- generateTree(H, B, A)
  V(g)$reward <- data$reward
  V(g)$unscaled_reward <- inverse_transform_reward(data$reward)
  return(g)
}

importManyTrees <- function(){
  folder <- "../study/trees/" # export directory (include trailing slash!)
  
  # for storing the path statistics:
  path_stats <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(path_stats) <- c("H", "B", "A", "v", "mean", "sd", "best_reward", "best_leaf", "worst_reward", "worst_leaf")
  
  for(h in 2:4)
    for(b in 2:4)
      for(a in c(1/2, 1, 2))
        for(v in 0:9){
          #filename <- paste0(folder, "H", h, "B", b, "A", a, "v", v, ".csv")
          g <- importTree(h, b, a, v)
          row <- calc_path_stats(g, V(g)$scaled_reward)
          row$H <- h
          row$B <- b
          row$A <- a
          row$v <- v
          path_stats <- rbind(path_stats, row)
        }
  
  write.csv(path_stats, file=paste(folder, "path_stats.csv", sep=""), na="", row.names = FALSE)
  return(path_stats)
}



###########################################################
# MORE UTILS
###########################################################

all_path_stats <- function(){
  df_paths <- tibble()
  row <- NULL
  for(H in 2:4){
    row$H <- H
    for(B in 2:4){
      row$B <- B
      for(A in 0:1){
        row$A <- A
        for(v in 0:9){
          row$v <- v
          g <- importTree(H, B, A, v)
          row <- cbind(row, calc_path_stats(g, V(g)$reward))
          df_paths <- rbind(df_paths, row)
        }
      }
    }
  }
  
  write.csv(df_paths, "../study/trees/path_stats.csv", row.names = FALSE)
}


all_layer_stats <- function(){
  df_layer <- tibble()
  row <- NULL
  for(H in 2:4){
    row$H <- H
    for(B in 2:4){
      row$B <- B
      for(A in 0:1){
        row$A <- A
        for(v in 0:9){
          row$v <- v
          g <- importTree(H, B, A, v)
          layer_nodes <- 1 # start with root
          for(layer in 2:H){
            row$layer <- layer
            new_nodes <- NULL
            for(node in layer_nodes){
              new_nodes <- rbind(new_nodes, children(g, node))
            }
            layer_nodes <- new_nodes
            
            tmp <- V(g)$reward[layer_nodes]
            row$reward_m <- mean(tmp)
            row$reward_s <- sd(tmp)
            df_layer <- rbind(df_layer, row)
          }
        }
      }
    }
  }
  
  write.csv(df_layer, "../study/trees/layer_stats.csv", row.names = FALSE)
}
