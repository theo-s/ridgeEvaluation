library(visNetwork)
library(forestry)
library(rpart)
library(shiny)


# Simple Iris example with ridgeRF + rpart
y <- iris[1:75,1]
x <- iris[1:75,c(-1, -5)]

rpart_tree <- rpart(y~., 
                    data = x)

forestry_tree <- forestry(x = x,
                          y = y,
                          nodesizeStrictSpl = 10,
                          ntree = 1, 
                          splitratio = .9,
                          ridgeRF = TRUE)


visTree(rpart_tree, main = "Iris classification Tree", width = "100%")

# Get split points data for Ridge tree and translate ---------------------------
forestry_tree <- make_savable(forestry_tree)

feat_names <- colnames(x)
split_feat <- forestry_tree@R_forest[[1]]$var_id
split_val <- forestry_tree@R_forest[[1]]$split_val

split_feat
split_val
num_obs <- ifelse(split_feat < 0, -split_feat, 0)

number_of_nodes <- length(split_val)

get_to_remove <- function(split_feat) {
  zeros <- which(split_feat < 0)
  to_remove <- zeros[2 * (1:(length(zeros) / 2))]
  return(to_remove)
}
get_leaves <- function(split_feat) {
  return(split_feat[-get_to_remove(split_feat)] < 0)
}


node_info <- data.frame(
  node_id = 1:number_of_nodes,
  is_leaf = get_leaves(split_feat)
)
node_info$parent <- 
  c(NA, node_info$node_id[!node_info$is_leaf][floor(1:number_of_nodes / 2)])

node_info$left_child[!node_info$is_leaf] <- 
  2 * (node_info$node_id - cumsum(node_info$is_leaf))[!node_info$is_leaf]
node_info$right_child[!node_info$is_leaf] <- 
  2 * (node_info$node_id - cumsum(node_info$is_leaf))[!node_info$is_leaf] + 1

split_feat_no_double <- split_feat[-get_to_remove(split_feat)]

node_info$split_feat[!node_info$is_leaf] <- 
  split_feat_no_double[node_info$node_id[!node_info$is_leaf]]
node_info$split_val[!node_info$is_leaf] <- 
  split_val[node_info$node_id[!node_info$is_leaf]]


node_info$num_splitting <- 0
node_info$num_averaging <- 0

node_info$num_splitting[node_info$is_leaf] <- 
  -split_feat[get_to_remove(split_feat)]
node_info$num_averaging[node_info$is_leaf] <- 
  -split_feat_no_double[node_info$is_leaf]

for (i in nrow(node_info):1) {
  if (node_info$num_splitting[i] != 0) {
    node_info$num_splitting[node_info$parent[i]] <- 
      node_info$num_splitting[node_info$parent[i]] + node_info$num_splitting[i]
    
    node_info$num_averaging[node_info$parent[i]] <- 
      node_info$num_averaging[node_info$parent[i]] + node_info$num_averaging[i]
  }
}

node_info

# ------------------------------------------------------------------------------
nodes <- data.frame(id = 1:4, level = c(2, 1, 1, 1))
edges <- data.frame(from = c(1, 1, 1),
                    to =   c(2, 3, 4))
### with level
visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as   visLayout(hierarchical = TRUE) 
