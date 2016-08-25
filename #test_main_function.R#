library(igraph)
library(qgraph)
tree <- read.tree("./data/tree_test.txt")

net.test <- netEvolve(tree, 0.1)

net35 <- as.matrix(net.test$edge_list[net.test$edge_list$time.step == net.test$extant_lineages[[35]]$time, 1:2])

extlin35 <- net.test$extant_lineages[[35]]$current_edges

## prepare function that creates adjacency matrix from the edgelist
