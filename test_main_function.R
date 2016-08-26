library(ape)
library(dplyr)

tree <- ape::read.tree("./data/tree_test.txt")

net.test <- netEvolve(tree, 0.1)
net_evolved <- evolve_net(tree, inheriting = int_exact_copy, evolving = evo_qrate_integrate, q01 = 0)


net35 <- as.matrix(net_evolved$edgelist[net_evolved$edgelist$time.step == net_evolved$extant_lineages[[35]]$time, 1:2])

extlin35 <- net_evolved$extant_lineages[[35]]$current_edges


net35 <- as.matrix(net.test$edgelist[net.test$edgelist$time.step == net.test$extant_lineages[[35]]$time, 1:2])

extlin35 <- net.test$extant_lineages[[35]]$current_edges

## prepare function that creates adjacency matrix from the edgelist
