get_adjacency_matrix <- function(edgelist, extlin, time.step){
    n.sp <- extlin[[time.step]]$current_edges
    mat <- matrix(0, length(n.sp), length(n.sp))
    rownames(mat) <- n.sp
    colnames(mat) <- n.sp
    for(i in 1:dim(edgelist)[1]){
        mat[match(edgelist[i,1], row.names(mat)), match(edgelist[i,2], colnames(mat))] <- 1
    }
    return(mat)
}
