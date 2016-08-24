int_random_child <- function(current_interactions, mom_number, child_l_number, child_r_number, new_time, budding = TRUE){
    mom_traits <- current_interactions[which(current_interactions == mom_number), ]
    if(budding == TRUE){
        n.int <- sample(x = seq(1, dim(mom_traits)[1]), size = 1)
        child <- sample(x = c("left", "right"), size = 1)
        if(child == "left"){
            child_r_traits <- mom_traits
            child_l_traits <- mom_traits[sample(x = seq(1, dim(mom_traits)[1]), size = n.int), ]
        } else {
            child_l_traits <- mom_traits
            child_r_traits <- mom_traits[sample(x = seq(1, dim(mom_traits)[1]), size = n.int), ]
        }
    } else {
        n.int.l <- sample(x = seq(1, dim(mom_traits)[1]), size = 1)
        n.int.r <- sample(x = seq(1, dim(mom_traits)[1]), size = 1)
        child_l_traits <- mom_traits[sample(x = seq(1, dim(mom_traits)[1]), size = n.int.l), ]
        child_r_traits <- mom_traits[sample(x = seq(1, dim(mom_traits)[1]), size = n.int.r), ]
    }
    children_interactions <- as.data.frame(mapply(c, child_l_traits, child_r_traits, child_l_traits[, c(2,1,3)], child_r_traits[, c(2,1,3)]))
    res <- new_network(mom_traits, mom_number, new_time, children_interactions)
    return(res)
}
