int_exact_copy <- function(current_interactions, mom_number, child_l_number, child_r_number, new_time, ...){
    mom_traits <- current_interactions[which(current_interactions$from == mom_number), ]
    child_l_traits <- mom_traits
    child_l_traits$from <- child_l_number
    child_r_traits <- mom_traits
    child_r_traits$from <- child_r_number
    children_interactions <- as.data.frame(mapply(c, child_l_traits, child_r_traits, child_l_traits[, c(2,1,3)], child_r_traits[, c(2,1,3)]))
    res <- new_network(mom_traits, mom_number, new_time, children_interactions)
    return(res)
}
