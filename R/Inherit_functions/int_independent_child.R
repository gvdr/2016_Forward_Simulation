int_indep_child <- function(current_interactions, mom_number, child_l_number, child_r_number, new_time){
    mom_traits <- current_interactions[which(current_interactions$from == mom_number), ]
    child <- sample(x = c("left", "right"), size = 1)
    if(child == "left"){
        child_l_traits <- mom_traits
        child_l_traits$from <- child_l_number
        children_interactions <- as.data.frame(mapply(c, child_l_traits, child_l_traits[, c(2,1,3)]))
        res <- new_network(mom_traits, mom_number, new_time, children_interactions)
        return(res)
    } else {
        child_r_traits <- mom_traits
        child_r_traits$from <- child_r_number
        children_interactions <- as.data.frame(mapply(c, child_r_traits, child_r_traits[, c(2,1,3)]))
        res <- new_network(mom_traits, mom_number, new_time, children_interactions)
        return(res)
    }
}
