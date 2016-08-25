new_network <- function(current_interactions, mom_number, new_time, children_interactions){
    no_mom <- current_interactions[-which(current_interactions$from == mom_number), ]
    no_mom <- no_mom[-which(current_interactions$to == mom_number), ]
    res <- as.data.frame(mapply(c, no_mom, children_interactions))
    res$time <- new_time
    return(res)
}
