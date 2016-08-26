evolve_net <- function(phy, inheriting, evolving, ..., ancestral.interaction = TRUE){
  extlin <- get_extant_taxa(phy)
  brtimes <- sapply(extlin, "[[", "time")
  edgelist <- tibble(
    from = integer(1000),
    to = integer(1000),
    time.step = double(1000)
  )
  
  if(ancestral.interaction == TRUE){
    edgelist$from[1] = extlin[[1]]$childs[1]
    edgelist$to[1] = extlin[[1]]$childs[2]
    edgelist$from[2] = extlin[[1]]$childs[2]
    edgelist$to[2] = extlin[[1]]$childs[1]
    edgelist$time.step[c(1,2)] = extlin[[1]]$time
    ii <- 2
  } else { # this part need to be updated with the corresponding scenario's functions
    edgelist$mother[1] = extlin[[1]]$mom
    edgelist$child_l[1] = extlin[[1]]$childs[1]
    edgelist$child_r[1] = extlin[[1]]$childs[2]
    edgelist$child_l[1] = extlin[[1]]$childs[2]
    edgelist$child_r[1] = extlin[[1]]$childs[1]
    time.step = extlin[[1]]$time
    ii <- 1
  }
  
  for(i in ii:length(extlin)){
      if(i != length(extlin)){
        tmp  <- inheriting(
          current_interactions = edgelist[which(edgelist$time.step == brtimes[i - 1]), ],
          mom_number = extlin[[i]]$mom,
          child_l_number = extlin[[i]]$childs[1],
          child_r_number = extlin[[i]]$childs[2],
          new_time = extlin[[i]]$time,
          ...
        )
        newint <- evolving(edgelist[which(edgelist$time.step == brtimes[i]), ], extlin[[i]], extlin[[i]]$time, extlin[[i-1]]$time, q01, current_interactions, ...)
        tmp <- rbind(tmp, newint)
        edgelist[seq(sum(edgelist[,1] != 0) + 1, sum(edgelist[,1] != 0) + dim(tmp)[1]), ] <- tmp
    } else {
      #browser()
      tmp <- inheriting(
        current_interactions = edgelist[which(edgelist$time.step == brtimes[i-1]), ],
        mom_number = extlin[[i]]$mom,
        child_l_number = extlin[[i]]$childs[1],
        child_r_number = extlin[[i]]$childs[2],
        new_time = 0,
        ...
      )
      newint <- evolving(edgelist[which(edgelist$time.step == brtimes[i]), ], extlin[[i]], extlin[[i]]$time, extlin[[i-1]]$time, q01, current_interactions, ...)
      tmp <- rbind(tmp, newint)
      edgelist[seq(sum(edgelist[,1] != 0) + 1, sum(edgelist[,1] != 0) + dim(tmp)[1]), ] <- tmp
    }
  }
  return(list(edgelist = edgelist[which(edgelist[,1] != 0), ], extant_lineages = extlin))
}