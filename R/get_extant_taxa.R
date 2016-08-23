get_extant_taxa <-function(phy) {
  
  bt<-sort(branching.times(phy), decr=T)
  nb<-dim(phy$edge)[1]
  
  currentEdges<-numeric()
  rootNumber<-as.numeric(names(bt)[1])
  
  times_lineages <- list()
  
  for(i in 1:length(bt)) {
    thisEdge<-as.numeric(names(bt)[i])
    ancestorRow<-which(phy$edge[,2]==thisEdge)	
    descendantRow<-which(phy$edge[,1]==thisEdge)
    
    if(length(ancestorRow)==0) { #at root of tree
      currentEdges<-c(currentEdges, descendantRow)
    } else {
      toCut<-which(currentEdges==ancestorRow)
      currentEdges<-currentEdges[-toCut]	
      currentEdges<-c(currentEdges, descendantRow)
    }
    times_lineages[[i]] <- list(
      mom = ancestorRow,
      childs = descendantRow,
        time = bt[i],
        current_edges = currentEdges)
  }
  return(times_lineages)
}