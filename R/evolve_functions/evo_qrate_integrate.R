evo_qrate_integrate <- function(edgelist, currlin, currtime, prevtime, q01, current_interactions){
    timeSpan <- abs(currtime - prevtime)
    qMatrix <- rbind(c(-1, 1), c(1, -1)) * q01
    tProb <- MatrixExp.eig(qMatrix*timeSpan)
    newint <- data.frame(from = integer(), to = integer(), time.step = double())
    for(j in 1:length(currlin$current_edges)){
        for(k in 1:length(currlin$current_edges)){
            if(j != k){
                if(sum(is.na(match(c(i,k), edgelist[which(edgelist$time.step == currlin$time), 1:2]))) != 0){
                    r <- runif(1)
                    if(r > tProb[1,2]){
                        newint <- rbind(newint, data.frame(from = currlin$current_edges[j], to = currlin$current_edges[k], time.step = 0))
                    }
                }
            }
        }
    }
    newint$time.step <- currtime
    return(newint)
}
