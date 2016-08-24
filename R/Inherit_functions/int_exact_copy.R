int_exact_copy <- function(current_interactions,mom_number){
  mom_traits <- current_interactions[mom_number,]
  child_l_traits <- mom_traits
  child_r_traits <- mom_traits
  return(rbind(child_l_traits,child_r_traits))
}