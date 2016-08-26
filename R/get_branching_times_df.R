#' build an informative data frame with the branching events
#' written as:
#' `mom`, the mother node that splitted in that event
#' `child_left` and `child_right`, the two daughter species originated from `mom`
#' `origin_time`, the time (with respect to the root of the tree) at which the event occurred
#'  `delta_time`, the total time until the next branching event
#'  (notice that from `origin_time`, for a period of time defined
#'  by `delta_time`, the number of lineages extant in the tree is fixed)
branching_events <- function(phylo) {
  #' obtain the branching times
  b_times <- sort(ape::branching.times(tree),decreasing = T)

  #' compute the time occurring between one branching event and the next
  #' that, is, the time length of the event
  delta_times <- b_times - dplyr::lead(b_times)
  delta_times[length(delta_times)] <- b_times[length(b_times)]
  
  branev_df <- seq_along(b_times) %>%
  purrr::map_df( ~ .x %>% {
    dplyr::tibble(
      mom = names(b_times[.]),
      child_left = tree$edge[tree$edge[,1] == mom,2][1],
      child_right = tree$edge[tree$edge[,1] == mom,2][2],
      origin_time = b_times[.],
      delta_time = delta_times[.])
  }
  )
  
  return(branev_df)
}