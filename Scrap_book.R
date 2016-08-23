library(dplyr)
library(ape)
library(tidyr)
library(purrr)

#' simulate a tree
tree <- rtree(n = 20)
#' select a discrete time step
dt <- 0.01

#' obtain the branching times
b_times <- sort(branching.times.with.extinct(tree),decreasing = T)
#' save the largest b_times as the total time range of the tree
tot_time <- max(b_times)
#' express the b_times as time distances from the root (rather then from the tips)
b_times <- max(b_times) - b_times
#' compute the time occurring between one branching event and the next
#' that, is, the time length of the event
delta_times <-lead(b_times) - b_times


#' build an informative data frame with the branching events
#' written as:
#' `mom`, the mother node that splitted in that event
#' `child_left` and `child_right`, the two daughter species originated from `mom`
#' `origin_time`, the time (with respect to the root of the tree) at which the event occurred
#'  `delta_time`, the total time until the next branching event
#'  (notice that from `origin_time`, for a period of time defined
#'  by `delta_time`, the number of lineages extant in the tree is fixed)
branching_events <- seq_along(b_times) %>%
  map_df( ~ .x %>% {
    tibble(
      mom = names(b_times[.]),
      child_left = tree$edge[tree$edge[,1] == mom,2][1],
      child_right = tree$edge[tree$edge[,1] == mom,2][2],
      origin_time = b_times[.],
      delta_time = delta_times[.])
    }
  )


#' I prefer to keep all the labels as integers, so they are lighter
#' so here we build a table of original <-> shortened labels
tree_tips <- tree$tip.label
N_tips <- length(tree_tips)
labelz <- tibble(
  original = c(tree_tips,
               seq(N_tips + 1, N_tips + tree$Nnode)),
  short = seq(1, N_tips + tree$Nnode)
)


#' next we allocate the data frame that will keep track of the interaction in the network:
#' ALERT: this version is subpar, as we allocate an entry for each
#' node or tip on the tree at all time steps
#' We can do better using the branching event dataframes to allocate memory
#' only for those node that actually were there at that time.
N_steps <- tot_time / dt + 1
to_from_labels <- cbind(rep(expand.grid(labelz$short,labelz$short)[,1],N_steps),
  rep(expand.grid(labelz$short,labelz$short)[,2],N_steps))

net_series <- tibble(
  from=to_from_labels[,1],
  to=to_from_labels[,2]
  ) %>%
  mutate(link=logical(1),
         to_exists = logical(1),
         from_exists = logical(1),
         time_step = rep(seq(0,tot_time,dt),each=nrow(labelz)^2))

