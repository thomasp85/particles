#' Apply the mean velocity of all the neighbors to a particle
#'
#' This force takes the mean of all the neighbors (in the graph sense) of a
#' particle (and optionally itself) and applies it to itself.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `include_self` : Should the velocity of itself be included in the mean
#'   calculation
#' - `mode` : How should neighbors be found? `'all'` uses all edges. `'out'`
#'   only uses outbound edges, and `'in'` only uses inbound edges. Ignored for
#'   undirected particle graphs
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
mean_force <- structure(list(
  include_self = NULL,
  mode = NULL
), class = c('mean_force', 'force'))
#' @export
print.mean_force <- function(x, ...) {
  cat('Mean Force:\n')
  cat('* A force that applies the mean velocity of a particle neighbours to itself\n')
}
train_force.mean_force <- function(force, particles, include_self = FALSE, mode = 'all', ...) {
  force$include_self <- include_self
  force$mode <- mode
  force
}
#' @importFrom igraph ego
apply_force.mean_force <- function(force, particles, pos, vel, alpha, ...) {
  neighbors <- ego(particles, order = 1, mode = force$mode, mindist = if (force$include_self) 0 else 1)
  vel_mod <- do.call(rbind, lapply(neighbors, function(i) {
    colSums(vel[i, , drop = FALSE])
  }))
  vel <- vel + vel_mod
  list(position = pos, velocity = vel)
}
