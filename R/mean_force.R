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
train_force.center_force <- function(force, particles, include_self = FALSE, mode = 'all', ...) {
  force$include_self <- include_self
  force$mode <- mode
  force
}
#' @importFrom igraph ego
apply_force.center_force <- function(force, particles, pos, vel, alpha, ...) {
  neighbors <- ego(particles, order = 1, mode = force$mode, mindist = if (force$include_self) 0 else 1)
  vel_mod <- do.call(rbind, lapply(neighbors, function(i) {
    colSums(vel[i, , drop = FALSE])
  }))
  vel <- vel + vel_mod
  list(position = pos, velocity = vel)
}
