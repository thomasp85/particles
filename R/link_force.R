#' Attract or repel linked particles
#'
#' This force works between linked particles and either attracts or repel them
#' from each other depending on the value of the strength and distance
#' parameters. The force is stronger the longer the linked particles are from
#' each other, mimicking the mechanics of a rubber band.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `strength` : The attractive force between the linked particles. The default
#'   weighs edges from low-degree particles higher
#'   (`strength = 1 / (min(degree(from), degree(to)))`). (*tidy eval*)
#' - `distance` : The desired distance between linked particles. Defaults to
#'   `30` (*tidy eval*)
#' - `n_iter` : The number of iteration towards the optimal solution per
#'   generation. Higher values leads to faster convergence (measured in number
#'   of generations) at the expense of longer computation time per generation.
#'   Defaults to `1`.
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
link_force <- structure(list(
  strength = NULL,
  distance = NULL,
  count = NULL,
  bias = NULL,
  n_iter = NULL,
  strength_quo = NULL,
  distance_quo = NULL
), class = c('link_force', 'force'))
#' @export
print.link_force <- function(x, ...) {
  cat('Link Force:\n')
  cat('* A force implementing attraction between connected particles\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
#' @importFrom igraph degree
#' @export
train_force.link_force <- function(force, particles, strength = NULL, distance = NULL, n_iter = 1, ...) {
  force <- NextMethod()
  force$strength_quo <- enquo(strength)
  force$distance_quo <- enquo(distance)
  edges <- as_tibble(particles, 'edges')
  count <- degree(particles)
  strength <- eval_tidy(force$strength_quo, edges) %||% (1 / pmin(count[edges$from], count[edges$to]))
  distance <- eval_tidy(force$distance_quo, edges) %||% 30

  force$n_iter <- n_iter
  force$strength <- rep(strength, length.out = nrow(edges))
  force$distance <- rep(distance, length.out = nrow(edges))
  force$bias <- count[edges$from] / (count[edges$from] + count[edges$to])
  force$count <- count
  force
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_force.link_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  force$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
  edges <- as_tibble(particles, active = 'edges')
  if (new_particles) {
    force$count <- degree(particles)
    force$bias <- force$count[edges$from] / (force$count[edges$from] + force$count[edges$to])
  }
  force <- update_quo(force, 'strength', dots, edges, new_particles, 1 / pmin(force$count[edges$from], force$count[edges$to]))
  force <- update_quo(force, 'distance', dots, edges, new_particles, 30)
  force <- update_unquo(force, 'n_iter', dots)
  force
}
#' @importFrom tidygraph as_tibble
#' @importFrom stats runif
#' @export
apply_force.link_force <- function(force, particles, pos, vel, alpha, ...) {
  edges <- as_tibble(particles, 'edges')
  for (i in seq_len(force$n_iter)) {
    dist <- pos[edges$to, ] + vel[edges$to, ] - pos[edges$from, ] - vel[edges$from, ]
    dist[dist == 0] <- runif(sum(dist == 0), min = -0.5, max = 0.5) * 1e-6
    l <- sqrt(rowSums(dist^2))
    l <- (l - force$distance) / l * alpha * force$strength
    dist <- dist * cbind(l, l)
    all_mod <- rbind(
      dist * cbind(1 - force$bias, 1 - force$bias),
      -dist * cbind(force$bias, force$bias)
    )
    mod_index <- split(seq_len(nrow(all_mod)), c(edges$from, edges$to))
    all_mod_sum <- do.call(rbind, lapply(mod_index, function(i) {
      colSums(all_mod[i, , drop = FALSE])
    }))
    vel[as.integer(names(mod_index)), ] <- vel[as.integer(names(mod_index)), ] + all_mod_sum
  }
  list(position = pos, velocity = vel)
}
