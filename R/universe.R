create_universe <- function(alpha = 1, alpha_min = 0.001,
                            alpha_decay = 1- alpha_min^(1/300), alpha_target = 0,
                            velocity_decay = 0.6,
                            setup = default_genesis(), ...) {
  structure(list(
    parameters = list(
      alpha = alpha,
      alpha_min = alpha_min,
      alpha_decay = alpha_decay,
      alpha_target = alpha_target,
      velocity_decay = velocity_decay,
      ...
    ),
    genesis = setup,
    forces = list(),
    constraints = list()
  ), class = 'universe')
}
is.universe <- function(x) inherits(x, 'universe')

#' @importFrom igraph gorder
#' @export
default_genesis <- function(radius = 10, angle = pi * (3 - sqrt(5))) {
  function(particles, ...) {
    n_particles <- gorder(particles)
    p_radius <- radius * sqrt(seq_len(n_particles))
    p_angle <- angle * seq_len(n_particles)
    pos <- cbind(p_radius * cos(p_angle), p_radius * sin(p_angle))
    vel <- matrix(0, ncol = 2, nrow = n_particles)
    list(position = pos, velocity = vel)
  }
}

# Helpers -----------------------------------------------------------------

init_particles <- function(universe, particles) {
  universe$genesis(particles, universe$parameters)
}
add_force <- function(universe, force) {
  stopifnot(is.universe(universe))
  stopifnot(is.force(force))
  universe$forces <- append(universe$forces, list(force))
  universe
}
