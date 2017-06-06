create_universe <- function(alpha = 1, alpha_min = 0.001,
                            alpha_decay = 1- alpha_min^(1/300), alpha_target = 0,
                            velocity_decay = 0.6,
                            setup = phyllotactic_genesis(), ...) {
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
add_constraint <- function(universe, constraint) {
  stopifnot(is.universe(universe))
  stopifnot(is.constraint(constraint))
  universe$constraints <- append(universe$constraints, list(constraint))
  universe
}
