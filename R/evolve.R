#' @export
evolve <- function(simulation, steps = 1, on_generation = NULL, ...) {
  stopifnot(is.simulation(simulation))
  while (TRUE) {
    if (!is.null(steps)) {
      steps <- steps - 1
      if (steps < 0) break
    }
    if (alpha(simulation) < universe_def(simulation)$alpha_min) break
    particles <- particles(simulation)
    alpha <- alpha(simulation, progress = TRUE)
    alpha(simulation) <- alpha
    evolution <- Reduce(function(l ,r) {
      apply_force(r, particles, l$position, l$velocity, alpha)
    }, forces(simulation), init = list(position = position(simulation), velocity = velocity(simulation)))
    velocity(simulation) <- evolution$velocity * universe_def(simulation)$velocity_decay
    position(simulation) <- position(simulation) + velocity(simulation)
    if (!is.null(on_generation)) {
      on_generation(
        particles(simulation),
        position(simulation),
        velocity(simulation),
        ...
      )
    }
  }
  simulation
}
