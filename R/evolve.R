#' Move the simulation forward one or more steps
#'
#' This is the function that move the simulation forward in time. It is possible
#' to either specify the number of steps that should be simulated or let the
#' smulation terminate as `alpha_min` is reached. Note that some values of
#' `alpha` and `alpha_target` does not allow alpha to converge to `alpha_min` so
#' letting the simulation self-terminate can result in an infinete loop. The
#' default settings will result in `alpha_min` being reached in 300 generations.
#'
#' @details
#' Each generation in the simulation progress in the following manner:
#'
#' 1. Check whether the speficied number of generations has been reached
#' 2. Check whether `alpha_min` has been reached
#' 3. If either 1. or 2. is true, terminate the simulation
#' 4. Apply the forces on the current particle positions and velocities in the
#' order they have been added
#' 5. Reduce the velocity according to the given `velocity_decay`
#' 6. Calculate the new particle positions based on the new velocity
#' 7. Update the position and velocity based on any provided constraints
#' 8. If given, call the `on_generation` function.
#'
#' @param simulation A simulation object
#' @param steps The number of generations to progress. If `NULL` the simulation
#' will run until `alpha_min` has been reached.
#' @param on_generation A function to be called after each generation has been
#' progressed. The function will get the current state of the simulation as the
#' first argument.
#' @param ... Additional arguments to `on_generation`
#'
#' @return A simulation object with updated positions and velocities
#'
#' @export
#'
#' @examples
#' graph <- igraph::make_graph('folkman')
#' sim <- graph %>%
#'   simulate() %>%
#'   wield(link_force) %>%
#'   wield(manybody_force)
#'
#' # Take 5 steps and tell about it
#' sim %>% evolve(5, function(sim) {
#'   cat('Generation: ', evolutions(sim), '\n', sep = '')
#' })
#'
#' # Run evolution until alpha_min is reached
#' sim %>% evolve(NULL)
#'
evolve <- function(simulation, steps = NULL, on_generation = NULL, ...) {
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
    velocity(simulation) <- evolution$velocity * (1 - universe_def(simulation)$velocity_decay)
    position(simulation) <- position(simulation) + velocity(simulation)

    constrained_evolution <- Reduce(function(l ,r) {
      apply_constraint(r, particles, l$position, l$velocity, alpha)
    }, constraints(simulation), init = list(position = position(simulation), velocity = velocity(simulation)))
    velocity(simulation) <- constrained_evolution$velocity
    position(simulation) <- constrained_evolution$position

    simulation <- advance(simulation)
    if (!is.null(on_generation)) {
      on_generation(
        simulation,
        ...
      )
    }
  }
  simulation
}
