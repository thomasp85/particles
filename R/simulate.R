#' Start a simulation based on a graph
#'
#' This function initiates a simulation based on the provided graph and
#' parameters. Any graph structure with a [tidygraph::as_tbl_graph()] method
#' is supported as input. This function does not start the simulation but merely
#' sets it up.
#'
#' @details
#' A simulation in the context of the particles package is a series of
#' equidistant steps where the velocity and position of each particle is
#' updated. A few global rules applies to this cycle irrespectively of the
#' forces added to the simulation. Once a simulation is initiated an `alpha`
#' value is defined (defaults to `1`). At each step this alpha value is
#' decreased according to its distance to the `alpha_target` (defaults to `0`)
#' and `alpha_decay` (defaults to ~`0.023`). Once the alpha value gets below
#' `alpha_min` (defaults to `0.001`) the simulation seizes to take additional
#' steps. The default values is adapted from the d3-force implementation and
#' corresponds to 300 steps. Conceptually the `alpha` progression can be seen
#' as a cooling off of the system as the value decreases quiclky in the
#' beginning and then slowly reach the target value. If it is not intended to
#' have a system that cools off, simply set the `alpha_target` value to the same
#' as `alpha`. At each step, after the new particle velocities has been
#' calculated but before they have been applied to the positions, a dampening
#' factor (`velocity_decay`) is applied in order to simulate the gradual loss
#' of momentum. If this is not intended for the simulation, simply set the value
#' to `0`.
#'
#' @param graph A graph in a format supported by tidygraph
#'
#' @param alpha The starting alpha value. See Details.
#'
#' @param alpha_min The minimum alpha value after which the simulation is
#' terminated. See Details.
#'
#' @param alpha_decay The speed at which the alpha value decreases. See Details.
#'
#' @param alpha_target The alpha value that alpha drifts towards. See Details.
#'
#' @param velocity_decay The dampening factor of the system. See Details.
#'
#' @param setup A function that takes the particle graph and returns a start
#' position and velocity to each particle. `particles` provides a range of
#' [genesis] functions to choose from.
#'
#' @param ... Additional parameters for the simulation (currently ignored)
#'
#' @return A simulation object
#'
#' @importFrom tidygraph as_tbl_graph
#' @export
#'
#' @examples
#' graph <- igraph::make_graph('folkman')
#' graph %>%
#'   simulate()
#'
simulate <- function(graph, alpha = 1, alpha_min = 0.001,
                     alpha_decay = 1 - alpha_min^(1/300), alpha_target = 0,
                     velocity_decay = 0.4, setup = phyllotactic_genesis(), ...) {
  graph <- as_tbl_graph(graph)
  universe <- create_universe(alpha = alpha, alpha_min = alpha_min,
                              alpha_decay = alpha_decay,
                              alpha_target = alpha_target,
                              velocity_decay = velocity_decay,
                              setup = setup, ...)
  genesis <- init_particles(universe, graph)
  structure(list(
    particles = graph,
    position = genesis$position,
    velocity = genesis$velocity,
    universe = universe,
    evolutions = 0
  ), class = 'simulation')
}
