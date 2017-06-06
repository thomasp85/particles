#' Assign a force or constraint to a simulation
#'
#' This function adds a new force/constraint to the simulation and trains the it
#' on the current particle graph. The parameters passed on to the training are
#' using tidy evaluation from the rlang package. Depending on the
#' force/constraint the data getting referenced is either the node or the edge
#' data of the particle graph. Both forces and constraints manipulate position
#' and velocity of the particles but they differ in when the are applied during
#' a generation. First forces are applied sequentially and the resulting
#' velocity is added to the resulting position after `velocity_decay` has been
#' applied. After this operation any constraint is imposed on the results. In
#' general, forces tends to calculate velocity adjustments, while constraints
#' modify position and velocity directly, but this difference is not in any way
#' enforced.
#'
#' @param simulation A simulation object
#'
#' @param force A force object
#'
#' @param ... Parameters passed on to the training of the force or constraint
#'
#' @return A simulation with the force or constraint added
#'
#' @export
#'
#' @examples
#' graph <- igraph::make_graph('folkman')
#' graph %>%
#'   simulate() %>%
#'   wield(link_force)
#'
wield <- function(simulation, force, ...) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.force(force))
  universe(simulation) <- add_force(
    universe(simulation),
    train_force(force, particles(simulation), ...)
  )
  simulation
}
