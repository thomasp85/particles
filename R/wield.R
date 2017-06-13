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
#' @param name The name of the force. For use when accessing the force at a
#' later stage. If no name is given the force is accessible by its index in the
#' stack.
#'
#' @param include The particles to be affected by this force. Defaults to every
#' particle in the simulation
#'
#' @return A simulation with the force or constraint added
#'
#' @importFrom rlang enquo
#' @export
#'
#' @examples
#' graph <- igraph::make_graph('folkman')
#' graph %>%
#'   simulate() %>%
#'   wield(link_force)
#'
wield <- function(simulation, force, ..., name, include = TRUE) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.force(force))
  include <- enquo(include)
  universe(simulation) <- add_force(
    universe(simulation),
    name = name,
    train_force(force, particles(simulation), ..., include = include)
  )
  simulation
}
rewield <- function(simulation, name, ...) {
  stopifnot(is.simulation(simulation))
  universe(simulation) <- modify_force(
    universe(simulation),
    name = name,
    retrain_force(get_force(universe(simulation), name), particles(simulation), ...)
  )
}
unwield <- function(simulation, name) {
  stopifnot(is.simulation(simulation))
  universe(simulation) <- remove_force(universe(simulation), name)
  simulation
}
