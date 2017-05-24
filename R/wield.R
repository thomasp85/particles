#' Assign a force to a simulation
#'
#' This function adds a new force to the simulation and trains the force on the
#' current particle graph. The parameters passed on to the force training are
#' using tidy evaluation from the rlang package. Depending on the force the
#' data getting referenced is either the node or the edge data of the particle
#' graph.
#'
#' @param simulation A simulation object
#'
#' @param force A force object
#'
#' @param ... Parameters passed on to the training of the force
#'
#' @return A simulation with the force added
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
