#' @export
wield <- function(simulation, force, ...) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.force(force))
  universe(simulation) <- add_force(
    universe(simulation),
    train_force(force, particles(simulation), ...)
  )
  simulation
}
