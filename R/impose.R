#' @rdname wield
#'
#' @param constraint A constraint object
#'
#' @export
#'
impose <- function(simulation, constraint, ...) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.constraint(constraint))
  universe(simulation) <- add_constraint(
    universe(simulation),
    train_constraint(constraint, particles(simulation), ...)
  )
  simulation
}
