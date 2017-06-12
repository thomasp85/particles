#' @rdname wield
#'
#' @param constraint A constraint object
#'
#' @importFrom rlang enquo
#' @export
#'
impose <- function(simulation, constraint, ..., name, include = TRUE) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.constraint(constraint))
  include <- enquo(include)
  universe(simulation) <- add_constraint(
    universe(simulation),
    name = name,
    train_constraint(constraint, particles(simulation), ..., include = include)
  )
  simulation
}
