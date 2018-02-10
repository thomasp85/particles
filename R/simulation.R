#' @rdname simulate
#' @aliases simulation
#' @param x,simulation A simulation object
#' @export
is.simulation <- function(x) inherits(x, 'simulation')
#' @export
print.simulation <- function(x, ...) {
  cat('A particle simulation:\n')
  cat('* ', gorder(particles(x)), ' particles\n', sep = '')
  f <- forces(x)
  cn <- constraints(x)
  cat('* ', length(f), ' Force', if (length(f) == 1) '\n' else 's\n', sep = '')
  lapply(f, function(ff) cat('  - ', class(ff)[1], '\n', sep = ''))
  cat('* ', length(cn), ' Constraint', if (length(f) == 1) '\n' else 's\n', sep = '')
  lapply(cn, function(ff) cat('  - ', class(ff)[1], '\n', sep = ''))
  cat('* ', evolutions(x), ' Evolution', if (evolutions(x) == 1) '\n' else 's\n', sep = '')
}
#' @importFrom tidygraph as_tbl_graph mutate activate
#' @export
as_tbl_graph.simulation <- function(x, ...) {
  graph <- particles(x)
  pos <- position(x)
  vel <- velocity(x)
  if (gorder(graph) > nrow(pos)) {
    fill <- matrix(NA, ncol = 2, nrow = gorder(graph) - nrow(pos))
    pos <- rbind(pos, fill)
    vel <- rbind(vel, fill)
  }
  mutate(activate(graph, 'nodes'),
         x = pos[, 1],
         y = pos[, 2],
         x_vel = vel[, 1],
         y_vel = vel[, 2])
}
#' @importFrom tidygraph as_tibble
#' @export
as_tibble.simulation <- function(x, ...) {
  as_tibble(as_tbl_graph(x))
}
#' @describeIn simulate Save the current state in the simulation's history
#' @export
record <- function(simulation, ...) {
  history <- simulation
  history$history <- list()
  simulation$history <- append(simulation$history, list(history))
  simulation
}
#' @describeIn simulate Clear the current history from the simulation
#' @export
clear_history <- function(simulation) {
  simulation$history <- list()
  simulation
}
#' @describeIn simulate Retrieve a simulation from the history
#' @param age The version to retrieve. Positive numbers count from the
#' beginning, while negative numbers counts backwards from current version.
#' Defaults to `-1`.
#' @export
get_history <- function(simulation, age = -1) {
  if (age <= 0) {
    age <- length(simulation$history) + age
    if (age <= 0) stop('Can\'t go back that far', call. = FALSE)
  }
  if (age > history_length(simulation)) stop('Not that many records in the simulation', call. = TRUE)
  state <- simulation$history[[age]]
  state$history <- simulation$history[seq_len(age)]
  state
}
#' @describeIn simulate Get the number of versions stored in the history of the simulation
#' @export
history_length <- function(simulation) {
  length(simulation$history)
}
universe <- function(simulation) {
  stopifnot(is.simulation(simulation))
  simulation$universe
}
`universe<-` <- function(simulation, value) {
  stopifnot(is.universe(value))
  stopifnot(is.simulation(simulation))
  simulation$universe <- value
  simulation
}
universe_def <- function(simulation) {
  universe(simulation)$parameters
}
`universe_def<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.list(value))
  universe(simulation)$parameters <- value
  simulation
}
#' @describeIn simulate Extract the particle graph from a simulation
#' @export
particles <- function(simulation) {
  stopifnot(is.simulation(simulation))
  simulation$particles
}
#' @importFrom tidygraph as_tbl_graph
`particles<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  simulation$particles <- as_tbl_graph(value)
  simulation
}
alpha <- function(simulation, progress = FALSE) {
  stopifnot(is.simulation(simulation))
  alpha <- universe_def(simulation)$alpha
  if (progress) {
    alpha <- alpha + (universe_def(simulation)$alpha_target - alpha) * universe_def(simulation)$alpha_decay
  }
  alpha
}
`alpha<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  universe_def(simulation)$alpha <- value
  simulation
}
forces <- function(simulation) {
  stopifnot(is.simulation(simulation))
  universe(simulation)$forces
}
`forces<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.list(value))
  stopifnot(all(vapply(value, inherits, logical(1), 'force')))
  universe(simulation)$forces <- value
  simulation
}
constraints <- function(simulation) {
  stopifnot(is.simulation(simulation))
  universe(simulation)$constraints
}
`constraints<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.list(value))
  stopifnot(all(vapply(value, inherits, logical(1), 'constraint')))
  universe(simulation)$constraints <- value
  simulation
}
#' @describeIn simulate Extract the position coordinates from a simulation
#' @export
position <- function(simulation) {
  stopifnot(is.simulation(simulation))
  simulation$position
}
`position<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.matrix(value))
  stopifnot(dim(position(simulation)) == dim(value))
  simulation$position <- value
  simulation
}
#' @describeIn simulate Extract the velocity verlets from a simulation
#' @export
velocity <- function(simulation) {
  stopifnot(is.simulation(simulation))
  simulation$velocity
}
`velocity<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  stopifnot(is.matrix(value))
  stopifnot(dim(velocity(simulation)) == dim(value))
  simulation$velocity <- value
  simulation
}
#' @describeIn simulate Get the number of generations the simulation has undergone
#' @export
evolutions <- function(simulation) {
  stopifnot(is.simulation(simulation))
  simulation$evolutions
}
`evolutions<-` <- function(simulation, value) {
  stopifnot(is.simulation(simulation))
  simulation$evolutions <- value
  simulation
}
advance <- function(simulation) {
  stopifnot(is.simulation(simulation))
  evolutions(simulation) <- evolutions(simulation) + 1
  simulation
}
retrain <- function(simulation) {
  stopifnot(is.simulation(simulation))
  forces(simulation) <- lapply(forces(simulation), retrain_force, particles = particles(simulation))
  constraints(simulation) <- lapply(constraints(simulation), retrain_constraint, particles = particles(simulation))
  simulation
}
