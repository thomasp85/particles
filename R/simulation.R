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
  cat('* ', length(f), ' Force', if (length(f) == 1) '\n' else 's\n', sep = '')
  lapply(f, function(ff) cat('  - ', class(ff)[1], '\n', sep = ''))
  cat('* ', evolutions(x), ' Evolution', if (evolutions(x) == 1) '\n' else 's\n', sep = '')
}
#' @importFrom tidygraph as_tbl_graph mutate activate
#' @export
as_tbl_graph.simulation <- function(simulation) {
  graph <- particles(simulation)
  pos <- position(simulation)
  vel <- velocity(simulation)
  mutate(activate(graph, 'nodes'),
         x = pos[, 1],
         y = pos[, 2],
         x_vel = vel[, 1],
         y_vel = vel[, 2])
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
constraints <- function(simulation) {
  stopifnot(is.simulation(simulation))
  universe(simulation)$constraints
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
