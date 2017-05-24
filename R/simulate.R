#' @importFrom tidygraph as_tbl_graph
#' @export
simulate <- function(graph, ...) {
  graph <- as_tbl_graph(graph)
  universe <- create_universe(...)
  genesis <- init_particles(universe, graph)
  structure(list(
    particles = graph,
    position = genesis$position,
    velocity = genesis$velocity,
    universe = universe
  ), class = 'simulation')
}
