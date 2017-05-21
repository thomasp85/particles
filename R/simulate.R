#' @importFrom tidygraph as_tbl_graph
simulate <- function(graph, ...) {
  structure(list(
    particles = as_tbl_graph(graph),
    universe = create_universe(...)
  ), class = 'particle_system')
}
