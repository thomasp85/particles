create_universe <- function(...) {
  structure(list(
    parameters = list(...),
    genesis = list(),
    forces = list()
  ), class = 'universe')
}
add_force <- function(universe, force) {
  universe$forces <- append(universe$forces, list(force))
  universe
}
