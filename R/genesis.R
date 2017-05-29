#' Particle initialisation
#'
#' These functions are passed to the simulation and defines how the position and
#' velocity of the particles are initiated. The default is to lay out the nodes
#' in a phyllotactic arrangement (think sunflower seeds) and with no velocity,
#' which is also the default in d3-force.
#'
#' @return A function that takes the particle graph and returns a list with a
#' position and velocity element, each holding a matrix with two columns and a
#' row for each particle giving the x and y position and velocity respectively.
#'
#' @name genesis
#' @rdname genesis
#'
#' @examples
#' # A contrieved example
#' graph <- igraph::make_graph('bull')
#' genesis <- phyllotactic_genesis()
#' genesis(graph)
#'
#' # Usually used as an argument to simulate
#' graph %>%
#'   simulate(setup = phyllotactic_genesis())
#'
NULL

#' @describeIn genesis Initiates particles in a phyllotactic arrangement with zero velocity
#' @importFrom igraph gorder
#' @param radius The radius modifier (will be multiplied by the square root of the index of the particle)
#' @param angle The angular difference between two adjacent particles
#' @export
phyllotactic_genesis <- function(radius = 10, angle = pi * (3 - sqrt(5))) {
  function(particles, ...) {
    n_particles <- gorder(particles)
    p_radius <- radius * sqrt(seq_len(n_particles))
    p_angle <- angle * seq_len(n_particles)
    pos <- cbind(p_radius * cos(p_angle), p_radius * sin(p_angle))
    vel <- matrix(0, ncol = 2, nrow = n_particles)
    list(position = pos, velocity = vel)
  }
}
#' @describeIn genesis Uses information from the node data to set position and velocity.
#' @importFrom rlang enquo eval_tidy
#' @importFrom tidygraph as_tibble
#' @param x,y The columns holding (or value of) the position coordinates
#' @param x_vel,y_vel The columns holding (or value of) the velocity verlets
#' @export
predefined_genesis <- function(x = x, y = y, x_vel = 0, y_vel = 0) {
  x <- enquo(x)
  y <- enquo(y)
  x_vel <- enquo(x_vel)
  y_vel <- enquo(y_vel)
  function(particles, ...) {
    nodes <- as_tibble(particles, active = 'nodes')
    x <- rep(eval_tidy(x, nodes), length.out = nrow(nodes))
    y <- rep(eval_tidy(y, nodes), length.out = nrow(nodes))
    x_vel <- rep(eval_tidy(x_vel, nodes), length.out = nrow(nodes))
    y_vel <- rep(eval_tidy(y_vel, nodes), length.out = nrow(nodes))
    if (any(is.null(x), is.null(y), is.null(x_vel), is.null(y_vel))) {
      stop('Cannot extract start position or velocity', call. = FALSE)
    }
    list(position = cbind(x, y), velocity = cbind(x_vel, y_vel))
  }
}
#' @describeIn genesis Initiates particles at center position and a random velocity
#' @importFrom tidygraph as_tibble
#' @importFrom stats runif
#' @importFrom igraph gorder
#' @param vel_min,vel_max The bounds of the uniformly distributed velocities
#' @export
bigbang_genesis <- function(vel_min = 0, vel_max = 1) {
  function(particles, ...) {
    n_particles <- gorder(particles)
    radius <- runif(n = n_particles, min = vel_min, max = vel_max)
    angle <- runif(n = n_particles, min = 0, max = 2*pi)
    vel <- cbind(radius * cos(angle), radius * sin(angle))
    pos <- matrix(0, ncol = 2, nrow = n_particles)
    list(position = pos, velocity = vel)
  }
}
#' @describeIn genesis Places particles randomly in a rectangle and gives them a random velocity
#' @importFrom igraph gorder
#' @importFrom stats runif
#' @param width,height The size of the rectangle holding the particles
#' @export
aquarium_genesis <- function(width = 10, height = 10, vel_min = 0, vel_max = 1) {
  function(particles, ...) {
    n_particles <- gorder(particles)
    pos <- cbind(runif(n_particles, -width/2, width/2),
                 runif(n_particles, -height/2, height/2))
    radius <- runif(n = n_particles, min = vel_min, max = vel_max)
    angle <- runif(n = n_particles, min = 0, max = 2*pi)
    vel <- cbind(radius * cos(angle), radius * sin(angle))
    list(position = pos, velocity = vel)
  }
}
#' @describeIn genesis Places particles randomly on a disc and gives them a random velocity
#' @importFrom igraph gorder
#' @importFrom stats runif
#' @param max_radius The size of the disc.
#' @export
petridish_genesis <- function(max_radius = 10, vel_min = 0, vel_max = 1) {
  function(particles, ...) {
    n_particles <- gorder(particles)
    radius <- runif(n = n_particles, min = 0, max = max_radius^2)
    angle <- runif(n = n_particles, min = 0, max = 2*pi)
    pos <- cbind(sqrt(radius) * cos(angle), sqrt(radius) * sin(angle))

    radius <- runif(n = n_particles, min = vel_min, max = vel_max)
    angle <- runif(n = n_particles, min = 0, max = 2*pi)
    vel <- cbind(radius * cos(angle), radius * sin(angle))
    list(position = pos, velocity = vel)
  }
}
