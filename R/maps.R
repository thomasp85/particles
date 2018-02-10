van_der_pol <- function(mu = 4) {
  function(pos, ...) {
    cbind(pos[, 2], mu*(1 - pos[, 1]^2)*pos[, 2] - pos[, 1])
  }
}
arnolds_cat <- function(xlim, ylim) {
  mat <- matrix(c(2, 1, 1, 1), ncol = 2)
  function(pos, ...) {
    pos <- norm_pos(pos, xlim, ylim)
    pos <- mat %*% t(pos)
    pos <- t(pos) %% 1
    unnorm_pos(pos, xlim, ylim)
  }
}
baker_folded <- function(xlim, ylim) {
  function(pos, ...) {
    pos <- norm_pos(pos, xlim, ylim)
    new_pos <- cbind(pos[,1]*2, pos[,2]/2)
    new_pos[pos[,1] >= 0.5, ] <- matrix(c(2,1), ncol = 2) - new_pos[pos[,1] >= 0.5, , drop = FALSE]
    unnorm_pos(new_pos, xlim, ylim)
  }
}
baker_unfolded <- function(xlim, ylim) {
  function(pos, ...) {
    pos <- norm_pos(pos, xlim, ylim)
    x2 <- pos[,1] * 2
    pos <- cbind(x2-floor(x2), (pos[,2] + floor(x2))/2)
    unnorm_pos(new_pos, xlim, ylim)
  }
}
bogdanov <- function(epsilon = 0.15, k = 0.275, mu = -1.7) {
  function(pos, ...) {
    new_y <- pos[,2] + epsilon*pos[,2] + k*pos[,1]*(pos[,1]-1) + mu*pos[,1]*pos[,2]
    new_x <- pos[,1] + new_y
    cbind(new_x, new_y)
  }
}
duffing <- function(a, b) {
  function(pos, ...) {
    cbind(
      pos[, 2],
      -b*pos[, 1] + a*pos[, 2] - pos[, 2]^3
    )
  }
}
lorenz <- function(sigma = 10, beta = 8/3, rho = 28) {

}
