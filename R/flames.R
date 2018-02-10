r <- function(pos) matrix(sqrt(rowSums(pos^2)), ncol = 1)
theta <- function(pos) matrix(atan2(pos[,2], pos[,1]), ncol = 1)
phi <- function(pos) matrix(atan2(pos[,1], pos[,2]), ncol = 1)
omega <- function(pos) matrix(sample(c(0, pi), nrow(pos), replace = TRUE), ncol = 1)
lambda <- function(pos) matrix(sample(c(-1, 1), nrow(pos), replace = TRUE), ncol = 1)
psi <- function(pos) matrix(runif(nrow(pos), 0, 1), ncol = 1)


linear <- function(pos, a, b, c, d, e, f) pos
sinusoidal <- function(pos, a, b, c, d, e, f) sin(pos)
spherical <- function(pos, a, b, c, d, e, f) pos/((r(pos)^2)[, c(1,1)])
swirl <- function(pos, a, b, c, d, e, f) {
  r2 <- r(pos)^2
  sinr <- sin(r2)
  cosr <- cos(r2)
  cbind(pos[,1]*sinr - pos[,2]*cosr, pos[,1]*cosr + pos[,2]*sinr)
}
horseshoe <- function(pos, a, b, c, d, e, f) {
  cbind((pos[,1]-pos[,2])*(pos[,1]+pos[,2]), 2*pos[,1]*pos[,2])/r(pos)[, c(1,1)]
}
polar <- function(pos, a, b, c, d, e, f) cbind(theta(pos)/pi, r(pos) - 1)
handkerchief <- function(pos, a, b, c, d, e, f) {
  r1 <- r(pos)
  theta1 <- theta(pos)
  cbind(sin(theta1 + r1) * r1, cos(theta1 - r1) * r1)
}
heart <- function(pos, a, b, c, d, e, f) {
  r1 <- r(pos)
  thetar <- theta(pos)*r1
  cbind(sin(thetar)*r1, -cos(thetar)*r1)
}
disc <- function(pos, a, b, c, d, e, f) {
  r1pi <- r(pos)*pi
  cbind(sin(r1pi), cos(r1pi)) * theta(pos)[,c(1,1)]/pi
}
spiral <- function(pos, a, b, c, d, e, f) {
  r1 <- r(pos)
  theta1 <- theta(pos)
  cbind(cos(theta1) + sin(r1), sin(theta1) - cos(r1)) / r1[,c(1,1)]
}
hyperbolic <- function(pos, a, b, c, d, e, f) {
  r1 <- r(pos)
  theta1 <- theta(pos)
  cbind(sin(theta1)/r1, r1 * cos(theta1))
}
diamond <- function(pos, a, b, c, d, e, f) {
  r1 <- r(pos)
  theta1 <- theta(pos)
  cbind(sin(theta1) * cos(r1), cos(theta1) * sin(r1))
}
ex <- function(pos, a, b, c, d, e, f) {
  r1 <- r(pos)
  theta1 <- theta(pos)
  p0 <- sin(theta1 + r1)^3
  p1 <- cos(theta1 - r1)^3
  cbind((p0+p1)*r1, (p0-p1)*r1)
}
julia <- function(pos, a, b, c, d, e, f) {
  theta1 <- theta(pos)
  omega1 <- omega(pos)
  rsq <- sqrt(r(pos))
  cbind(cos(theta1/2 + omega1)*rsq, sin(theta1/2 + omega1)*rsq)
}
bent <- function(pos, a, b, c, d, e, f) {
  bigx <- pos[,1] >= 0
  bigy <- pos[,2] >= 0
  cbind(ifelse(bigx, pos[,1], 2*pos[,1]), ifelse(bigy, pos[,2], pos[,2]/2))
}
waves <- function(pos, a, b, c, d, e, f) {
  pos + cbind(b * sin(pos[,2]/c^2), e * sin(pos[,1]/f^2))
}
fisheye <- function(pos, a, b, c, d, e, f) {
  mod <- 2/(r(pos) + 1)
  mod[, c(1,1)] * pos[,c(2,1)]
}
popcorn <- function(pos, a, b, c, d, e, f) {
  pos + cbind(c * sin(tan(pos[,2] * 3)), f * sin(tan(pos[,1] * 3)))
}
exponental <- function(pos, a, b, c, d, e, f) {
  piy <- pos[,2]*pi
  mod <- exp(pos[,1, drop = FALSE] - 1)
  mod[, c(1,1)] * cbind(cos(piy), sin(piy))
}
power <- function(pos, a, b, c, d, e, f) {
  theta1 <- theta(pos)
  rmod <- r(pos)^sin(theta1)
  rmod[,c(1,1)] * cbind(cos(theta1), sin(theta1))
}
cosine <- function(pos, a, b, c, d, e, f) {
  cbind(cos(pos[,1] * pi) * cosh(pos[,2]), -sin(pos[,1] * pi) * sinh(pos[,2]))
}
rings <- function(pos, a, b, c, d, e, f) {
  c2 <- c^2
  r1 <- r(pos)
  theta1 <- theta(pos)
  mod <- (r1 + c2) %% (2*c2) - c2 + r1 * (1-c2)
  mod[, c(1,1)] * cbind(cos(theta1), sin(theta1))
}
fan <- function(pos, a, b, c, d, e, f) {
  r1 <- r(pos)
  theta1 <- theta(pos)
  t <- pi * c^2
  t2 <- ifelse((theta1 + f) %% t > t/2, -t/2, t/2)
  cbind(cos(theta1 + t2), sin(theta1 + t2)) * r1[,c(1,1)]
}
blob <- function(pos, high, low, waves) {
  function(pos, a, b, c, d, e, f) {

  }
}
