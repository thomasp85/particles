#include <cpp11/doubles.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <math.h>
#include "vector.h"

using namespace cpp11::literals;

VectorN<2> projection(VectorN<2> a, VectorN<2> b, VectorN<2> p, bool clamp) {
  if (a.sameAs(b)) return a;
  double length2 = a.distSquared(b);
  VectorN<2> norm = b - a;
  double t = norm.dot(p - a) / length2;
  if (clamp) {
    t = std::max(0.0, std::min(1.0, t));
  }
  norm.multiplyScalar(t);
  return a + norm;
}
void dist_to_path(double x, double y, cpp11::list_of< cpp11::doubles_matrix<> > path, std::vector<double> &res, bool closed_poly) {
  int i, j, k;
  double dist, shortest_dist = -1;
  VectorN<2> point, a, b, close, closest;
  point.coord[0] = x;
  point.coord[1] = y;
  for (i = 0; i < path.size(); ++i) {
    for (j = 0; j < path[i].nrow(); ++j) {
      if (j == path[i].nrow() && !closed_poly) break;
      a.coord[0] = path[i](j, 0);
      a.coord[1] = path[i](j, 1);
      k = j == path[i].nrow() - 1 ? 0 : j + 1;
      b.coord[0] = path[i](k, 0);
      b.coord[1] = path[i](k, 1);
      close = projection(a, b, point, true);
      dist = std::sqrt(point.distSquared(close));
      if (shortest_dist < 0 || dist < shortest_dist) {
        shortest_dist = dist;
        closest = close;
      }
    }
  }
  res.clear();
  res.push_back(closest.coord[0]);
  res.push_back(closest.coord[1]);
  res.push_back(shortest_dist);
}

[[cpp11::register]]
cpp11::writable::list points_to_path_c(cpp11::doubles_matrix<> pos,
                                       cpp11::list_of< cpp11::doubles_matrix<> > path,
                                       bool close) {
  std::vector<double> res_container;
  cpp11::writable::doubles_matrix<> proj(pos.nrow(), 2);
  cpp11::writable::doubles dist(pos.nrow());
  for (int i = 0; i < pos.nrow(); ++i) {
    dist_to_path(pos(i, 0), pos(i, 1), path, res_container, close);
    proj(i, 0) = res_container[0];
    proj(i, 1) = res_container[1];
    dist[i] = res_container[2];
  }
  return cpp11::writable::list({
    "projection"_nm = proj,
    "distance"_nm = dist
  });
}

[[cpp11::register]]
cpp11::writable::list points_to_lines_c(cpp11::doubles_matrix<> line1,
                                        cpp11::doubles_matrix<> line2,
                                        cpp11::doubles_matrix<> point) {
  if (point.nrow() != line1.nrow() || point.nrow() != line2.nrow()) {
    cpp11::stop("Line and point matrices must have same dimensions");
  }
  cpp11::writable::doubles_matrix<> proj(point.nrow(), 2);
  cpp11::writable::doubles dist(point.nrow());
  VectorN<2> p, a, b, project;
  for (int i = 0; i < point.nrow(); ++i) {
    a.coord[0] = line1(i, 0);
    a.coord[1] = line1(i, 1);
    b.coord[0] = line2(i, 0);
    b.coord[1] = line2(i, 1);
    p.coord[0] = point(i, 0);
    p.coord[1] = point(i, 1);
    project = projection(a, b, p, false);
    proj(i, 0) = project.coord[0];
    proj(i, 1) = project.coord[1];
    dist[i] = std::sqrt(p.distSquared(project));
  }
  return cpp11::writable::list({
    "projection"_nm = proj,
    "distance"_nm = dist
  });
}
[[cpp11::register]]
cpp11::writable::doubles angle_diff_c(cpp11::doubles_matrix<> a,
                                      cpp11::doubles_matrix<> b) {
  double cosine;
  if (a.nrow() != b.nrow())
    cpp11::stop("a and b must have same dimensions");
  if (a.ncol() != 2 || b.ncol() != 2)
    cpp11::stop("a and b must have two columns");
  cpp11::writable::doubles res(a.nrow());
  VectorN<2> vec_a, vec_b;
  for (int i = 0; i < a.nrow(); ++i) {
    vec_a.coord[0] = a(i, 0);
    vec_a.coord[1] = a(i, 1);
    vec_b.coord[0] = b(i, 0);
    vec_b.coord[1] = b(i, 1);
    vec_a.relax();
    vec_b.relax();
    cosine = vec_a.dot(vec_b)/(vec_a.length() * vec_b.length());
    res[i] = std::acos(std::max(-1.0, std::min(1.0, cosine)));
  }
  return res;
}
