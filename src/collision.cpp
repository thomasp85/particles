#include <cpp11/doubles.hpp>
#include <cpp11/matrix.hpp>
#include <deque>
#include "vector.h"
#include "quadTree.h"

[[cpp11::register]]
cpp11::writable::doubles_matrix<> collision_c(cpp11::doubles_matrix<> pos,
                                              cpp11::doubles_matrix<> vel,
                                              cpp11::doubles radii,
                                              double strength) {
  size_t i;
  cpp11::writable::doubles_matrix<> res(pos.nrow(), pos.ncol());
  QuadTree<2> tree;
  std::deque<Body<2> *> bodies;
  for (i = 0; i < pos.nrow(); ++i) {
    Body<2> * body = new Body<2>();
    body->pos.coord[0] = pos(i, 0);
    body->pos.coord[1] = pos(i, 1);
    body->vel.coord[0] = vel(i, 0);
    body->vel.coord[1] = vel(i, 1);
    body->radius = radii[i];
    body->strength = strength;
    bodies.push_back(body);
  }

  tree.insertBodies(bodies);

  for (i = 0; i < pos.nrow(); ++i) {
    tree.collideBodies(bodies[i]);
  }
  for (i = 0; i < pos.nrow(); ++i) {
    res(i, 0) = bodies[i]->force.coord[0];
    res(i, 1) = bodies[i]->force.coord[1];
  }

  return res;
}
