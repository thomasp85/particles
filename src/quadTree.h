//
//  quadTree.h
//  layout++
//
//  Created by Andrei Kashcha on 5/21/15.
//  Copyright (c) 2015 Andrei Kashcha. All rights reserved.
//
//  Copied from https://github.com/anvaka/quadtree.cc/blob/master/include/quadtree.cc/quadtree.h
//  on 28/5/17 commit ad61580ad243d482492b799b673a8b7f51367665
//
//  Modified by Thomas Lin Pedersen
//

#ifndef __quadTree__
#define __quadTree__

#include <vector>
#include <cmath>
#include <functional>
#include <Rcpp.h>

#include "vector.h"
const double min_double = std::numeric_limits<double>::min();
const double max_double = std::numeric_limits<double>::max();

/**
* A single physical body in the tree node
*/
template <size_t Dimension>
struct Body {
  VectorN<Dimension> pos;
  VectorN<Dimension> vel;
  VectorN<Dimension> force;
  double strength = 0;
  double radius = 0;
  bool collided = false;

  Body() {}

  Body(VectorN<Dimension> _pos, double _strength): pos(_pos), strength(_strength) { }

  void setPos(const VectorN<Dimension> &_pos) {
    pos = _pos;
  }

  bool positionInitialized() {
    return !pos.isZero();
  }
};

struct IQuadTreeNode {
public:
  virtual ~IQuadTreeNode() {};
  virtual IVector *getMin() = 0;
  virtual IVector *getMax() = 0;
};

template <size_t N>
struct QuadTreeNode : public IQuadTreeNode {
  std::vector<QuadTreeNode *> quads;
  std::vector<Body<N> *> bodies;

  double strength = 0;        // This is total strength of the current node;
  double radius = 0;
  VectorN<N> massVector; // This is a center of the mass-vector for the current node;

  VectorN<N> minBounds;    // "left" bounds of the node.
  VectorN<N> maxBounds;    // "right" bounds of the node.


  QuadTreeNode() : quads(1 << N) {}
  ~QuadTreeNode() {}

  void reset() {
    for(size_t i = 0; i < quads.size(); ++i) quads[i] = NULL;
    bodies.clear();
    massVector.reset();
    strength = 0;
    radius = 0;
    minBounds.set(0);
    maxBounds.set(0);
  }

  bool isLeaf() const {
    return !bodies.empty();
  }

  virtual IVector *getMin() {
    return (IVector *)&minBounds;
  }
  virtual IVector *getMax() {
    return (IVector *)&maxBounds;
  }
};

/**
* Iterates over each node of the quadtree. `visitor()` takes current node
* and returns true or false. If true is returned, then iterator should
* continue descent into this node. Otherwise iteration should not descent.
*/
template <size_t N>
void traverse(const QuadTreeNode<N> *node, const std::function<bool(const QuadTreeNode<N> *node)> &visitor) {
  auto shouldDescent = visitor(node);
  if (shouldDescent) {
    int size = node->quads.size();
    for (int i = 0; i < size; ++i) {
      if (node->quads[i]) traverse(node->quads[i], visitor);
    }
  }
}

/**
* This class manages creation of a QuadTree nodes between iterations.
* So that we are not creating to much memor pressure.
*/
template <size_t N>
class NodePool {
  size_t currentAvailable = 0;
  std::vector<QuadTreeNode<N> *> pool;

public:
  ~NodePool() {
    for(auto node : pool) {
      delete node;
    }
  }

  void reset() {
    currentAvailable = 0;
  }

  /**
  * Gets a new node from the pool.
  */
  QuadTreeNode<N>* get() {
    QuadTreeNode<N> *result;
    if (currentAvailable < pool.size()) {
      result = pool[currentAvailable];
      result->reset();
    } else {
      result = new QuadTreeNode<N>();
      pool.push_back(result);
    }
    currentAvailable += 1;
    return result;
  }
};

class IQuadTree {
public:
  virtual IQuadTreeNode* getRoot() = 0;
};

template<size_t N>
class QuadTree : public IQuadTree {
  double _theta;
  double _mindist;
  double _maxdist;
  double _alpha;

  NodePool<N> treeNodes;
  QuadTreeNode<N> *root;

  QuadTreeNode<N> *createRootNode(const std::deque<Body<N> *> &bodies) {
    QuadTreeNode<N> *root = treeNodes.get();
    VectorN<N> &min = root->minBounds;
    VectorN<N> &max = root->maxBounds;
    min.set(INT32_MAX);
    max.set(INT32_MIN);

    const int size = N;
    for (auto body : bodies) {
      for(int i = 0; i < size; ++i) {
        double v = body->pos.coord[i];
        if (v < min.coord[i]) min.coord[i] = v;
        if (v > max.coord[i]) max.coord[i] = v;
      }
    }

    // squarify bounds:
    double maxSide = 0;
    for (int i = 0; i < size; ++i) {
      double side = max.coord[i] - min.coord[i];
      if (side > maxSide) maxSide = side;
    }

    if (maxSide == 0) {
      maxSide = bodies.size() * 500;
      for (int i = 0; i < size; ++i) {
        min.coord[i] -= maxSide;
        max.coord[i] += maxSide;
      }
    } else {
      for (int i = 0; i < size; ++i) max.coord[i] = min.coord[i] + maxSide;
    }

    return root;
  }
  void insert(Body<N> *body, QuadTreeNode<N> *node) {
    if (node->radius < body->radius) node->radius = body->radius;

    if (node->isLeaf()) {
      if (node->bodies[0]->pos.sameAs(body->pos)) {
        // Stack it together with the other coincident bodies
        node->bodies.push_back(body);
        node->strength += body->strength;
      } else {
        // We are trying to add to the leaf node.
        // We have to convert current leaf into "internal node"
        // and continue adding two nodes.
        std::vector<Body<N> *> oldBodies = node->bodies;

        // Node is not considered a leaf it has no body:
        node->bodies.clear();
        node->strength = 0;
        // Insert all bodies into a node that is no longer a leaf:
        insert(body, node);
        // TODO: improve perf by not inserting all coincident bodies one by one
        for (size_t i = 0; i < oldBodies.size(); ++i) {
          insert(oldBodies[i], node);
        }
      }
    } else {
      // This is internal node. Update the total mass of the node and center-of-mass.
      VectorN<N>& pos = body->pos;
      node->strength += body->strength;
      node->massVector.addScaledVector(pos, body->strength);

      // Recursively insert the body in the appropriate quadrant.
      // But first find the appropriate quadrant.
      int quadIdx = 0; // Assume we are in the 0's quad.
      VectorN<N> tempMin(node->minBounds), tempMax;
      tempMax.setMedian(node->minBounds, node->maxBounds);

      for (size_t i = 0; i < N; ++i) {
        if (pos.coord[i] > tempMax.coord[i]) {
          quadIdx += (1 << i);
          auto oldLeft = tempMin.coord[i];
          tempMin.coord[i] = tempMax.coord[i];
          tempMax.coord[i] = tempMax.coord[i] + (tempMax.coord[i] - oldLeft);
        }
      }

      QuadTreeNode<N> *child = node->quads[quadIdx];
      if (child) {
        // continue searching in this quadrant.
        insert(body, child);
      } else {
        // The node is internal but this quadrant is not taken. Add subnode to it.
        child = treeNodes.get();
        child->minBounds.set(tempMin);
        child->maxBounds.set(tempMax);
        child->bodies.push_back(body);
        child->strength += body->strength;
        child->radius = body->radius;
        node->quads[quadIdx] = child;
      }
    }
  }

public:
  QuadTree() {}
  QuadTree(const double &theta, const double &mindist, const double &maxdist, const double &alpha) : _theta(theta), _mindist(mindist), _maxdist(maxdist), _alpha(alpha) {}

  void insertBodies(const std::deque<Body<N> *> &bodies) {
    for (int attempt = 0; attempt < 3; ++attempt) {
      try {
        treeNodes.reset();
        root = createRootNode(bodies);
        if (bodies.size() > 0) {
          root->bodies.push_back(bodies[0]);
          root->strength = bodies[0]->strength;
        }

        for (size_t i = 1; i < bodies.size(); ++i) {
          insert(bodies[i], root);
        }
        return; // no need to retry - everything inserted properly.
      } catch(NotEnoughQuadSpaceException &e) {
        // well we tried, but some bodies ended up on the same
        // spot, cannot do anything, but hope that next iteration will fix it
      }
    }
    Rcpp::stop("Could not insert bodies: Not enought tree precision");
  }
  void updateBodyForce(Body<N> *sourceBody) {
    VectorN<N> force;
    force.reset();

    auto visitNode = [&](const QuadTreeNode<N> *node) -> bool {
      std::vector<Body<N> *> bodies = node->bodies;
      if (std::find(bodies.begin(), bodies.end(), sourceBody) != bodies.end()) return false; // no need to traverse: This is current body

      if (node->isLeaf()) {
        VectorN<N> dt = bodies[0]->pos - sourceBody->pos;
        dt.relax();
        auto dist = dt.length();
        if (_maxdist <= 0 || dist < _maxdist) {
          if (dist < _mindist) {
            dist = std::sqrt(std::sqrt(float(dist * dist * _mindist * _mindist)));
          }
          auto v = node->strength * _alpha / (dist * dist);
          force.addScaledVector(dt, v);
        }
        return false; // no need to traverse this route;
      }

      // This is not a leaf then.
      // Calculate the ratio s / r,  where s is the width of the region
      // represented by the internal node, and r is the distance between the body
      // and the node's center-of-mass

      VectorN<N> centerOfMass(node->massVector);
      centerOfMass.multiplyScalar(1./node->strength);

      VectorN<N> dt = centerOfMass - sourceBody->pos;
      dt.relax();
      auto distanceToCenterOfMass = dt.length();

      auto regionWidth = node->maxBounds.coord[0] - node->minBounds.coord[0];
      // If s / r < θ, treat this entire node as a single body, and calculate the
      // force it exerts on sourceBody. Add this amount to sourceBody's net force.
      if (regionWidth / distanceToCenterOfMass < _theta) {
        // in the if statement above we consider node's width only
        // because the region was squarified during tree creation.
        // Thus there is no difference between using width or height.
        if (_maxdist <= 0 || distanceToCenterOfMass < _maxdist) {
          if (distanceToCenterOfMass < _mindist) {
            distanceToCenterOfMass = std::sqrt(std::sqrt(float(distanceToCenterOfMass * distanceToCenterOfMass * _mindist * _mindist)));
          }
          auto v = node->strength * _alpha / (distanceToCenterOfMass * distanceToCenterOfMass);
          force.addScaledVector(dt, v);
        }
        return false;
      }

      // Otherwise, run the procedure recursively on each of the current node's children.
      return true;
    };

    traverse<N>(root, visitNode);

    sourceBody->force.add(force);
  }

  void collideBodies(Body<N> *sourceBody) {
    VectorN<N> posi = sourceBody->pos + sourceBody->vel;
    double ri2 = sourceBody->radius * sourceBody->radius;

    auto visitNode = [&](const QuadTreeNode<N> *node) -> bool {
      if (std::find(node->bodies.begin(), node->bodies.end(), sourceBody) != node->bodies.end()) return false;

      double r_sum = sourceBody->radius + node->radius;
      if (node->isLeaf()) {
        if (!node->bodies[0]->collided) {
          VectorN<N> dt = posi - node->bodies[0]->pos - node->bodies[0]->vel;
          dt.relax();
          auto dist = dt.length();
          if (r_sum > dist) {
            dist = sourceBody->strength * (r_sum - dist) / dist;
            double r_mod = (node->radius * node->radius) / (ri2 + node->radius);
            dt.multiplyScalar(dist);
            VectorN<N> dt_rev(dt);
            dt.multiplyScalar(r_mod);
            dt_rev.multiplyScalar(1 - r_mod);
            sourceBody->force.add(dt);
            node->bodies[0]->force.sub(dt_rev);
          }
        }
        return false;
      }
      for (size_t i = 0; i < posi.size; ++i) {
        if (node->minBounds.coord[i] > posi.coord[i] + r_sum) return false;
        if (node->maxBounds.coord[i] < posi.coord[i] - r_sum) return false;
      }
      return true;
    };
    traverse<N>(root, visitNode);
    sourceBody->collided = true;
  }

  virtual QuadTreeNode<N>* getRoot() {
    return root;
  }
};

#endif /* defined(__layout____quadTree__) */
