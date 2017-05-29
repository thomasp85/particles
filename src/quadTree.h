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
  VectorN<Dimension> force;
  double mass = 1.0;

  Body() {}

  Body(VectorN<Dimension> _pos): pos(_pos) { }

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
  Body<N> *body;

  double mass;        // This is total mass of the current node;
  VectorN<N> massVector; // This is a center of the mass-vector for the current node;

  VectorN<N> minBounds;    // "left" bounds of the node.
  VectorN<N> maxBounds;    // "right" bounds of the node.


  QuadTreeNode() : quads(1 << N) {}
  ~QuadTreeNode() {}

  void reset() {
    for(size_t i = 0; i < quads.size(); ++i) quads[i] = NULL;
    body = NULL;
    massVector.reset();
    mass = 0;
    minBounds.set(0);
    maxBounds.set(0);
  }

  bool isLeaf() const {
    return body != NULL;
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
  double _gravity;

  NodePool<N> treeNodes;
  QuadTreeNode<N> *root;

  QuadTreeNode<N> *createRootNode(const std::vector<Body<N> *> &bodies) {
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
    if (node->isLeaf()) {
      // We are trying to add to the leaf node.
      // We have to convert current leaf into "internal node"
      // and continue adding two nodes.
      Body<N> *oldBody = node->body;

      // Node is not considered a leaf it has no body:
      node->body = NULL;

      if (oldBody->pos.sameAs(body->pos)) {
        // Ugh, both bodies are at the same position. Let's try to
        // bump them within the quadrant:
        int retriesCount = 3;
        do {
          double offset = R::runif(min_double, max_double);
          VectorN<N> diff = node->maxBounds - node->minBounds;
          diff.multiplyScalar(offset)->add(node->minBounds);

          oldBody->pos.set(diff);
          retriesCount -= 1;
          // Make sure we don't bump it out of the box. If we do, next iteration should fix it
        } while (retriesCount > 0 && oldBody->pos.sameAs(body->pos));

        if (retriesCount == 0 && oldBody->pos.sameAs(body->pos)) {
          // This is very bad, we ran out of precision.
          // We cannot proceed under current root's constraints, so let's
          // throw - this will cause parent to give bigger space for the root
          // node, and hopefully we can fit on the subsequent iteration.
          NotEnoughQuadSpaceException  _NotEnoughQuadSpaceException;
          throw _NotEnoughQuadSpaceException;
        }
      }
      // Insert both bodies into a node that is no longer a leaf:
      insert(oldBody, node);
      insert(body, node);
    } else {
      // This is internal node. Update the total mass of the node and center-of-mass.
      VectorN<N>& pos = body->pos;
      node->mass += body->mass;
      node->massVector.addScaledVector(pos, body->mass);

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
        child->body = body;
        node->quads[quadIdx] = child;
      }
    }
  }

public:
  QuadTree() : QuadTree(-1.2, 0.8) {}
  QuadTree(const double &gravity, const double &theta) : _theta(theta), _gravity(gravity) {}

  void insertBodies(const std::vector<Body<N> *> &bodies) {
    for (int attempt = 0; attempt < 3; ++attempt) {
      try {
        treeNodes.reset();
        root = createRootNode(bodies);
        if (bodies.size() > 0) {
          root->body = bodies[0];
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

    auto visitNode = [&](const QuadTreeNode<N> *node) -> bool {
      Body<N> *body = node->body;
      if (node->body == sourceBody) return false; // no need to traverse: This is current body

      if (node->isLeaf()) {
        VectorN<N> dt = body->pos - sourceBody->pos;
        auto dist = dt.length();
        if (dist == 0) {
          dist = 0.1;
        }
        auto v = _gravity * body->mass * sourceBody->mass / (dist * dist * dist);
        force.addScaledVector(dt, v);

        return false; // no need to traverse this route;
      }

      // This is not a leaf then.
      // Calculate the ratio s / r,  where s is the width of the region
      // represented by the internal node, and r is the distance between the body
      // and the node's center-of-mass

      VectorN<N> centerOfMass(node->massVector);
      centerOfMass.multiplyScalar(1./node->mass);

      VectorN<N> dt = centerOfMass - sourceBody->pos;
      auto distanceToCenterOfMass = dt.length();

      if (distanceToCenterOfMass == 0) {
        distanceToCenterOfMass = 0.1;
      }

      auto regionWidth = node->maxBounds.coord[0] - node->minBounds.coord[0];
      // If s / r < θ, treat this entire node as a single body, and calculate the
      // force it exerts on sourceBody. Add this amount to sourceBody's net force.
      if (regionWidth / distanceToCenterOfMass < _theta) {
        // in the if statement above we consider node's width only
        // because the region was squarified during tree creation.
        // Thus there is no difference between using width or height.
        auto v = _gravity * node->mass * sourceBody->mass / (distanceToCenterOfMass * distanceToCenterOfMass * distanceToCenterOfMass);
        force.addScaledVector(dt, v);
        return false;
      }

      // Otherwise, run the procedure recursively on each of the current node's children.
      return true;
    };

    traverse<N>(root, visitNode);

    sourceBody->force.add(force);
  }

  virtual QuadTreeNode<N>* getRoot() {
    return root;
  }
};

#endif /* defined(__layout____quadTree__) */
