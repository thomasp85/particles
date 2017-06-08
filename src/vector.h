//
//  primitives.h
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

#ifndef __primitives_h
#define __primitives_h
#include <Rcpp.h>
#include <cmath>        // std::abs
#include <vector>

using namespace std;

struct IVector {
  virtual double& operator [](size_t idx) = 0;
};

template <size_t DIMENSION>
struct VectorN : public IVector {
  static const int size = DIMENSION;
  double coord[size];

  VectorN() {
    for(int i = 0; i < size; ++i) coord[i] = 0;
  }

  VectorN(const VectorN &other) {
    for(int i = 0; i < size; ++i) {
      coord[i] = other.coord[i];
    }
  }

  virtual double& operator [](size_t idx) {
    return coord[idx];
  }

  friend VectorN operator-(VectorN left, const VectorN& right) {
    VectorN result(left);
    result.sub(right);
    return result;
  }

  friend VectorN operator+(VectorN left, const VectorN& right) {
    VectorN result(left);
    result.add(right);
    return result;
  }

  bool isZero() {
    for (int i = 0; i < size; ++i) {
      if (coord[i] != 0) return false;
    }
    return true;
  }

  void reset () {
    for (int i = 0; i < size; ++i) coord[i] = 0;
  }

  bool sameAs(const VectorN &other) {
    for (int i = 0; i < size; ++i) {
      if (std::abs(coord[i] - other.coord[i]) >= 1e-8) return false;
    }
    return true;
  }

  bool operator==(const VectorN &other) {
    for (int i = 0; i < size; ++i) {
      if (coord[i] != other.coord[i]) return false;
    }
    return true;
  }

  void operator=(const VectorN &other) {
    for (int i = 0; i < size; ++i) coord[i] = other.coord[i];
  }

  double length() {
    double sum = 0;
    for (int i = 0; i < size; ++i) sum += coord[i] * coord[i];
    return sqrt(sum);
  }

  VectorN* multiplyScalar(const double &scalar) {
    for (int i = 0; i < size; ++i) coord[i] *= scalar;
    return this;
  }

  VectorN* setMedian(const VectorN min, const VectorN max) {
    for(int i = 0; i < size; ++i) coord[i] = (min.coord[i] + max.coord[i]) / 2.0;
    return this;
  }

  VectorN* set(const VectorN &other) {
    for (int i = 0; i < size; ++i) coord[i] = other.coord[i];
    return this;
  }

  VectorN* set(double c) {
    for (int i = 0; i < size; ++i) coord[i] = c;
    return this;
  }

  VectorN* normalize() {
    auto length = this->length();
    for (int i = 0; i < size; ++i) coord[i] /= length;
    return this;
  }

  VectorN* addScaledVector(VectorN &v, double s) {
    for (int i = 0; i < size; ++i) coord[i] += v.coord[i] * s;
    return this;
  }

  VectorN* sub(const VectorN &other) {
    for (int i = 0; i < size; ++i) coord[i] -= other.coord[i];
    return this;
  }
  VectorN* add(const VectorN &other) {
    for (int i = 0; i < size; ++i) coord[i] += other.coord[i];
    return this;
  }
  VectorN* relax() {
    for (int i = 0; i < size; ++i) {
      if (coord[i] == 0) coord[i] = R::runif(-0.5, 0.5) * 1e-6;
    }
    return this;
  }
  double distSquared(const VectorN &other) {
    VectorN norm = *(this) - other;
    double sum = 0;
    for (int i = 0; i < size; ++i) sum += norm.coord[i] * norm.coord[i];
    return sum;
  }
  double dot(const VectorN &other) {
    double dot = 0;
    for (int i = 0; i < size; ++i) dot += coord[i] * other.coord[i];
    return dot;
  }
};

template <>
struct VectorN<3> : public IVector {
  static const int size = 3;
  double coord[size];

  VectorN() {
    coord[0] = coord[1] = coord[2] = 0;
  }

  VectorN(const VectorN &other) {
    coord[0] = other.coord[0];
    coord[1] = other.coord[1];
    coord[2] = other.coord[2];
  }

  virtual double& operator [](size_t idx) {
    return coord[idx];
  }

  friend VectorN operator-(VectorN left, const VectorN& right) {
    VectorN result(left);
    result.sub(right);
    return result;
  }

  friend VectorN operator+(VectorN left, const VectorN& right) {
    VectorN result(left);
    result.add(right);
    return result;
  }

  bool isZero() {
    return coord[0] == 0 && coord[1] == 0 && coord[2] == 0;

  }

  void reset () {
    coord[0] = coord[1] = coord[2] = 0;
  }

  bool sameAs(const VectorN &other) {
    return std::abs(coord[0] - other.coord[0]) < 1e-8 &&
      std::abs(coord[1] - other.coord[1]) < 1e-8 &&
      std::abs(coord[2] - other.coord[2]) < 1e-8;
  }

  bool operator==(const VectorN &other) {
    return coord[0] == other.coord[0] &&
      coord[1] == other.coord[1] &&
      coord[2] == other.coord[2];
  }

  void operator=(const VectorN &other) {
    coord[0] = other.coord[0];
    coord[1] = other.coord[1];
    coord[2] = other.coord[2];
  }

  double length() {
    double sum = coord[0] * coord[0] + coord[1] * coord[1] + coord[2] * coord[2];
    return sqrt(sum);
  }

  VectorN* multiplyScalar(const double &scalar) {
    coord[0] *= scalar;
    coord[1] *= scalar;
    coord[2] *= scalar;
    return this;
  }

  VectorN* setMedian(const VectorN min, const VectorN max) {
    coord[0] = (min.coord[0] + max.coord[0])/2;
    coord[1] = (min.coord[1] + max.coord[1])/2;
    coord[2] = (min.coord[2] + max.coord[2])/2;
    return this;
  }

  VectorN* set(const VectorN &other) {
    coord[0] = other.coord[0];
    coord[1] = other.coord[1];
    coord[2] = other.coord[2];
    return this;
  }

  VectorN* set(double c) {
    coord[0] = c;
    coord[1] = c;
    coord[2] = c;
    return this;
  }

  VectorN* normalize() {
    auto length = this->length();
    coord[0] /= length;
    coord[1] /= length;
    coord[2] /= length;
    return this;
  }

  VectorN* addScaledVector(VectorN &v, double s) {
    coord[0] += v.coord[0] * s;
    coord[1] += v.coord[1] * s;
    coord[2] += v.coord[2] * s;
    return this;
  }

  VectorN* sub(const VectorN &other) {
    coord[0] -= other.coord[0];
    coord[1] -= other.coord[1];
    coord[2] -= other.coord[2];
    return this;
  }
  VectorN* add(const VectorN &other) {
    coord[0] += other.coord[0];
    coord[1] += other.coord[1];
    coord[2] += other.coord[2];
    return this;
  }
  VectorN* relax() {
    if (coord[0] == 0) coord[0] = R::runif(-0.5, 0.5) * 1e-6;
    if (coord[1] == 0) coord[1] = R::runif(-0.5, 0.5) * 1e-6;
    if (coord[2] == 0) coord[2] = R::runif(-0.5, 0.5) * 1e-6;
    return this;
  }
  double distSquared(const VectorN &other) {
    VectorN norm = *(this) - other;
    double sum = norm.coord[0] * norm.coord[0] + norm.coord[1] * norm.coord[1] + norm.coord[2] * norm.coord[2];
    return sum;
  }
  double dot(const VectorN &other) {
    return coord[0] * other.coord[0] + coord[1] * other.coord[1] + coord[2] * other.coord[2];
  }
};

template <>
struct VectorN<2> : public IVector {
  static const int size = 2;
  double coord[size];

  VectorN() {
    coord[0] = coord[1] = 0;
  }

  VectorN(const VectorN &other) {
    coord[0] = other.coord[0];
    coord[1] = other.coord[1];
  }

  virtual double& operator [](size_t idx) {
    return coord[idx];
  }

  friend VectorN operator-(VectorN left, const VectorN& right) {
    VectorN result(left);
    result.sub(right);
    return result;
  }

  friend VectorN operator+(VectorN left, const VectorN& right) {
    VectorN result(left);
    result.add(right);
    return result;
  }

  bool isZero() {
    return coord[0] == 0 && coord[1] == 0;

  }

  void reset () {
    coord[0] = coord[1] = 0;
  }

  bool sameAs(const VectorN &other) {
    return std::abs(coord[0] - other.coord[0]) < 1e-8 &&
      std::abs(coord[1] - other.coord[1]) < 1e-8;
  }

  bool operator==(const VectorN &other) {
    return coord[0] == other.coord[0] &&
      coord[1] == other.coord[1];
  }

  void operator=(const VectorN &other) {
    coord[0] = other.coord[0];
    coord[1] = other.coord[1];
  }

  double length() {
    return sqrt(coord[0] * coord[0] + coord[1] * coord[1]);
  }

  VectorN* multiplyScalar(const double &scalar) {
    coord[0] *= scalar;
    coord[1] *= scalar;
    return this;
  }

  VectorN* setMedian(const VectorN min, const VectorN max) {
    coord[0] = (min.coord[0] + max.coord[0])/2;
    coord[1] = (min.coord[1] + max.coord[1])/2;
    return this;
  }

  VectorN* set(const VectorN &other) {
    coord[0] = other.coord[0];
    coord[1] = other.coord[1];
    return this;
  }

  VectorN* set(double c) {
    coord[0] = c;
    coord[1] = c;
    return this;
  }

  VectorN* normalize() {
    auto length = this->length();
    coord[0] /= length;
    coord[1] /= length;
    return this;
  }

  VectorN* addScaledVector(VectorN &v, double s) {
    coord[0] += v.coord[0] * s;
    coord[1] += v.coord[1] * s;
    return this;
  }

  VectorN* sub(const VectorN &other) {
    coord[0] -= other.coord[0];
    coord[1] -= other.coord[1];
    return this;
  }
  VectorN* add(const VectorN &other) {
    coord[0] += other.coord[0];
    coord[1] += other.coord[1];
    return this;
  }
  VectorN* relax() {
    if (coord[0] == 0) coord[0] = R::runif(-0.5, 0.5) * 1e-6;
    if (coord[1] == 0) coord[1] = R::runif(-0.5, 0.5) * 1e-6;
    return this;
  }
  double distSquared(const VectorN &other) {
    VectorN norm = *(this) - other;
    double sum = norm.coord[0] * norm.coord[0] + norm.coord[1] * norm.coord[1];
    return sum;
  }
  double dot(const VectorN &other) {
    return coord[0] * other.coord[0] + coord[1] * other.coord[1];
  }
};


class NotEnoughQuadSpaceException: public exception {};

#endif
