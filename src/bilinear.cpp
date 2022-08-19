#include <cpp11/doubles.hpp>
#include <cpp11/matrix.hpp>
#include <algorithm>

double interpret(double q11, double q12, double q21, double q22, double x1, double x2, double y1, double y2, double x, double y) {
  double x2x1, y2y1, x2x, y2y, yy1, xx1;
  x2x1 = x2 - x1;
  y2y1 = y2 - y1;
  x2x = x2 - x;
  y2y = y2 - y;
  yy1 = y - y1;
  xx1 = x - x1;
  return 1.0 / (x2x1 * y2y1) * (
    q11 * x2x * y2y +
      q21 * xx1 * y2y +
      q12 * x2x * yy1 +
      q22 * xx1 * yy1
  );
}

[[cpp11::register]]
cpp11::writable::doubles bilinear_c(cpp11::doubles x_breaks, cpp11::doubles y_breaks,
                                    cpp11::doubles_matrix<> grid,
                                    cpp11::doubles x, cpp11::doubles y) {
  if (x.size() != y.size()) {
    cpp11::stop("x and y must be of same length");
  }
  if (x_breaks.size() != grid.ncol() || y_breaks.size() != grid.nrow()) {
    cpp11::stop("x_breaks and y_breaks must match the dimensions of the grid");
  }
  cpp11::writable::doubles interp(x.size());

  int i, x1, x2, y1, y2;

  for (i = 0; i < x.size(); ++i) {
    if (x[i] < x_breaks[0] || x[i] > x_breaks[x_breaks.size() - 1] ||
        y[i] < y_breaks[0] || y[i] > y_breaks[y_breaks.size() - 1]) {
      interp[i] = 0;
      continue;
    }
    auto it = std::lower_bound(x_breaks.begin(), x_breaks.end(), x[i]);
    x1 = it - x_breaks.begin();
    x2 = x1 + 1;
    if (x2 == x_breaks.size()) {
      --x1;
      --x2;
    }
    it = std::lower_bound(y_breaks.begin(), y_breaks.end(), y[i]);
    y1 = it - y_breaks.begin();
    y2 = y1 + 1;
    if (y2 == y_breaks.size()) {
      --y1;
      --y2;
    }
    interp[i] = interpret(grid(y1, x1), grid(y2, x1), grid(y1, x2), grid(y2, x2),
                          x_breaks[x1], x_breaks[x2], y_breaks[y1], y_breaks[y2],
                          x[i], y[i]);
  }

  return interp;
}
