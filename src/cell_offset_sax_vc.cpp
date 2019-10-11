#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector cell_offset_sax_vc(NumericVector qnorm,
                                 IntegerVector x,
                                 IntegerVector y,
                                 NumericVector qoff,
                                 IntegerVector xoff,
                                 IntegerVector yoff) {
  int n = x.size();
  NumericVector d(n);
  double xmin, ymin, xmax, ymax;

  for(int i = 0; i < n; ++i) {
    xmin = qoff[xoff[i]]     + qnorm[x[i]];
    xmax = qoff[xoff[i] + 1] + qnorm[x[i] + 1];
    ymin = qoff[yoff[i]]     + qnorm[y[i]];
    ymax = qoff[yoff[i] + 1] + qnorm[y[i] + 1];

    if (xmin > ymax) {
      d[i] = xmin - ymax;
    } else if (ymin > xmax) {
      d[i] = ymin - xmax;
    } else {
      d[i] = 0;
    }
  }

  return d;
}
