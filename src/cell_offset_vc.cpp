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
NumericVector cell_offset_vc(NumericVector qnorm,
                             IntegerVector x,
                             IntegerVector y,
                             NumericVector xoff,
                             NumericVector yoff) {
  int n = x.size();
  NumericVector d(n);
  double xmin, ymin, xmax, ymax;
  LogicalVector xmi_gt_yma(n), ymi_gt_xma;

  for(int i = 0; i < n; ++i) {
    xmin = xoff[i] + qnorm[x[i]];
    xmax = xoff[i] + qnorm[x[i] + 1];
    ymin = yoff[i] + qnorm[y[i]];
    ymax = yoff[i] + qnorm[y[i] + 1];

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


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
cell_offset_vc(42)
*/
