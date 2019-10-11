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
double d_sax(NumericMatrix m,
             IntegerVector x,
             IntegerVector y) {
  double result = 0.0;

  for (int i = 0; i < x.length(); i++) {
    result += pow(m(x[i], y[i]), 2.0);
  }

  return result;
}
