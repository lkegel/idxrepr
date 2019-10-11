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
double d_seassaxpaa(NumericVector x,
                           NumericVector y,
                           int B,
                           int L,
                           int f,
                           NumericVector qseas) {

  double xmin, xmax, ymin, ymax;
  double xmin_seas, ymax_seas;
  double d = 0.0;

  for (int j = 0; j < L; j++) {
    xmin_seas = qseas[x[j]];
    ymax_seas = qseas[y[j] + 1];

    for (int k = 0; k < B; k++) {
      xmin = xmin_seas + x[k + L];
      ymax = ymax_seas + y[k + L];

      if (xmin > ymax) {
        d += pow(xmin - ymax, 2);
      } else {
        xmax = qseas[x[j] + 1] + x[k + L];
        ymin = qseas[y[j]]     + y[k + L];
        if (ymin > xmax) {
          d += pow(ymin - xmax, 2);
        }
      }
    }
  }

  return sqrt(d * f);
}
