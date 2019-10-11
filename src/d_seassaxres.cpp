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
double d_seassaxres(NumericVector x,
                           NumericVector y,
                           int B,
                           int L,
                           double f,
                           NumericVector qres,
                           NumericVector qseas) {

  double d = 0.0;
  double xmin, ymin, xmax, ymax, xmin_seas, ymax_seas;

  for (int j = 0; j < L; j++) {
    xmin_seas = qseas[x[j]];
    ymax_seas = qseas[y[j] + 1];

    for (int k = 0; k < B; k++) {

      xmin = xmin_seas + qres[x[k + L]];
      ymax = ymax_seas + qres[y[k + L] + 1];

      if (xmin > ymax) {
        d += pow(xmin - ymax, 2);
      } else {
        xmax = qseas[x[j] + 1] + qres[x[k + L] + 1];
        ymin = qseas[y[j]]     + qres[y[k + L]];
        if (ymin > xmax) {
          d += pow(ymin - xmax, 2);
        }
      }
    }
  }

  return sqrt(d * f);
}

// [[Rcpp::export]]
double d_seassaxres_lut(NumericVector x,
                        NumericVector y,
                        int T,
                        int B,
                        int L,
                        NumericMatrix mres,
                        NumericMatrix mseas) {

  double d = 0.0;

  for (int j = 0; j < L; j++) {
    for (int k = 0; k < B; k++) {
      double c2_seas = mseas(x[j], y[j]);
      double c2_res = mres(x[k + L], y[k + L]);
      if (c2_seas > -1 * c2_res) {
        d += pow(c2_seas + c2_res, 2.0);
      } else {
        double c2_seas_inv = mseas(y[j], x[j]);
        double c2_res_inv = mres(y[k + L], x[k + L]);
        if (c2_seas_inv > -1 * c2_res_inv) {
          d += pow(c2_seas_inv + c2_res_inv, 2.0);
        }
      }
    }
  }

  return sqrt((double) T / (double)(B * L)) * sqrt(d);
}
