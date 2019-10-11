#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector to_series_dseas(NumericVector sigma12,
                              int l1,
                              int l2,
                              int n) {
  NumericVector series(n);

  for (int i = 0; i < n; ++i) {
    series[i] = sigma12[i % l1] + sigma12[(i / l1) % l2 + l1];
  }

  return series;
}

