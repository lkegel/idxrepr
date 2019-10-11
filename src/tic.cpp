#include <Rcpp.h>
#include <stdio.h>
#include <sys/time.h>

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

//' @export
// [[Rcpp::export]]
IntegerVector tic() {
  struct timeval st;
  gettimeofday(&st, NULL);

  IntegerVector start(2);
  start[0] = st.tv_sec;
  start[1] = st.tv_usec;

  return(start);
}
