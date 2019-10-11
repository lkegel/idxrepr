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
int toc(IntegerVector start) {
  struct timeval et;

  gettimeofday(&et,NULL);

  int elapsed = (((et.tv_sec - start[0]) * 1000000) + (et.tv_usec - start[1]));

  return(elapsed);
}

