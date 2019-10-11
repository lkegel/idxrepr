#include <Rcpp.h>
#include <time.h>

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

timespec diff(timespec start, timespec end);

// [[Rcpp::export]]
List d_ed_tic(NumericVector x, NumericVector y, int TT) {
  double d = 0;

  timespec time1, time2;

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int t = 0; t < TT; t++) {
    d += pow(x[t] - y[t], 2.0);
  }

  d = sqrt(d);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  int elapsed = diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

  List ret;
  ret["distance"] = d;
  ret["elapsed"] = elapsed;

  return ret;
}


// [[Rcpp::export]]
List d_sax_tic(int n,
               int w,
               NumericMatrix m,
               IntegerVector x,
               IntegerVector y) {
  double result = 0.0;
  timespec time1, time2;

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = 0; i < x.length(); i++) {
    result += pow(m(x[i], y[i]), 2.0);
  }

  result *= n / w;
  result = sqrt(result);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  int elapsed = diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

  List ret;
  ret["distance"] = result;
  ret["elapsed"] = elapsed;

  return ret;
}

timespec diff(timespec start, timespec end)
{
  timespec temp;
  if ((end.tv_nsec-start.tv_nsec)<0) {
    temp.tv_sec = end.tv_sec-start.tv_sec-1;
    temp.tv_nsec = 1000000000+end.tv_nsec-start.tv_nsec;
  } else {
    temp.tv_sec = end.tv_sec-start.tv_sec;
    temp.tv_nsec = end.tv_nsec-start.tv_nsec;
  }
  return temp;
}
