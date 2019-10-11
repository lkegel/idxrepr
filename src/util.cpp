#include <Rcpp.h>
#include <stdio.h>
#include <stdlib.h>
using namespace Rcpp;

// [[Rcpp::export]]
void write_float8_c(NumericVector x, const char * fp, int size,
                    const char * mod) {
  double *xout = (double *) malloc(sizeof(double) * size);
  for (int i = 0; i < size; i++)
    xout[i] = x[i];

  FILE *fd = fopen(fp, mod);
  fwrite(xout, sizeof(double), size, fd);
  fclose(fd);

  free(xout);
}

// [[Rcpp::export]]
void write_uint2_c(IntegerVector x, const char * fp, int size,
                   const char * mod) {
  unsigned short int *xout = (unsigned short int*) malloc(sizeof(unsigned short int) * size);
  for (int i = 0; i < size; i++)
    xout[i] = x[i];

  FILE *fd = fopen(fp, mod);
  fwrite(xout, sizeof(unsigned short int), size, fd);
  fclose(fd);

  free(xout);
}

// [[Rcpp::export]]
void write_uint1_c(IntegerVector x, const char * fp, int size,
                  const char * mod) {
  unsigned char *xout = (unsigned char*) malloc(sizeof(unsigned char) * size);
  for (int i = 0; i < size; i++)
    xout[i] = x[i];

  FILE *fd = fopen(fp, mod);
  fwrite(xout, sizeof(unsigned char), size, fd);
  fclose(fd);

  free(xout);
}

// [[Rcpp::export]]
NumericVector read_float8_c(const char * fp, int size, int pos1, int pos2) {
  double *xin = (double *) malloc(sizeof(double) * size);
  NumericVector result(size);

  FILE *fd = fopen(fp, "rb");
  if (pos1 > 0) {
    int64_t offset = (int64_t) pos1 * (int64_t) pos2 * sizeof(double);
#ifdef _WIN32
    int ret = _fseeki64(fd, offset, SEEK_SET);
#else
    int ret = fseek(fd, offset, SEEK_SET);
#endif
    if (ret != 0)
      perror("Error with fseek in read_float8_c");
  }
  fread(xin, sizeof(double), size, fd);
  fclose(fd);

  for(int i = 0; i < size; i++) {
    result[i] = xin[i];
  }

  free(xin);

  return result;
}

// [[Rcpp::export]]
IntegerVector read_uint2_c(const char * fp, int size, int pos1, int pos2) {
  unsigned short int *xin = (unsigned short int*) malloc(sizeof(unsigned short int) * size);
  IntegerVector result(size);

  FILE *fd = fopen(fp, "rb");
  if (pos1 > 0) {
    int64_t offset = (int64_t) pos1 * (int64_t) pos2 * sizeof(unsigned short int);
#ifdef _WIN32
    int ret = _fseeki64(fd, offset, SEEK_SET);
#else
    int ret = fseek(fd, offset, SEEK_SET);
#endif
    if (ret != 0)
      perror("Error with fseek in read_float8_c");
  }
  fread(xin, sizeof(unsigned short int), size, fd);
  fclose(fd);

  for(int i = 0; i < size; i++) {
    result[i] = xin[i];
  }

  free(xin);

  return result;
}

// [[Rcpp::export]]
IntegerVector read_uint1_c(const char * fp, int size, int pos1, int pos2) {
  unsigned char *xin = (unsigned char*) malloc(sizeof(unsigned char) * size);
  IntegerVector result(size);

  FILE *fd = fopen(fp, "rb");
  if (pos1 > 0) {
    int64_t offset = (int64_t) pos1 * (int64_t) pos2 * sizeof(unsigned char);
#ifdef _WIN32
    int ret = _fseeki64(fd, offset, SEEK_SET);
#else
    int ret = fseek(fd, offset, SEEK_SET);
#endif
    if (ret != 0)
      perror("Error with fseek in read_float8_c");
  }
  fread(xin, sizeof(unsigned char), size, fd);
  fclose(fd);

  for(int i = 0; i < size; i++) {
    result[i] = xin[i];
  }

  free(xin);

  return result;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
f <- tempfile()
print(f)
write_series(c(1.0, 2.0, 3.0), f, 3)
*/
