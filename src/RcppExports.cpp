// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cell_offset_sax_vc
NumericVector cell_offset_sax_vc(NumericVector qnorm, IntegerVector x, IntegerVector y, NumericVector qoff, IntegerVector xoff, IntegerVector yoff);
RcppExport SEXP _idxrepr_cell_offset_sax_vc(SEXP qnormSEXP, SEXP xSEXP, SEXP ySEXP, SEXP qoffSEXP, SEXP xoffSEXP, SEXP yoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type qnorm(qnormSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type qoff(qoffSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type xoff(xoffSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yoff(yoffSEXP);
    rcpp_result_gen = Rcpp::wrap(cell_offset_sax_vc(qnorm, x, y, qoff, xoff, yoff));
    return rcpp_result_gen;
END_RCPP
}
// cell_offset_vc
NumericVector cell_offset_vc(NumericVector qnorm, IntegerVector x, IntegerVector y, NumericVector xoff, NumericVector yoff);
RcppExport SEXP _idxrepr_cell_offset_vc(SEXP qnormSEXP, SEXP xSEXP, SEXP ySEXP, SEXP xoffSEXP, SEXP yoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type qnorm(qnormSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xoff(xoffSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yoff(yoffSEXP);
    rcpp_result_gen = Rcpp::wrap(cell_offset_vc(qnorm, x, y, xoff, yoff));
    return rcpp_result_gen;
END_RCPP
}
// d_ed
double d_ed(NumericVector x, NumericVector y, int TT);
RcppExport SEXP _idxrepr_d_ed(SEXP xSEXP, SEXP ySEXP, SEXP TTSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type TT(TTSEXP);
    rcpp_result_gen = Rcpp::wrap(d_ed(x, y, TT));
    return rcpp_result_gen;
END_RCPP
}
// d_sax
double d_sax(NumericMatrix m, IntegerVector x, IntegerVector y);
RcppExport SEXP _idxrepr_d_sax(SEXP mSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(d_sax(m, x, y));
    return rcpp_result_gen;
END_RCPP
}
// d_seassaxpaa
double d_seassaxpaa(NumericVector x, NumericVector y, int B, int L, int f, NumericVector qseas);
RcppExport SEXP _idxrepr_d_seassaxpaa(SEXP xSEXP, SEXP ySEXP, SEXP BSEXP, SEXP LSEXP, SEXP fSEXP, SEXP qseasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type f(fSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type qseas(qseasSEXP);
    rcpp_result_gen = Rcpp::wrap(d_seassaxpaa(x, y, B, L, f, qseas));
    return rcpp_result_gen;
END_RCPP
}
// d_seassaxres
double d_seassaxres(NumericVector x, NumericVector y, int B, int L, double f, NumericVector qres, NumericVector qseas);
RcppExport SEXP _idxrepr_d_seassaxres(SEXP xSEXP, SEXP ySEXP, SEXP BSEXP, SEXP LSEXP, SEXP fSEXP, SEXP qresSEXP, SEXP qseasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    Rcpp::traits::input_parameter< double >::type f(fSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type qres(qresSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type qseas(qseasSEXP);
    rcpp_result_gen = Rcpp::wrap(d_seassaxres(x, y, B, L, f, qres, qseas));
    return rcpp_result_gen;
END_RCPP
}
// d_seassaxres_lut
double d_seassaxres_lut(NumericVector x, NumericVector y, int T, int B, int L, NumericMatrix mres, NumericMatrix mseas);
RcppExport SEXP _idxrepr_d_seassaxres_lut(SEXP xSEXP, SEXP ySEXP, SEXP TSEXP, SEXP BSEXP, SEXP LSEXP, SEXP mresSEXP, SEXP mseasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type T(TSEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mres(mresSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mseas(mseasSEXP);
    rcpp_result_gen = Rcpp::wrap(d_seassaxres_lut(x, y, T, B, L, mres, mseas));
    return rcpp_result_gen;
END_RCPP
}
// d_ed_tic
List d_ed_tic(NumericVector x, NumericVector y, int TT);
RcppExport SEXP _idxrepr_d_ed_tic(SEXP xSEXP, SEXP ySEXP, SEXP TTSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type TT(TTSEXP);
    rcpp_result_gen = Rcpp::wrap(d_ed_tic(x, y, TT));
    return rcpp_result_gen;
END_RCPP
}
// d_sax_tic
List d_sax_tic(int n, int w, NumericMatrix m, IntegerVector x, IntegerVector y);
RcppExport SEXP _idxrepr_d_sax_tic(SEXP nSEXP, SEXP wSEXP, SEXP mSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type w(wSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(d_sax_tic(n, w, m, x, y));
    return rcpp_result_gen;
END_RCPP
}
// tic
IntegerVector tic();
RcppExport SEXP _idxrepr_tic() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(tic());
    return rcpp_result_gen;
END_RCPP
}
// to_series_dseas
NumericVector to_series_dseas(NumericVector sigma12, int l1, int l2, int n);
RcppExport SEXP _idxrepr_to_series_dseas(SEXP sigma12SEXP, SEXP l1SEXP, SEXP l2SEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type sigma12(sigma12SEXP);
    Rcpp::traits::input_parameter< int >::type l1(l1SEXP);
    Rcpp::traits::input_parameter< int >::type l2(l2SEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(to_series_dseas(sigma12, l1, l2, n));
    return rcpp_result_gen;
END_RCPP
}
// toc
int toc(IntegerVector start);
RcppExport SEXP _idxrepr_toc(SEXP startSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type start(startSEXP);
    rcpp_result_gen = Rcpp::wrap(toc(start));
    return rcpp_result_gen;
END_RCPP
}
// write_float8_c
void write_float8_c(NumericVector x, const char * fp, int size, const char * mod);
RcppExport SEXP _idxrepr_write_float8_c(SEXP xSEXP, SEXP fpSEXP, SEXP sizeSEXP, SEXP modSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const char * >::type fp(fpSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< const char * >::type mod(modSEXP);
    write_float8_c(x, fp, size, mod);
    return R_NilValue;
END_RCPP
}
// write_uint2_c
void write_uint2_c(IntegerVector x, const char * fp, int size, const char * mod);
RcppExport SEXP _idxrepr_write_uint2_c(SEXP xSEXP, SEXP fpSEXP, SEXP sizeSEXP, SEXP modSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const char * >::type fp(fpSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< const char * >::type mod(modSEXP);
    write_uint2_c(x, fp, size, mod);
    return R_NilValue;
END_RCPP
}
// write_uint1_c
void write_uint1_c(IntegerVector x, const char * fp, int size, const char * mod);
RcppExport SEXP _idxrepr_write_uint1_c(SEXP xSEXP, SEXP fpSEXP, SEXP sizeSEXP, SEXP modSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const char * >::type fp(fpSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< const char * >::type mod(modSEXP);
    write_uint1_c(x, fp, size, mod);
    return R_NilValue;
END_RCPP
}
// read_float8_c
NumericVector read_float8_c(const char * fp, int size, int pos1, int pos2);
RcppExport SEXP _idxrepr_read_float8_c(SEXP fpSEXP, SEXP sizeSEXP, SEXP pos1SEXP, SEXP pos2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char * >::type fp(fpSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type pos1(pos1SEXP);
    Rcpp::traits::input_parameter< int >::type pos2(pos2SEXP);
    rcpp_result_gen = Rcpp::wrap(read_float8_c(fp, size, pos1, pos2));
    return rcpp_result_gen;
END_RCPP
}
// read_uint2_c
IntegerVector read_uint2_c(const char * fp, int size, int pos1, int pos2);
RcppExport SEXP _idxrepr_read_uint2_c(SEXP fpSEXP, SEXP sizeSEXP, SEXP pos1SEXP, SEXP pos2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char * >::type fp(fpSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type pos1(pos1SEXP);
    Rcpp::traits::input_parameter< int >::type pos2(pos2SEXP);
    rcpp_result_gen = Rcpp::wrap(read_uint2_c(fp, size, pos1, pos2));
    return rcpp_result_gen;
END_RCPP
}
// read_uint1_c
IntegerVector read_uint1_c(const char * fp, int size, int pos1, int pos2);
RcppExport SEXP _idxrepr_read_uint1_c(SEXP fpSEXP, SEXP sizeSEXP, SEXP pos1SEXP, SEXP pos2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char * >::type fp(fpSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type pos1(pos1SEXP);
    Rcpp::traits::input_parameter< int >::type pos2(pos2SEXP);
    rcpp_result_gen = Rcpp::wrap(read_uint1_c(fp, size, pos1, pos2));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_idxrepr_cell_offset_sax_vc", (DL_FUNC) &_idxrepr_cell_offset_sax_vc, 6},
    {"_idxrepr_cell_offset_vc", (DL_FUNC) &_idxrepr_cell_offset_vc, 5},
    {"_idxrepr_d_ed", (DL_FUNC) &_idxrepr_d_ed, 3},
    {"_idxrepr_d_sax", (DL_FUNC) &_idxrepr_d_sax, 3},
    {"_idxrepr_d_seassaxpaa", (DL_FUNC) &_idxrepr_d_seassaxpaa, 6},
    {"_idxrepr_d_seassaxres", (DL_FUNC) &_idxrepr_d_seassaxres, 7},
    {"_idxrepr_d_seassaxres_lut", (DL_FUNC) &_idxrepr_d_seassaxres_lut, 7},
    {"_idxrepr_d_ed_tic", (DL_FUNC) &_idxrepr_d_ed_tic, 3},
    {"_idxrepr_d_sax_tic", (DL_FUNC) &_idxrepr_d_sax_tic, 5},
    {"_idxrepr_tic", (DL_FUNC) &_idxrepr_tic, 0},
    {"_idxrepr_to_series_dseas", (DL_FUNC) &_idxrepr_to_series_dseas, 4},
    {"_idxrepr_toc", (DL_FUNC) &_idxrepr_toc, 1},
    {"_idxrepr_write_float8_c", (DL_FUNC) &_idxrepr_write_float8_c, 4},
    {"_idxrepr_write_uint2_c", (DL_FUNC) &_idxrepr_write_uint2_c, 4},
    {"_idxrepr_write_uint1_c", (DL_FUNC) &_idxrepr_write_uint1_c, 4},
    {"_idxrepr_read_float8_c", (DL_FUNC) &_idxrepr_read_float8_c, 4},
    {"_idxrepr_read_uint2_c", (DL_FUNC) &_idxrepr_read_uint2_c, 4},
    {"_idxrepr_read_uint1_c", (DL_FUNC) &_idxrepr_read_uint1_c, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_idxrepr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
