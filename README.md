# *idxrepr*: Season- and Trend-aware Symbolic Approximations
Processing and analyzing time series datasets have become a central issue in many domains requiring data management systems to support time series as a data type natively. A crucial prerequisite of these systems is time series matching, yet it is a challenging problem. A time series is a high-dimensional data type, its representation is storage-, and its comparison is time-consuming. Among the representation techniques that tackle these challenges, the symbolic aggregate approximation (SAX) is of particular interest. This technique reduces a time series in a low-dimensional space by segmenting it and discretizing each segment into a small alphabet of symbols. However, SAX ignores the deterministic behavior of a time series such as its cyclical repeated season or its trend component affecting all segments and leading to a distortion of the symbolic distribution. In this work, we present a season- and a trend-aware symbolic approximation. We show that they improve a representation's symbolic distribution and increase the representation accuracy without increasing the representation size. Most importantly, they enable a more efficient time series matching by providing a match up to three orders of magnitude faster than SAX.

The *idxrepr* package contains the representation techniques used by the thesis *Feature-based Time Series Analytics*. Subsequently we explain the main components.

## Representation Techniques
The package contains the following representations techniques:

 - PAA: piecewise aggregate approximation
 - SAX: symbolic aggregation approximation
 - 1d-SAX: average and the trend symbols time series segments
 - sSAX = seassaxres: season-aware symbolic approximation
 - tSAX = lrrsaxres: trend-aware symbolic approximation
 - Intermediate representation techniques such as sPAA (seaspaa), tPAA (lrrpaa) etc. that are building blocks for sSAX and tSAX

## Manager
The file manager.R contains all methods that are needed for representing a time series:

 - *mgr_init*: initialize a representation technique by its name
 - *mgr_get_config*: retrieve the configuration of a representation technique
 - *mgr_set_config*: set the configuration of a representation technique
 - *mgr_represent*: represent a time series
 - *mgr_distance*: calculate the distance between two time series representations
 - *mgr_to_series*: if available, return the time series from a representation in the original time domain
 - *mgr_det_symbols*: if available, return the symbols used for the deterministic component of a time series
 - *mgr_res_symbols*: if available, return the symbols used for the residual component of a time series

Example:

	# Two sample time series
    series <- rep(seq(12), 11) + rnorm(132)
    series <- (series - mean(series)) / sd(series)
    series_2 <- rep(seq(-1, -12), 11) + rnorm(132)
    series_2 <- (series_2 - mean(series_2)) / sd(series_2)

    # Initialize seassaxres (sSAX), default configuration: T = 132, W = 6, A_res = A_seas = 3
    method <- mgr_init("seassaxres")
    # Set alphabets A_res = A_seas = 256 and update lookup tables
    method$seassax$sax <- mgr_set_config(method$seassax$sax, list(a = 2**8))
    method$sax <- mgr_set_config(method$sax, list(a = 2**8))
    method <- set_config(method, list())
    
    # Represent time series
    repr <- mgr_represent(method, series)
    repr
    > [1]  21  24  41  59  82 106 149 170 180 216 232 240 129 118 139 126 127 126
    repr_2 <- mgr_represent(method, series_2)
    
    # Calculate distance
    mgr_distance(method, repr, repr_2)
    > [1] 21.74766
    
    # Return symbols of season mask
    mgr_det_symbols(method, repr)
    > [1]  21  24  41  59  82 106 149 170 180 216 232 240
    # Return symbols of residuals
    mgr_res_symbols(method, repr)
    > [1] 129 118 139 126 127 126
    
## Required R Packages

 - distr
 - Rcpp
 - testthat
 - TSrepr
