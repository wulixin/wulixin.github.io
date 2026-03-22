

Analysis of large groups of time series

Time series features are computed in feasts for time series in tsibble format. 
They are computed using tsfeatures for a list or matrix of time series in ts format. 
In both packages, many built-in feature functions are included, and users can add their own.
Rcatch22 provides fast computation of 22 features identified as particularly useful. 
theft calculates time series features from various R and Python packages. 
fsMTS implements feature selection routines for multivariate time series. 
Feature extraction for ordinal time series is provided by otsfeatures.
Time series clustering is implemented in TSclust, dtwclust, BNPTSclust and pdc.
TSdist provides distance measures for time series data.
TSrepr includes methods for representing time series using dimension reduction and 
feature extraction.
rucrdtw provides R bindings for functions from the UCR Suite to enable 
ultrafast subsequence search for the best match under Dynamic Time Warping and 
Euclidean Distance. 
IncDTW provides incremental calculation of dynamic time warping for streaming time series.
Methods for plotting and forecasting collections of hierarchical and 
grouped time series are provided by fable and hts. thief uses hierarchical 
methods to reconcile forecasts of temporally aggregated time series.
FoReco provides various forecast reconciliation methods for cross-sectional,
temporal, and cross-temporal constrained time series. 
An alternative approach to reconciling forecasts of 
hierarchical time series is provided by gtop. ProbReco (archived) 
provides tools to train forecast reconciliation weights by optimizing 
probability scoring functions.

library(feasts)
library(tsfeatures)
library(Rcatch22)
library(theft)
library(fsMTS)
library(TSclust)
library(dtwclust)
library(BNPTSclust)
library(pdc)
library(TSdist)
library(TSrepr)
library(rucrdtw)
library(fable)
library(hts.thies)
library(FoReco)
library(gtop)
library(ProbReco)















