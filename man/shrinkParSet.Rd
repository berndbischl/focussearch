% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shrinkParSet.R
\name{shrinkParSet}
\alias{shrinkParSet}
\title{Shrink param set towards a point.}
\usage{
shrinkParSet(par.set, x.df, check.feasible = FALSE)
}
\arguments{
\item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}]\cr
Param set.}

\item{x.df}{[(`data.frame`)]\cr
`data.frame` containing the x values to shrink around.}

\item{check.feasible}{[`logical(1)`]\cr
Should feasibility of the parameters be checked?
If feasibility is not checked, and invalid params are present,
no shrinking will be done.}
}
\value{
[\code{\link{ParamSet}}]
}
\description{
Shrinks a paramset towards the best found point.
Numeric values are shrinked to an interval around the best obtained value of half of
the previous length in each iteration, while for discrete variables, a random, 
not best-performing level is dropped.
}
