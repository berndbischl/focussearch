#' @title Run focus search.
#'
#' @description
#' Perform random search on a region, that is sequentially shrinked around the best performing parameters.
#' Parameters in (`FocusSearchControl`) allows for controlling the number of sampled points, restarts
#' and the number of shrinking iterations.
#' 
#' _points_ randomly sampled points from the parameter set are used in each iteration.
#' Numeric values are shrinked to an interval around the best obtained value of half of
#' the previous length in each iteration, while for discrete variables, a random, 
#' not best-performing level is dropped.
#' In order to improve method robustness, several restarts can be made.
#'
#' @template arg_fn
#' @template arg_parset
#' @template arg_control
#' @param show.info (`logical(1)`) \cr
#'   Should the parameter sets be printed while training? Defaults is `FALSE`.
#' @param \ldots [any]\cr
#'   Passed to \code{fn}.
#' @return [\code{\link{ParamSet}}]
#' @export
focussearch = function(fn, par.set, control, show.info = FALSE, ...) {
  assertFunction(fn, args = "x")
  assertClass(par.set, "ParamSet")
  assertClass(control, "FocusSearchControl")
  assertFlag(show.info)
  
  global.y = Inf
  # Restart restart.iter times
  for (restart.iter in seq_len(control$restarts)) {
    if (show.info) sprintf("Multistart %f of %f \n", restart.iter, control$restarts)
    par.set.local = par.set
    # do iterations where we focus the region-of-interest around the current best point
    for (local.iter in seq_len(control$maxit)) {
      z = doRandomSearch(fn, par.set, control, ...)
      # if we found a new best value, store it
      if (z$y < global.y) {
        global.x = z$x
        global.y = z$y
      }
      # now shrink ps so we search more locally
      par.set.local = shrinkParSet(par.set.local, z$x)
      if (show.info) print(par.set.local)
    }
  }
  list(y = global.y, x = global.x)
}





