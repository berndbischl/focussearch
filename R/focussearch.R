#' @title Run focus search.
#'
#' @description
#' fooo
#'
#' @template arg_fn
#' @template arg_parset
#' @template arg_control
#' @return [\code{\link{ParamSet}}]
#' @export
focussearch = function(fn, par.set, control, ...) {
  assertClass(par.set, "ParamSet")
  assertClass(control, "FocusSearchControl")
  global.y = Inf
  # restart the whole crap some times
  for (restart.iter in seq_len(control$restarts)) {
    par.set.local = par.set
    # do iterations where we focus the region-of-interest around the current best point
    for (local.iter in seq_len(control$maxit)) {
      z = doRandomSearch(f, par.set, control, ...)
      # if we found a new best value, store it
      if (z$y < global.y) {
        global.x = z$x
        global.y = z$y
      }
      # now shrink ps so we search more locally
      par.set.local = shrinkParSet(par.set.local, z$x)
      print(par.set.local)
    }
  }
  list(y = global.y, x = global.x)
}





