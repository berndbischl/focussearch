#' @title Shrink param set towards a point.
#'
#' @description
#' fooo
#'
#' @template arg_parset
#' @return [\code{\link{ParamSet}}]
#' @export
shrinkParSet = function(par.set, x.df) {
  x.list = dfRowToList(x.df, par.set, 1L)
  # print(x.list)
  par.set$pars = lapply(par.set$pars, function(par) {
    # only shrink when there is a value
    val = x.list[[par$id]]
    # print(val)
    # print(par)
    if (!isScalarNA(val)) {
      if (isNumeric(par)) {
        # shrink to range / 2, centered at val
        range = par$upper - par$lower
        par$lower = pmax(par$lower, val - (range / 4))
        par$upper = pmin(par$upper, val + (range / 4))
        if (isInteger(par)) {
          par$lower = floor(par$lower)
          par$upper = ceiling(par$upper)
        }
      } else if (isDiscrete(par)) {
        # randomly drop a level, which is not val
        if (length(par$values) > 1L) {
          val.names = names(par$values)
          # remove current val from delete options, should work also for NA
          val.names = setdiff(val.names, val)
          to.del = sample(seq_along(val.names), 1)
          par$values = par$values[-to.del]
        }
      }
    }
    # print(par)
    return(par)
  })
  return(par.set)
}


