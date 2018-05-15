#' @title Shrink param set towards a point.
#'
#' @description
#' Shrinks a paramset towards the best found point.
#' Numeric values are shrinked to an interval around the best obtained value of half of
#' the previous length in each iteration, while for discrete variables, a random, 
#' not best-performing level is dropped.
#'
#' @template arg_parset
#' @param x.df [(`data.frame`)]\cr
#'   `data.frame` containing the x values to shrink around.
#' @return [\code{\link{ParamSet}}]
#' @export
shrinkParSet = function(par.set, x.df) {
  x.list = dfRowToList(x.df, par.set, 1L)
  # shrink each parameter set
  par.set$pars = lapply(par.set$pars, function(par) {
    # only shrink when there is a value
    val = x.list[[par$id]]
    if (!isScalarNA(val)) {
      if (isNumeric(par)) {
        if (!isFeasible(par, val)) stop(sprintf("Parameter value %s is not feasible for %s!", val, par$id))
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
          val.del = setdiff(val.names, as.character(val))
          # remove the parameter from param values
          to.del = which(val.names == sample(val.del, 1))
          par$values = par$values[-to.del]
        }
      }
    }
    return(par)
  })
  return(par.set)
}


