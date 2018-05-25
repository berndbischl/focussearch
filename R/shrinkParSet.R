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
#' @param check.feasible [`logical(1)`]\cr
#'   Should feasibility of the parameters be checked?
#'   If feasibility is not checked, and invalid params are present,
#'   no shrinking will be done.
#' @return [\code{\link{ParamSet}}]
#' @export
shrinkParSet = function(par.set, x.df, check.feasible = FALSE) {
  x.list = dfRowToList(x.df, par.set, 1L)
  # shrink each parameter set
  par.set$pars = lapply(par.set$pars, function(par) {
    # only shrink when there is a value
    val = x.list[[par$id]]
    if (!isScalarNA(val)) {
      
      if (check.feasible & !isFeasible(par, val)) {
        stop(sprintf("Parameter value %s is not feasible for %s!", val, par$id))
      }
      
      if (isNumeric(par)) {
        range = par$upper - par$lower
        
        if (!is.null(par$trafo)) 
          # Find val on the original scale
          val = uniroot(function(x) {par$trafo(x) - val}, interval = c(par$lower, par$upper),
                        extendInt = "yes", tol = .Machine$double.eps^0.5 * range, maxiter = 10^4)$root
        
        # If it is not feasible we do nothing
        if (isFeasible(par, val)) {
          # shrink to range / 2, centered at val
          par$lower = pmax(par$lower, val - (range / 4))
          par$upper = pmin(par$upper, val + (range / 4))
          if (isInteger(par)) {
            par$lower = floor(par$lower)
            par$upper = ceiling(par$upper)
          }
        }
        
      } else if (isDiscrete(par)) {
        
        if (isFeasible(par, val)) {
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
    }
    return(par)
  })
  return(par.set)
}


