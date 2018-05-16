#' @title Run a random search.
#'
#' @description
#' fooo
#'
#' @template arg_fn
#' @template arg_parset
#' @template arg_control
#' @param \ldots [any]\cr
#'   Passed to \code{fn}.
#' @return [\code{list}]. 
#'   \item{x [list]}{s}
#'   \item{y [numeric]}{s}
#' @export
doRandomSearch = function(fn, par.set, control, ...) {
  
  # predict on design where NAs were imputed, but return proposed points with NAs
  newdesign = generateRandomDesign(control$points, par.set, trafo = TRUE)

  # delete / change invalid params
  newdesign = deleteNA(newdesign)
  # convert to param encoding our model was trained on and can use
  # FIXME: Why do we convert logicals to factor?
  newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE, logicals.as.factor = TRUE)
  y = fn(newdesign, ...)

  # get current best value
  best.index = getMinIndex(y, ties.method = "random")
  best.y = y[best.index]
  best.x = newdesign[best.index, , drop = FALSE]

  list(x = best.x, y = best.y)
}

# Change invalid params, for example stemming from SVM hierarchies.
deleteNA = function(newdesign) {
  for(i in 1:ncol(newdesign)) {
    if(is.numeric(newdesign[, i]))
      newdesign[is.na(newdesign[, i]), i] = -10 - 1
    if(is.factor(newdesign[, i])) {
      newdesign[, i] = addNA(newdesign[, i])
      newdesign[, i] = droplevels(newdesign[, i])
    }
    if(is.logical(newdesign[, i]))
      newdesign[, i] = as.factor(newdesign[, i])
  }
  return(newdesign)
}