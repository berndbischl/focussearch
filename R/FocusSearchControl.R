#' @title Create control object for focus search.
#'
#' @description
#' Creates a control object to be used with (`focussearch`). 
#' See __Arguments__ for further details.
#'
#' @param points [\code{integer(1)}]\cr
#'   Number of points for each random search.
#'   Default is 100.
#' @param maxit [\code{integer(1)}]\cr
#'   Number of focus interation where feasible region is shrinked.
#'   Default is 100.
#' @param restarts [\code{integer(1)}]\cr
#'   Number of independent restarts.
#'   Default is 1.
#'   @param exploit (`numeric`) \¢r
#'   Fraction of points to exploit in the local param space vs. in the global param space.
#'   Defaults to 1 (full exploitation).
#' @return [\code{\link{FocusSearchControl}}]
#' @aliases FocusSearchControl
#' @export
makeFocusSearchControl = function(points = 100L, maxit = 1L, restarts = 1L, exploit = 1) {
  points = asInt(points)
  maxit = asInt(maxit)
  restarts = asInt(restarts)
  assertNumber(exploit, lower = 0, upper = 1)
  makeS3Obj("FocusSearchControl",
    points = points,
    maxit = maxit,
    restarts = restarts,
    exploit = exploit
  )
}


# Helper to quickly set points in the control
# @template arg_control
# @param points.frac (`numeric`) \cr
#   Fraction of point to keep in the control.
setControlPoints = function(ctrl, points.frac) {
  ctrl$points = ceiling(points.frac * ctrl$points)
  return(ctrl)
}

