#' @title Create control object for focus search.
#'
#' @description
#' fooo
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
#' @return [\code{\link{FocusSearchControl}}]
#' @aliases FocusSearchControl
#' @export
makeFocusSearchControl = function(points = 100L, maxit = 1L, restarts = 1L) {
  points = asInt(points)
  maxit = asInt(maxit)
  restarts = asInt(restarts)
  makeS3Obj("FocusSearchControl",
    points = points,
    maxit = maxit,
    restarts = restarts
  )
}

