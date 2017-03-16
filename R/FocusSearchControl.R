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

