doRandomSearch = function(f, par.set, control, ...) {
  # predict on design where NAs were imputed, but return proposed points with NAs
  newdesign = generateDesign(control$points, par.set, randomLHS)

  # convert to param encoding our model was trained on and can use
  newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE, logicals.as.factor = TRUE)
  y = f(newdesign, ...)

  # get current best value
  best.index = getMinIndex(y, ties.method = "random")
  best.y = y[best.index]
  best.x = newdesign[best.index, , drop = FALSE]

  list(x = best.x, y = best.y)
}

