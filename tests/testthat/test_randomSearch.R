context("doRandomSearch")

test_that("Random search works", {
  set.seed(123L)
  f = makeSphereFunction(2)
  fn = function(x) apply(x, 1, f)
  ctrl = makeFocusSearchControl(maxit = 5, restarts = 1, points = 100)
  ps = makeParamSet(
    makeNumericParam("x1", lower = 0, upper = 10),
    makeNumericParam("x2", lower = 0, upper = 10)
  )
  doRandomSearch(fn, ps, ctrl) 
  test_list(z, len = 2, types = c("double", "list"))
})
