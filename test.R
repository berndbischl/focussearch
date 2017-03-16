library(ParamHelpers)
library(lhs)
library(smoof)
f = makeSphereFunction(2)
f2 = function(x) apply(x, 1, f)
ctrl = makeFocusSearchControl(maxit = 11, restarts = 1, points = 100)
ps = makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 10),
  makeNumericParam("x2", lower = 0, upper = 10)
)
z = focussearch(f2, ps, ctrl)
print(z)

