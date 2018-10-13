### this is some very unfinished code for stochastic approcimation
### written at useR 2018, after if spoke with vular



library(smoof)
library(ParamHelpers)

approxGrad = function(f, par.set, x, ck, control, op) {
  n = length(x)
  g = 0
  for (j in 1:control$grad.reps) {
    d = rbinom(n, size = 1, prob = 0.5) * 2 - 1 
    d = ck * d
    x1 = x - d
    x2 = x + d
    f1 = f(x1)
    f2 = f(x2)
    addOptPathEl(op, x = list(x = x1), y = f1, extra = list(ck = ck, stepsize = NA))
    addOptPathEl(op, x = list(x = x2), y = f2, extra = list(ck = ck, stepsize = NA))
    g = g + (f2 - f1) / (2 * ck * d)
  }
  g = g / control$grad.reps
}

spsa = function(f, par.set, x0, control) {
  x = x0
  op = makeOptPathDF(par.set, y.names = "y", minimize = TRUE, include.extra = TRUE)
  for (k in 1:control$maxit) {
    stepsize = control$stepsize.init / (control$stepsize.stab + k)^control$stepsize.alpha
    ck = control$c0 / (k^control$grad.gamma)
    g = approxGrad(f, par.set, x, ck, control, op)
    x = x - stepsize * g  
    y = f(x)
    ee = list(ck = ck, stepsize = stepsize)
    addOptPathEl(op, x = list(x = x), y = y, extra = ee)
  }
  list(x = x, y = f(x), opt.path = op)
}


f = makeSphereFunction(2)
control = list(maxit = 30L, grad.reps = 1L, 
  stepsize.init = 1, stepsize.stab = 1, stepsize.alpha = 1, 
  c0 = 1, grad.gamma = 0.5)
ps = makeParamSet(
  makeNumericVectorParam("x", len = 2L)
)
z = spsa(f, ps, c(3,7), control)
print(z)
opdf = as.data.frame(z$opt.path)
