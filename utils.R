get.X = function(obs, pred){
  
  X = sum((obs-pred)**2/pred)
  return(X)
  
}

prep.for.X = function(pis, ns, obs){

  ppr = pis*ns
  npr = (1-pis)*ns
  nobs = ns-obs 
  pred = c(ppr, npr)
  obss = c(obs, nobs)
  return(list('pr'=pred, 'ob'=obss))
}

get.disp = function(pis, ns, obs, p){
  
  m = prep.for.X(pis, ns, obs)
  X.2 = get.X(m$ob, m$pr)
  print("chi-sq")
  print(X.2)
  phi = X.2/(length(ns)-p)
  print("disp")
  print(phi)
  return(phi)
}

res.plot = function(fit){
  
  plot(residuals(fit, type = "pearson") ~ fit$linear.predictors, 
       xlab='eta (linear predictor)', ylab='Pearson Res')
  abline(h = 0, lty = 2)
}

res.plot.resp = function(fit){
  
  plot(residuals(fit, type = "response") ~ fit$linear.predictors, 
       xlab='eta (linear predictor)', ylab='Raw Res')
  abline(h = 0, lty = 2)
}