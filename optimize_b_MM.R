logLike = function(par,x,y,c){
  beta0 = par[1]
  beta1 = par[-1]
  res = sum(  y*log(c*sigma(x%*%beta1+beta0))+(1-y)*log(1-c*sigma(x%*%beta1+beta0))   )
  return(res)
}
grad_logLike = function(par,x,y,c){
  beta0 = par[1]
  beta1 = par[-1]
  
  var1 = sigma(x%*%beta1+beta0)*(1-sigma(x%*%beta1+beta0))
  sigma1 = sigma(x%*%beta1+beta0)
  
  a= var1*( (y-c*sigma1)/(sigma1*(1-c*sigma1)) )
  
  res = t(cbind(1,x))%*%a
  return(res)
}


optimize_b_MM = function(x,y,c,max_iter=200){
  x1 = cbind(1,x)
  p=ncol(x1)
  par_old = rep(0,p)
  for(iter in 1:max_iter){
    P = (0.25)*t(x1)%*%x1
    q = -grad_logLike(par_old,x,y,c)
    solve1 = solve_osqp(P=P, q=q,pars = osqpSettings(verbose=FALSE))
    par_new = solve1$x + par_old
    if(is.na(sum(par_new))) break
    if(max(abs(par_new-par_old))<0.001) break
    par_old = par_new
  }
  par = par_old
  return(par)
  
}

