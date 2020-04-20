##################

sigma = function(s) {
  res = exp(s) / (1 + exp(s))
  return(res)
}

logLike_c = function(par,x,y,c){
  beta0 = par[1]
  beta1 = par[-1]
  res = sum(  y*log(c*sigma(x%*%beta1+beta0))+(1-y)*log(1-c*sigma(x%*%beta1+beta0))   )
  return(res)
}
gr_c = function(par,x,y,c){
  beta0 = par[1]
  beta1 = par[-1]
  
  var1 = sigma(x%*%beta1+beta0)*(1-sigma(x%*%beta1+beta0))
  sigma1 = sigma(x%*%beta1+beta0)
  
  a= var1*( (y-c*sigma1)/(sigma1*(1-c*sigma1)) )
  
  res = t(cbind(1,x))%*%a
  return(res)
}

logistic_fit_c = function(x,y,c){
  
  optim1 = optim(par=rep(0,ncol(x)+1),fn=logLike_c,gr=gr_c,x=x,y=y,c=c,method="BFGS",control=list(fnscale=-1))
  par = optim1$par
  return(list(par=par))
}


logLike_beta = function(par,x,y,beta1,beta0){
  res = sum(  y*log(par*sigma(x%*%beta1+beta0))+(1-y)*log(1-par*sigma(x%*%beta1+beta0))   )
  return(res)
}
gr_beta = function(par,x,y,beta1,beta0){
  
  sigma1 = sigma(x%*%beta1+beta0)
  res = sum(y/par - (1-y)*sigma1/(1-par*sigma1)) #?????
  return(res)
}

logistic_fit_beta = function(x,y,beta1,beta0){
  
  optim1 = optim(par=0.5,fn=logLike_beta,gr=gr_beta,x=x,y=y,beta1=beta1,beta0=beta0,method="BFGS",control=list(fnscale=-1))
  par = optim1$par
  return(list(par=par))
}


logistic_fit_cdmm = function(x,s,max_iter=100){
  
  p = ncol(x)+1    
  n = nrow(x)
  b_old = numeric(p)  
  c_old = 0.5
  x1 = cbind(1,x)
  
  for(iter in 1:max_iter){
    #1 Compute c using b_old:
    c_new=  logistic_fit_beta(x,s,beta1=b_old[-1],beta0=b_old[1])$par
    
    if(c_new>1) c_new=0.99
    if(c_new<0) c_new=0.01
    
    #2 Compute b using c (using MM algorithm):
    b_new = optimize_b_MM(x,s,c_new,max_iter=50)
    if(max(abs(c_new-c_old))<0.001) break
    
    c_old = c_new
    b_old = b_new
    
  }
  res = list(c=c_old,beta=b_old)
  return(res)
  
}




