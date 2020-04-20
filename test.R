rm(list = ls())


#LOAD PACKAGES:
library("osqp")
source("logistic_CDMM.R") #CD MM algorithm
source("optimize_b_MM.R") #MM algorithm

#SET PARAMETERS:
c = 0.7 #true label frequency
pi = 0.5 #true class prior
b = 2 #E(X|Y=1) - E(X|Y=0), larger b, stronger the dependence between X and Y
n = 10000 #sample size

#GENERATE ARTIFICIAL DATASET:
y = rbinom(n,1,pi)
w1 = which(y==1)
w0 = which(y==0)

x = matrix(0,nrow=n,ncol=5)
x[w0,1] = rnorm(length(w0),0,1) 
x[w1,1] = rnorm(length(w1),b,1) 
for(j in 2:5) x[,j]=rnorm(n) #create distrurbing variables

#Generate surrogate variable s:
s = numeric(n)
for (i in 1:n) {
  if (y[i] == 1) {
    s[i] = rbinom(1, 1, c)
  }
}
x = as.matrix(x)

#RUN CD MM method:
obj = logistic_fit_cdmm(x,s,max_iter=100)
hat_c = obj$c
hat_pi = mean(s)/hat_c

#PRINT THE RESULTS:
cat("Label frequency (true):",c,"\n")
cat("Label frequency (estimated):",hat_c,"\n")

cat("Class prior (true):",pi,"\n")
cat("Class prior (estimated):",hat_pi,"\n")
