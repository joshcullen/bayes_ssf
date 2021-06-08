model{
  for (i in 1:nobs){
    p1[i] <- time.sel[i]    *exp(inprod(xmat.sel[i,],betas))
    p2[i] <- time.not.sel[i]*exp(inprod(xmat.not.sel[i,],betas))
    y[i]~dbern(p1[i]/(p1[i]+p2[i]))
  }
  
  for (k in 1:ncov){
    betas[k] ~ dnorm(0,0.1)
  }
}