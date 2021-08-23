
library(R2jags)
library(tictoc)
library(MCMCvis)
library(tidyverse)
library(lubridate)
library(coda)
library(fishualize)

#load data
xmat.not.sel<- data.matrix(read.csv('fake xmat not sel.csv', as.is = T))
time.not.sel<- read.csv('fake time not sel.csv', as.is = T)$x
xmat.sel<- data.matrix(read.csv('fake xmat sel.csv', as.is = T))
time.sel<- read.csv('fake time sel.csv', as.is = T)$x


#################
### Run model ###
#################

#define the model
model = function(){
  
  #likelihood
    for (i in 1:nobs){
      p1[i] <- time.sel[i] * exp(inprod(xmat.sel[i,], betas))
      p2[i] <- time.not.sel[i] * exp(inprod(xmat.not.sel[i,], betas))
      y[i] ~ dbern(p1[i] / (p1[i] + p2[i]))
    }
    
  #priors
    for (k in 1:ncov){
      betas[k] ~ dnorm(0,0.1)
    }
  }

#set vals for model
nobs<- nrow(xmat.sel)
ncov<- ncol(xmat.sel)
y<- rep(1, nobs)

# MCMC settings 
# ni <- 1500 #number of iterations
# nt <- 10    #interval to thin 
# nb <- 500  #number of iterations to discard as burn-in
# nc <- 3     #number of chains

#set parameters to monitor
params=c("betas")
dat=list(nobs=nobs,
         ncov=ncov,
         xmat.not.sel=xmat.not.sel,
         time.not.sel=time.not.sel,
         xmat.sel=xmat.sel,
         time.sel=time.sel,
         y=y)


#run model
tic()
model1=jags.parallel(model.file=model,
            parameters.to.save=params,
            data=dat,
            n.chains=3,
            n.burnin=1500,
            n.iter=3000,
            n.thin=5,
            jags.seed = 123,
            DIC=TRUE)
toc()
# takes 47 s to run for 3000 iter

model1

MCMCsummary(model1)
MCMCtrace(model1, ind = TRUE, iter = 300, pdf = FALSE)
par(mfrow=c(1,1))
MCMCplot(model1, excl = "deviance")

betas.true=c(-0.5,0,-0.5)
for (i in 1:ncov){
  rango=range(c(betas.true[i], model1$BUGSoutput$sims.list$betas[,i]))
  plot(model1$BUGSoutput$sims.list$betas[,i], type='l', ylim=rango)
  abline(h=betas.true[i], col='red')
  abline(h=model1$BUGSoutput$summary[i,3], col='blue')  #lower confint
  abline(h=model1$BUGSoutput$summary[i,7], col='blue')  #upper confint
}

rango=range(c(betas.true, model1$BUGSoutput$mean$betas))
plot(betas.true, model1$BUGSoutput$mean$betas, xlim=rango, ylim=rango)
lines(rango, rango, col='red')
