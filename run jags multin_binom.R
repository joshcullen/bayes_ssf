rm(list=ls(all=TRUE)) 
set.seed(121)
library(jagsUI)

#get data
setwd('U:\\GIT_models\\git_ssf1\\fake data')
xmat.not.sel=data.matrix(read.csv('fake xmat not sel.csv'))
time.not.sel=read.csv('fake time not sel.csv')$x
xmat.sel=data.matrix(read.csv('fake xmat sel.csv'))
time.sel=read.csv('fake time sel.csv')$x
nobs=nrow(xmat.sel)
ncov=ncol(xmat.not.sel)
y=rep(1,nobs)

# MCMC settings 
ni <- 1500 #number of iterations
nt <- 10    #interval to thin 
nb <- 500  #number of iterations to discard as burn-in
nc <- 3     #number of chains

#set parameters to monitor
params=c("betas")
dat=list(nobs=nobs,ncov=ncov,
         xmat.not.sel=xmat.not.sel,
         time.not.sel=time.not.sel,
         xmat.sel=xmat.sel,
         time.sel=time.sel,y=y)

#run model
setwd('U:\\GIT_models\\git_ssf1')
model1=jags(model.file="jags_multin_binom.R",
               parameters.to.save=params,
               data=dat,
               n.chains=nc,
               n.burnin=nb,n.iter=ni,n.thin=nt,DIC=TRUE)
summary(mod)
model1

betas.true=c(-0.5,0,-0.5)
for (i in 1:ncov){
  rango=range(c(betas.true[i],model1$sims.list$betas[,i]))
  plot(model1$sims.list$betas[,i],type='l',ylim=rango)
  abline(h=betas.true[i],col='red')
}

rango=range(c(betas.true,model1$mean$betas))
plot(betas.true,model1$mean$betas,xlim=rango,ylim=rango)
lines(rango,rango,col='red')