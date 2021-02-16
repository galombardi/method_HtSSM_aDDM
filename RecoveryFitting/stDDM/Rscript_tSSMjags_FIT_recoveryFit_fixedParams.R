rm(list=ls())

library(runjags)



for (p in 2:36){

load(paste( c("~/HtSSM_expdataFit/recoveryFitting_fixedParams/simsToRecover/DataSims_", toString(p) , ".RData"),collapse="" ))

Data = DataSim
    
    # Data = Data[Data$rt>0.2,]
    
    idx = which(Data$choice==0)
    #Data$rt <- Data$RT
    #Data$rt[idx] = Data$RT[idx] * -1
    
    idxC = rep(1,length(Data$subject))
    idxP = as.numeric(ordered(Data$subject)) #makes a sequentially numbered subj index
    
    hd = (Data$hd)
    td = (Data$td)
    
    rtpos = abs(Data$rt)
    
    y= Data$rt
    
    
    N = length(y)
    
    ns = length(unique(idxP))
    nc = 1
    
    #--------------------------------------------
    # fit the model
    
    dat <- dump.format(list(N=N, y=y, idxP=idxP, hd=hd, td=td, rt=rtpos, ns=ns))
    
    # alpha.mu1 = as.vector(matrix(1 + rnorm(nc)*0.2,nc,1))
    # alpha.mu2 = as.vector(matrix(1.3 + rnorm(nc)*0.2,nc,1))
    # alpha.mu3 = as.vector(matrix(1.3 + rnorm(nc)*0.2,nc,1))
    # alpha.si1 = as.vector(matrix(runif(nc)*10, nc,1))
    # alpha.si2 = as.vector(matrix(runif(nc)*10, nc,1))
    # alpha.si3 = as.vector(matrix(runif(nc)*10, nc,1))
    
    
    inits1 <- dump.format(list( alpha.mu=rep(0.5,nc), alpha.pr=rep(0.05,nc),  time.mu=rep(0.5,nc), time.pr= rep(0.05,nc), theta.mu=rep(0.1,nc),
    theta.pr=rep(0.05, nc),  b1.mu=rep(0.3,nc), b1.pr=rep(0.05,nc), b2.mu=rep(0.01,nc), b2.pr=rep(0.05, nc), bias.mu=rep(0.4,nc),
    bias.kappa=rep(1,nc), y_pred=y,  .RNG.name="base::Super-Duper", .RNG.seed=99999))
    
    
    inits2 <- dump.format(list( alpha.mu=rep(0.1,nc), alpha.pr=rep(0.05,nc),  time.mu=rep(-0.5,nc), time.pr= rep(0.05,nc), theta.mu=rep(0.01,nc),
    theta.pr=rep(0.05, nc),  b1.mu=rep(0.1,nc), b1.pr=rep(0.05,nc), b2.mu=rep(0.2,nc), b2.pr=rep(0.05, nc), bias.mu=rep(0.5,nc),
    bias.kappa=rep(1,nc), y_pred=y,  .RNG.name="base::Wichmann-Hill", .RNG.seed=1234))
    
    inits3 <- dump.format(list( alpha.mu=rep(0.2,nc), alpha.pr=rep(0.05,nc),   time.mu=rep(0,nc), time.pr= rep(0.05,nc), theta.mu=rep(0.15,nc),
    theta.pr=rep(0.05, nc),  b1.mu=rep(0.5,nc), b1.pr=rep(0.05,nc), b2.mu=rep(0.3,nc), b2.pr=rep(0.05, nc),  bias.mu=rep(0.6,nc),
    bias.kappa=rep(1,nc), y_pred=y, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))
    
    monitor = c(
    "b1.mu", "b2.mu", "theta.mu", "alpha.mu","bias.mu", "b1.p", "time.mu","time.pr","time", "b2.p",  "theta.p", "bias","alpha.p", "deviance" )
    
    #results <- run.jags(model="~/Documents/selfContAttributes/tSSM_jags/tssmHT_model.txt", monitor=monitor, data=dat, n.chains=3, inits=c(inits1,inits2, inits3), plots = TRUE, method="parallel", module="wiener", burnin=50000, sample=10000, thin=10)
    
    results <- run.jags(model="HtSSM_expdataFit/HtSSM_fixthr.txt", monitor=monitor, data=dat, n.chains=3, inits=c(inits1,inits2, inits3), plots = TRUE, method="parallel", module="wiener", burnin=50000, sample=10000, thin=10)
    
    
    
    #chains1 = rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]])
    #
    # ## store the estimates in vector variables for easier manipulation (see below)
    # bias.mu1 = chains1[,"bias.mu"]
    # # Do not close this plot window, because we will update this plot (see code below)
    # plot(density(bias.mu1), xlim=c(-1, 1))
    #
    #
    
    #plot(results, layout=c(7,2))
    suuum<-summary(results)
    save(results,params,DataSim, file=paste(c("~/HtSSM_expdataFit/recoveryFitting_fixedParams/results_HtSSM_recoverySim_fixedParams_",toString(p) , ".RData"), collapse = ""))
    write.csv(suuum, file=paste(c("~/HtSSM_expdataFit/recoveryFitting_fixedParams/results_HtSSM_recoverySim_fixedParams_",toString(p) , ".csv"), collapse = ""))
    #}
}
