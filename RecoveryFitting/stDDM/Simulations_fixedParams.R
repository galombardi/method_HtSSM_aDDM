## Fit the tSSM in parallel for multiple subjects
rm(list=ls())

# Install required packages if necessary:
want = c("DEoptim", "Rcpp", "plyr", "parallel", "RcppParallel","stats4","pracma")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load them all
suppressPackageStartupMessages(lapply(want, require, character.only = TRUE))

RcppParallel::setThreadOptions(numThreads = 1) #this is critical for mclapply

### -----
#load the c++ file containing the functions to simulate the delayed DDM
# will give some errors that can be ignored
sourceCpp("~/HtSSM_expdataFit/2ddm_r_cpp_22_noiseCorrected2.cpp")

### ----


###  format choice & RT data ----

load("~/HtSSM_expdataFit/dataToFit.Rda")
data$hds<- scale(data$hd)
data$tds<- scale(data$td)
#Need a dataframe concatenating all subjects' data with the following variables. ----
#The data should be cleaned to remove missed trials or implausibly fast responses < 200 ms

# subject = indicator for each individual subject
# condition = indicator for different conditions if applicable
# choice = [1,0] indicator for yes/no or left/right choices
# rt = reaction times for every choice 
# td = difference in taste ratings between the two options 
# generally, these should be z-scored (compute mean + sd from all ratings in experiment)
# if using the 426-point rating scales, consider binning ratings into 10 or 11 bins to speed up fitting <- zscore after
# hd = difference in healthiness ratings between the two options (same details as above)
# ----



DataFit<-data[data$condition == "Imgs",]
DDataSim<- data.frame(hd = DataFit$hds, td = DataFit$tds, subject = DataFit$SubjectNum )
### Simulations ###

wt = c(  rep(1.2,9),rep(1,9),rep(0.7,9), rep(0.5,9))
wh = c( rep(0.9,9),rep(0.6,9),rep(1.1,9), rep(0.8,9))
noise = 0.3
nDT = 0.3
tHin = rep(c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1),4)
bias = 0


subj <- unique(DDataSim$subject)
countSub=1
for (p in 1:length(wt)){
  params= c(wt[p], wh[p], noise, nDT, tHin[p], bias)
  names(params)= c("wt","wh","noise","nDT","tHin","bias")
  
for (s in subj){  
  DDataSim$wt[DDataSim$subject==s] <- wt
  DDataSim$wh[DDataSim$subject==s] <- wh
  DDataSim$noise[DDataSim$subject==s] <- noise
  DDataSim$nDT[DDataSim$subject==s] <- nDT
  DDataSim$bias[DDataSim$subject==s] <- bias
  
  vd <- DDataSim$td[DDataSim$subject==s]
  hd <- DDataSim$hd[DDataSim$subject==s]
  for (i in 1:length(vd)) {
    DDataSim$rt[DDataSim$subject==s & DDataSim$td==vd[i] & DDataSim$hd==hd[i]] = ddm2_parallel(wt[p],wh[p],noise,nDT,tHin[p],bias,vd[i],hd[i],1)
  }
  countSub=countSub+1
}


### Simulations ###

# parameters to recover with the fitting
DDataSim$rt[DDataSim$rt > 10 | DDataSim$rt < -10]<-NaN
DDataSim$rt[DDataSim$rt < 0.2 & DDataSim$rt > -0.2]<-NaN
DataSim<-DDataSim[!(is.nan(DDataSim$rt)),]


save(params,DataSim, file=paste(c("~/HtSSM_expdataFit/recoveryFitting_fixedParams/simsToRecover/DataSims_",toString(p), ".RData"), collapse = ""))

}

