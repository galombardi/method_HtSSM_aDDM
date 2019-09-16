# Recovery fitting for the HaDDM

## model and data
HaDDM_model_RiskySafe.txt  --> hierarchical bayesian aDDM with the piece-wise constant approximation method. The model has been written for a lottery task in which subjects can choose between a gamble and a safe option in each trials.

aDDMsim_ndt.R --> aDDM with the euler method (bounded accumulation series) for the lottery task

Data_RiskySafe.Rda --> lottery data from real experiment

 Data variables:
 GambleChosen  -> =1 if the gamble option is chosen, =0 if the safe option is chosen
 Ev  -> value safe option ()
 Ce -> value option on the right
 RT      -> reaction time in ms
 NumFixations  -> fixation number
 fixationTime  -> fixation duration in seconds
 FixateGamble  -> 1 if the gamble is fixated, 0 if the safe option is fixated
 PercentFixGamble -> proportion of time the gamble is attended
 TotFixTime    -> total fixation time of fixations
 FixationTimeGamble -> total fixation time to the gamble
 trialCount   -> trial number
 SubjectNum -> subject number


## a) Recovery of homogeneuos parameters
the scripts a_Rscript_HaDDM_RecoveryFit_homogeneousParams_*.R perform simulations of the aDDM with the euler method (bounded accumulation series) with different fixed parameters that are constant across subjects. After simulating the data, a fitting procedure is performed with the HaDDM on the simulated data.

a_Rscript_HaDDM_RecoveryFit_homogeneousParams_noise30.R -> perform simulations and recovery fitting with theta in {0.2,0.4,0.6,0.8}, drift constant in {6,10,14} and the standard deviation of the noise = 0.30

a_Rscript_HaDDM_RecoveryFit_homogeneousParams_noise45.R -> perform simulations and recovery fitting with theta in {0.2,0.4,0.6,0.8}, drift constant in {6,10,14} and the standard deviation of the noise = 0.45

a_Rscript_HaDDM_RecoveryFit_homogeneousParams_noise63.R -> perform simulations and recovery fitting with theta in {0.2,0.4,0.6,0.8}, drift constant in {6,10,14} and the standard deviation of the noise = 0.63

## b) Recovery of heterogeneuos parameters
the scripts b_Rscript_HaDDM_RecoveryFit_ heterogeneousParams.R perform simulations of the aDDM with the euler method (bounded accumulation series) with different parameters that are drawn from gaussian distributions separately for each subject. After simulating the data, a fitting procedure is performed with the HaDDM on the simulated data.

MEAN of the PARAMETERS to recover
d <- 12
theta <- 0.35
noise <- 0.41
bias<-0

GAUSSIAN DISTRIBUTIONS of the parameters
noiseDist<- rnorm(20000,noise,0.0008)
biasDist<- rnorm(20000,bias,0.01)
thetaDist<- rnorm(20000,theta,0.05)
dDist<- rnorm(20000,d,0.0005)

## c) Recovery from real fitted parameters parameters
After fitting the model to real data - results_realFIT_HaDDM_LotteryData_RiskySafe.RData , the scripts c_Rscript_HaDDM_RecoveryFit_fromRealFittedParams*.R perform simulations of the aDDM with the euler method (bounded accumulation series) with the mean at the individual level for each parameter. After simulating the data, a fitting procedure is performed with the HaDDM on the simulated data.

c_Rscript_HaDDM_RecoveryFit_fromRealFittedParams.R -> perform simulations and recovery fitting with all the real parameter fitted. For each subject the mean at the individual level for each parameter is used.

c_Rscript_HaDDM_RecoveryFit_fromRealFittedParams_FixedTheta4.R -> perform simulations and recovery fitting with all the real parameter fitted and a fixed theta=0.4. For each subject the mean at the individual level for each parameter is used and the same fixed theta=0.4 for all subjects.

c_Rscript_HaDDM_RecoveryFit_fromRealFittedParams_FixedTheta8.R -> perform simulations and recovery fitting with all the real parameter fitted and a fixed theta=0.8. For each subject the mean at the individual level for each parameter is used and the same fixed theta=0.8 for all subjects.

c_Rscript_HaDDM_RecoveryFit_fromRealFittedParams_FixedThetaMean.R -> perform simulations and recovery fitting with all the real parameter fitted and a fixed theta which is equal to the mean population parameter for all the subjects. For each subject the mean at the individual level for each parameter is used and the same fixed theta=mean group-level parameter equal for all subjects.




## Before running

install jags_4.3.0.orig.tar.gz from library https://sourceforge.net/projects/mcmc-jags/ then run  
`tar --bzip2 -xf ~/jags_4.3.0.orig.tar.gz`  
`cd JAGS_4.3.0/`  
`./configure`  
`make -j 8`  
`sudo make install`  

install JAGS-WIENER-MODULE-1.1.tar.gz from library https://sourceforge.net/projects/mcmc-jags/ then run  
`tar --bzip2 -xf ~/JAGS-WIENER-MODULE-1.1.tar.gz`  
`cd JAGS-WIENER-MODULE-1.1/`  
`./configure`  
`make -j 8`  
`sudo make install`  


## How to run the code
First change the path pathToFolder in the a_Rscript_HaDDM_RecoveryFit_homogeneousParams_noise30.R with the path in which you saved all the scripts, for example:
`pathToFolder <- "~/pathToFolder"`  
then run   
`source('~/method_HtSSM_aDDM/RecoveryFitting/aDDM/a_Rscript_HaDDM_RecoveryFit_homogeneousParams_noise30.R')`


## NOTES
Make sure that the reaction time data you want to fit does not have missing values or NAs before running.





