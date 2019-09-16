rm(list=ls())

%!in%' <- function(x,y)!('%in%'(x,y)) # overload my NotIn function

#--------------------------------
# Prepare packages
want = c("DEoptim", "doParallel", "doMC", "parallel", "foreach","stats4","pracma","runjags","RWiener")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load it
suppressPackageStartupMessages(lapply(want, require, character.only = TRUE))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CHANGE THIS WITH THE PATH WHERE YOU SAVE ALL THE SCRIPTS AND THE DATA
pathToFolder <- getwd()

# DATA FILE NAME
mydata <- "Data_RiskySafe.Rda"
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# load data of lotteries choice task
main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM")
load( file.path(main_folder, mydata))

# load script for simulations with the euler method - boundend accumulation series
source(file.path(main_folder, "aDDMsim_ndt.R"))

# # # Data variables: # # #
#
# GambleChosen  -> =1 if the gamble option is chosen, =0 if the safe option is chosen
# Ev  -> value safe option ()
# Ce -> value option on the right
# RT      -> reaction time in ms
# NumFixations  -> fixation number
# fixationTime  -> fixation duration in seconds
# FixateGamble  -> 1 if the gamble is fixated, 0 if the safe option is fixated
# PercentFixGamble -> proportion of time the gamble is attended
# TotFixTime    -> total fixation time of fixations
# FixationTimeGamble -> total fixation time to the gamble
# trialCount   -> trial number
# SubjectNum -> subject number



load( file.path(main_folder,"results_realFIT_HaDDM_LotteryData_RiskySafe.RData"))
chain= as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))


### prepare data
DataFit <- Data[Data$InitialFixation == 1,] # only first fixation data
subs <- unique(DataFit$SubjectNum)


for (s in 1:length(subs)) {
    
    subject <- subs[s]
    
    DataFit$d[DataFit$SubjectNum==subject] = mean(chain[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))])
    DataFit$theta[DataFit$SubjectNum==subject] = 0.8
    DataFit$noise[DataFit$SubjectNum==subject]  = mean(chain[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
    DataFit$nDT[DataFit$SubjectNum==subject]  = mean(chain[,c( paste( c("theta.p[",toString(s),"]"), collapse = ""))])
    DataFit$bias[DataFit$SubjectNum==subject]  = (mean(chain[,c( paste( c("bias[",toString(s),"]"), collapse = ""))])-0.5)/2
    
    d <- unique(DataFit$d[DataFit$SubjectNum==subject])/1000
    theta <- 0.8
    noise <- unique(DataFit$noise[DataFit$SubjectNum==subject])/sqrt(1000)
    bias <- unique(DataFit$bias[DataFit$SubjectNum==subject])
    paramSim<- c(d, theta,noise,bias)
    
    sub.dir <- toString(subject)
    load(paste(c(main_folder,'/subs/',sub.dir,'/data_preppedGain.Rda'), collapse = ""))
    dataSim <- DataFit[DataFit$SubjectNum == subject,]
    
    # simulate one subject
    simsRec <- aDDM.sim(paramSim, 1, data_mat, dataSim)
    
    DataFit$choice[DataFit$SubjectNum == subject]<- simsRec$choice
    DataFit$RT[DataFit$SubjectNum == subject]<- simsRec$RT
    DataFit$PercentFixGamble[DataFit$SubjectNum == subject]<- simsRec$fix.gamble/simsRec$fix.time
    DataFit$fixationTime[DataFit$SubjectNum == subject]<- simsRec$fix.time
    DataFit$ndt[DataFit$SubjectNum == subject]<- simsRec$ndt
    DataFit$NumFixations[DataFit$SubjectNum == subject]<- simsRec$n.fix
}

DataFit$rtPN[DataFit$choice==1]<- DataFit$RT[DataFit$choice==1]/1000
DataFit$rtPN[DataFit$choice==0]<- -DataFit$RT[DataFit$choice==0]/1000

DataFit<-DataFit[DataFit$RT!=0,]


#--------------------------------#--------------------------------
# prepare data for fitting
idxP = as.numeric(ordered(DataFit$SubjectNum)) #makes a sequentially numbered subj index

ev = (DataFit$Ev)/100
ce = (DataFit$Ce)/100
gazeG = DataFit$PercentFixGamble

y= DataFit$rtPN

N = length(y)

ns = length(unique(idxP))

#--------------------------------------------
# fit the model

dat <- dump.format(list(N=N, y=y, idxP=idxP, ev=ev,ce=ce, gazeG=gazeG, ns=ns))


#alpha.mu1 = as.vector(matrix(1.3 + rnorm(1)*0.2,1,1))
#alpha.mu2 = as.vector(matrix(1.3 + rnorm(1)*0.2,1,1))
#alpha.mu3 = as.vector(matrix(1.3 + rnorm(1)*0.2,1,1))
#alpha.si1 = as.vector(matrix(runif(1)*10, 1,1))
#alpha.si2 = as.vector(matrix(runif(1)*10, 1,1))
#alpha.si3 = as.vector(matrix(runif(1)*10, 1,1))


# initial parameters' values
inits1 <- dump.format(list( alpha.mu=0.01, alpha.pr=0.01,   thetaGaze.mu=0.6, thetaGaze.kappa= 0.1, ndt.mu=0.03,
                            ndt.pr=0.5,   b2.mu=0.01, b2.pr=0.05,  bias.mu=0.5,
                            bias.kappa=0.01, y_pred=y, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))

inits2 <- dump.format(list( alpha.mu=0.02, alpha.pr=0.01, thetaGaze.mu=0.5, thetaGaze.kappa= 0.1, ndt.mu=0.07,
                            ndt.pr=0.5,  b2.mu=0.1, b2.pr=0.05,  bias.mu=0.5,
                            bias.kappa=0.01, y_pred=y,  .RNG.name="base::Wichmann-Hill", .RNG.seed=1234))

inits3 <- dump.format(list( alpha.mu=0.007, alpha.pr=0.01,  thetaGaze.mu=0.4, thetaGaze.kappa= 0.1, ndt.mu=0.015,
                            ndt.pr=0.5,  b2.mu=0.04, b2.pr=0.05,  bias.mu=0.5,
                            bias.kappa=0.01, y_pred=y, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))


# parameters to monitor
monitor = c(
"b.mu", "ndt.mu", "alpha.mu","bias.mu", "thetaGaze.mu","thetaGaze", "b.p","thetaGaze.kappa",  "ndt.pr", "theta.p","alpha.p","bias","deviance" )

# run the model
results <- run.jags(model=file.path(main_folder,"HaDDM_model_RiskySafe.txt"), monitor=monitor, data=dat, n.chains=3, inits=c(inits1,inits2, inits3), plots = TRUE, method="parallel", module="wiener", burnin=50000, sample=10000, thin=10)

suuum<-summary(results)
save(results,DataFit, file=file.path(main_folder,"results_HaDDM_fromRealFittedParams_thetaFixed8.RData"))
write.csv(suuum, file=file.path(main_folder,"results_HaDDM_fromRealFittedParams_thetaFixed8.csv"))

