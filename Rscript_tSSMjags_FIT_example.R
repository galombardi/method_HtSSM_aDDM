rm(list=ls())
pack = "runjags"
have = pack %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( pack[!have] ) }
library(runjags)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# CHANGE THIS WITH THE PATH WHERE YOU SAVE ALL THE SCRIPTS AND THE DATA
pathToFolder <- getwd()

# DATA FILE NAME 
mydata <- "dataFit.Rda"
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# load example data of a bynary food choice task
main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM")
load( file.path(main_folder, mydata))


Data = dataFit
#
# # # Data variables: # # #
#
# choice  -> =1 if left food item chosen, =0 if right food item chosen
# vt_l    -> value taste food item on the left hand side of the screen
# vt_r    -> value taste food item on the right hand side of the screen
# vh_l    -> value health food item on the left hand side of the screen
# vh_r    -> value health food item on the right hand side of the screen
# td      -> value difference in taste
# hd      -> value difference in health
# rt      -> reaction time in seconds
# subject -> subject number



    # Data = Data[Data$rt>0.2,]

    # RT is positive if left food item choosen, negative if right food item chosen
    idx = which(Data$choice==0)
    Data$RT <- Data$rt
    Data$RT[idx] = Data$rt[idx] * -1
    
    idxP = as.numeric(ordered(Data$subject)) #makes a sequentially numbered subj index

    # scale value of health and taste difference
    hd = scale(Data$hd)[,1]
    td = scale(Data$td)[,1]

    rtpos = Data$rt
    
    y= Data$RT
    N = length(y)
    ns = length(unique(idxP))

    #--------------------------------------------
    # fit the model

    # data
    dat <- dump.format(list(N=N, y=y, idxP=idxP, hd=hd, td=td, rt=rtpos, ns=ns))

    # initial values for the parameters, it could also be created randomly.. make sure that the theta.mu initial value is smaller than the rt in each trial
    inits1 <- dump.format(list( alpha.mu=0.5, alpha.pr=0.05,  time.mu=0.5, time.pr= 0.05, theta.mu=0.1,
                                theta.pr=0.05,  b1.mu=0.3, b1.pr=0.05, b2.mu=0.01, b2.pr=0.05, bias.mu=0.4,
                                bias.kappa=1, y_pred=y,  .RNG.name="base::Super-Duper", .RNG.seed=99999))
    
    inits2 <- dump.format(list( alpha.mu=0.1, alpha.pr=0.05,  time.mu=-0.5, time.pr= 0.05, theta.mu=0.2,
                                theta.pr=0.05, b1.mu=0.3, b1.pr=0.05, b2.mu=0.1, b2.pr=0.05, bias.mu=0.5,
                                bias.kappa=1, y_pred=y,  .RNG.name="base::Wichmann-Hill", .RNG.seed=1234))
    
    inits3 <- dump.format(list( alpha.mu=0.2, alpha.pr=0.05, time.mu=0, time.pr= 0.05, theta.mu=0.15,
                                theta.pr=0.05,  b1.mu=0.5, b1.pr=0.05, b2.mu=0.3, b2.pr=0.05, bias.mu=0.6,
                                bias.kappa=1, y_pred=y, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

    # parameters to monitor
    monitor = c(
    "b1.mu", "b2.mu", "theta.mu", "alpha.mu","bias.mu", "b1.p", "time.mu","time.pr","time", "b2.p",  "theta.p", "bias","alpha.p", "deviance" )
    
    # run the fitting
    results <- run.jags(model=file.path(main_folder,"tssmHT_modelWIENER.txt"), monitor=monitor, data=dat, n.chains=3, inits=c(inits1,inits2, inits3), plots = TRUE, method="parallel", module="wiener", burnin=50000, sample=10000, thin=10)
    
    suuum<-summary(results)
    save(results,Data, file=file.path(main_folder,"results_HtSSM_FIT_dataExample.RData"))
    write.csv(suuum, file=file.path(main_folder,"results_HtSSM_FIT_dataExample.csv"))


