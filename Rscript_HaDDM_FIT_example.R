rm(list=ls())
pack = "runjags"
have = pack %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( pack[!have] ) }
# Now load it
suppressPackageStartupMessages(lapply(pack, require, character.only = TRUE))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CHANGE THIS WITH THE PATH WHERE YOU SAVE ALL THE SCRIPTS AND THE DATA
pathToFolder <- getwd()

# DATA FILE NAME
mydata <- "et2_data_raw.RData"
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# load example data of a bynary food choice task
main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM")
load( file.path(main_folder, mydata))


#
# # # data variables: # # #
#
# choice  -> =1 if left food item chosen, =0 if right food item chosen
# leftval  -> value option on the left
# rightval -> value option on the right
# rt      -> reaction time in ms
# fixnum  -> fixation number
# fixdur  -> fixation duration in ms
# roi     -> region of interest of the fixation (1 if fixation to the left option, 2 to the right option)
# trial   -> trial number
# subject -> subject number


### prepare data
# subject numbers
subjs<- unique(data$subject)

# calculate total fixation duration, total fixation time to the left option and total fixation time to the right option
for( s in subjs){
    for (t in unique(data$trial[data$subject==s])){
        #index for all fixations in one trial
        ind=which(data$subject==s & data$trial==t)
        #index for left fixations in the trial
        indl=which(data$subject==s & data$trial==t & data$roi==1)
        #index for right fixations in the trial
        indr=which(data$subject==s & data$trial==t & data$roi==2)
        #total fixation duration
        data$totfix[data$subject==s & data$trial==t] = sum(data$fixdur[ind])
        #fixation time to the left option
        data$fixleft[data$subject==s & data$trial==t] = sum(data$fixdur[indl])
        #fixation time to the right option
        data$fixright[data$subject==s & data$trial==t] = sum(data$fixdur[indr])
    }
}

# discard all the fixations, keep the first one
data<-data[data$fixnum==1,]

#non decision time = rt - total fixation time
#data$ndt<- data$rt - data$totfix
# you can decide whether to fit the ndt or give it as input to the model

# NB BEFORE FITTING THE MODEL MAKE SURE YOU HAVE NO NAN or NA IN YOUR DATA

#--------------------------------#--------------------------------

# RT is positive if left food item choosen, negative if right food item chosen
data$rtPN[data$choice==1]<- data$rt[data$choice==1]/1000 # make it in seconds
data$rtPN[data$choice==0]<- -data$rt[data$choice==0]/1000 # make it in seconds

idxP = as.numeric(ordered(data$subject)) #makes a sequentially numbered subj index

# scale the value of the options
v_left = (data$leftval)/5
v_right = (data$rightval)/5

# proportion of fixations to the left option (nb. fixright = 1-gazeL)
gazeL = data$fixleft/data$totfix

# rt to fit
y= data$rtPN

# number of trials
N = length(y)

# number of subjects
ns = length(unique(idxP))


#--------------------------------------------
# fit the model

# data
dat <- dump.format(list(N=N, y=y, idxP=idxP, v_left=v_left,v_right=v_right, gazeL=gazeL, ns=ns))
  
# create random values for the inital values of noise mean and variance
alpha.mu1 = as.vector(matrix(1.3 + rnorm(1)*0.2,1,1))
alpha.mu2 = as.vector(matrix(1.3 + rnorm(1)*0.2,1,1))
alpha.mu3 = as.vector(matrix(1.3 + rnorm(1)*0.2,1,1))
alpha.si1 = as.vector(matrix(runif(1)*10, 1,1))
alpha.si2 = as.vector(matrix(runif(1)*10, 1,1))
alpha.si3 = as.vector(matrix(runif(1)*10, 1,1))



inits1 <- dump.format(list( alpha.mu=alpha.mu1, alpha.pr=alpha.si1,  thetaGaze.mu=0.6, thetaGaze.kappa=1,
                            ndt.mu=0.1, ndt.pr=0.5,  b.mu=0.2, b.pr=0.05, bias.mu=0.5, bias.kappa=1,
                            y_pred=y, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))

inits2 <- dump.format(list( alpha.mu=alpha.mu2, alpha.pr=alpha.si2,  thetaGaze.mu=0.5, thetaGaze.kappa=1,
                            ndt.mu=0.2, ndt.pr=0.5,  b.mu=1, b.pr=0.05, bias.mu=0.6, bias.kappa=1,
                            y_pred=y, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234))

inits3 <- dump.format(list( alpha.mu=alpha.mu3, alpha.pr=alpha.si3,  thetaGaze.mu=0.2, thetaGaze.kappa=1,
                            ndt.mu=0.15, ndt.pr=0.5,  b.mu=1.3, b.pr=0.05, bias.mu=0.4, bias.kappa=1,
                            y_pred=y, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# parameters to monitor
monitor = c(
   "b.mu", "ndt.mu", "alpha.mu","bias.mu", "thetaGaze.mu","thetaGaze", "b.p","thetaGaze.kappa",  "ndt.pr", "theta.p","alpha.p","bias","deviance" )

# run the fitting
 results <- run.jags(model=file.path(main_folder,"addmHT_modelWIENER.txt"), monitor=monitor, data=dat, n.chains=3, inits=c(inits1,inits2, inits3), plots = TRUE, method="parallel", module="wiener", burnin=50000, sample=10000, thin=10)
  

suuum<-summary(results)
save(results,data, file=file.path(main_folder,"results_HaDDM_FIT_dataExample.RData"))
write.csv(suuum, file=file.path(main_folder,"results_HaDDM_FIT_dataExample.csv"))


