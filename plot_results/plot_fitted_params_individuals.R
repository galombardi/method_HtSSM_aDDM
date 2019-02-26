
rm(list=ls())

### PLOT GROUP LEVEL DISTRIBUTION OF PARAMETERS FITTING ######

## package to plot data
pack = "ggplot2"
have = pack %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( pack[!have] ) }
# Now load it
suppressPackageStartupMessages(lapply(pack, require, character.only = TRUE))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# CHANGE THIS WITH THE PATH WHERE YOU SAVE ALL THE SCRIPTS AND THE DATA
pathToFolder <- getwd()

# change this with the name you gave to your results file .RData
resultsFileName <- "results_HtSSM_FIT_dataExample.RData"

# change this with the name of file of your data
dataFileName <- "dataToFit.Rda"

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM")


# load Fit results
load( file.path(main_folder, resultsFileName))
chain=(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))



# load data
load(file.path(main_folder, dataFileName))

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




### prepare empirical data
dataSingleSubj = data.frame(subject = sort( rep(unique(Data$subject), (length(chain[,c("b1.mu")])) ) ))

subj <- unique(Data$subject)
for (s in 1:length(subj)){
    subNum = subj[s]
    dataSingleSubj$wh[dataSingleSubj$subject==subNum] = chain[,c( paste( c("b1.p[",toString(s),"]"), collapse = ""))]
    dataSingleSubj$wt[dataSingleSubj$subject==subNum] = chain[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))]
    dataSingleSubj$time[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("time[",toString(s),"]"), collapse = ""))])
    dataSingleSubj$noise[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
    dataSingleSubj$nDT[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("theta.p[",toString(s),"]"), collapse = ""))])
    dataSingleSubj$bias[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("bias[",toString(s),"]"), collapse = ""))])
}




## plot posterior distribution of the individual parameters

# TIME PARAMETER GROUP LEVEL
# if it's positive the b1 attribute (health) comes later at time t, if it's negative the b2 attribute (taste) comes later at time -t
dev.new(width = 5, height =5)
figTimeSubj <- ggplot() + geom_histogram(data=dataSingleSubj,aes(time),alpha = 0.8,position = "identity",binwidth=0.03,fill="red2",colour="black")+
#coord_cartesian(xlim = c(-1,1), ylim = c(0,6000)) +
ggtitle("Posterior distribution of the inidividual level mean")+
ylab(' ') +
xlab( 'Time health in' ) +
facet_wrap(subject~.)+
theme(text = element_text(family = 'Arial')) +
theme_bw() +
theme(panel.grid = element_blank())
print(figTimeSubj)


# WEIGHT PARAMETERS GROUP LEVEL for IMGS CONDITION
# pink is the health attribute (b1.mu) and red is the taste attribute (b2.mu)
dev.new(width = 5, height =5)
figWeightSubj <- ggplot() + geom_histogram(data=dataSingleSubj,aes(wt),alpha = 0.8,position = "identity",binwidth=0.01,fill="red",colour="black")+
geom_histogram(data=dataSingleSubj, aes(wh),alpha = 0.8,position = "identity",binwidth=0.01,fill="pink",colour="black")+
# coord_cartesian(xlim = c(0.3,1.1), ylim = c(0,6500)) +
ylab(' ') +
xlab( 'Weights' ) +
ggtitle("Posterior distribution of the inidividual level mean")+
theme(text = element_text(family = 'Arial')) +
# annotate(geom="text", x=c(0.97,0.7), y=c(3000,3000), label=c("Health","Taste"),
#           color=c("pink","red") ) +
facet_wrap(subject~.)+
theme_bw() +
theme(panel.grid = element_blank())
print(figWeightSubj)



# NOISE PARAMETER GROUP LEVEL
dev.new(width = 5, height =5)
figNoiseSubj <- ggplot() + geom_histogram(data=dataSingleSubj,aes(noise),alpha = 0.8,position = "identity",binwidth=0.01,fill="red2",colour="black")+
#coord_cartesian(xlim = c(0.25,1), ylim = c(0,8000)) +
ylab(' ') +
xlab( 'Noise' ) +
ggtitle("Posterior distribution of the inidividual level mean")+
facet_wrap(subject~.)+
theme(text = element_text(family = 'Arial')) +
theme_bw() +
theme(panel.grid = element_blank())
print(figNoiseSubj)




# BIAS PARAMETER GROUP LEVEL
dev.new(width = 5, height =5)
figBiasSubj <- ggplot() + geom_histogram(data=dataSingleSubj,aes((bias-0.5)/2),alpha = 0.8,position = "identity",binwidth=0.0025,fill="red2",colour="black")+
#coord_cartesian(xlim = (c(0.4,0.6)-0.5)/2, ylim = c(0,8000)) +
ylab(' ') +
xlab( 'Bias' ) +
ggtitle("Posterior distribution of the inidividual level mean")+
facet_wrap(subject~.)+
theme(text = element_text(family = 'Arial')) +
theme_bw() +
theme(panel.grid = element_blank())
print(figBiasSubj)



# NDT PARAMETER GROUP LEVEL
dev.new(width = 5, height =5)
figNDTSubj <- ggplot() + geom_histogram(data=dataSingleSubj,aes(nDT),alpha = 0.8,position = "identity",binwidth=0.0025,fill="red2",colour="black")+
#coord_cartesian(xlim = (c(0.4,0.6)-0.5)/2, ylim = c(0,8000)) +
ylab(' ') +
xlab( 'Non-decision time' ) +
ggtitle("Posterior distribution of the inidividual level mean")+
facet_wrap(subject~.)+
theme(text = element_text(family = 'Arial')) +
theme_bw() +
theme(panel.grid = element_blank())
print(figNDTSubj)





 
