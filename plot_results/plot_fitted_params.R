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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM")


# load fitting results
load( file.path(main_folder, resultsFileName))
chain=(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))


## plot of posterior distributions of the GROUP LEVEL mean parameters

# TIME PARAMETER GROUP LEVEL
# if it's positive the b1 attribute (health) comes later at time t, if it's negative the b2 attribute (taste) comes later at time -t
dev.new(width = 5, height =5)
figTime <- ggplot() + geom_histogram(data=chain,aes(time.mu),alpha = 0.8,position = "identity",binwidth=0.03,fill="red2",colour="black")+
#coord_cartesian(xlim = c(-1,1), ylim = c(0,6000)) +
ggtitle("Posterior distribution of the group level mean")+
  ylab(' ') +
  xlab( 'Time health in' ) +
  theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figTime)


# WEIGHT PARAMETERS GROUP LEVEL for IMGS CONDITION
# pink is the health attribute (b1.mu) and red is the taste attribute (b2.mu)
dev.new(width = 5, height =5)
figWeight <- ggplot() + geom_histogram(data=chain,aes(b2.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill="red",colour="black")+
  geom_histogram(data=chain, aes(b1.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill="pink",colour="black")+
# coord_cartesian(xlim = c(0.3,1.1), ylim = c(0,6500)) +
  ylab(' ') +
  xlab( 'Weights' ) +
  ggtitle("Posterior distribution of the group level mean")+
  theme(text = element_text(family = 'Arial')) +
# annotate(geom="text", x=c(0.97,0.7), y=c(3000,3000), label=c("Health","Taste"),
#           color=c("pink","red") ) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figWeight)



# NOISE PARAMETER GROUP LEVEL
dev.new(width = 5, height =5)
figNoise <- ggplot() + geom_histogram(data=chain,aes(alpha.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill="red2",colour="black")+
#coord_cartesian(xlim = c(0.25,1), ylim = c(0,8000)) +
  ylab(' ') +
  xlab( 'Noise' ) +
ggtitle("Posterior distribution of the group level mean")+
  theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figNoise)




# BIAS PARAMETER GROUP LEVEL
dev.new(width = 5, height =5)
figBias <- ggplot() + geom_histogram(data=chainI,aes((bias.mu-0.5)/2),alpha = 0.8,position = "identity",binwidth=0.0025,fill="red2",colour="black")+
#coord_cartesian(xlim = (c(0.4,0.6)-0.5)/2, ylim = c(0,8000)) +
  ylab(' ') +
  xlab( 'Bias' ) +
ggtitle("Posterior distribution of the group level mean")+
  theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figBias)



# NDT PARAMETER GROUP LEVEL
dev.new(width = 5, height =5)
figNDT <- ggplot() + geom_histogram(data=chainI,aes(theta.mu),alpha = 0.8,position = "identity",binwidth=0.0025,fill="red2",colour="black")+
  #coord_cartesian(xlim = (c(0.4,0.6)-0.5)/2, ylim = c(0,8000)) +
  ylab(' ') +
  xlab( 'Bias' ) +
ggtitle("Posterior distribution of the group level mean")+
   theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figNDT)



