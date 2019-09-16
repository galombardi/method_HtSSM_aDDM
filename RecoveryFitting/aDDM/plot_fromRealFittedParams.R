rm(list=ls())

### PLOT GROUP LEVEL DISTRIBUTION OF PARAMETERS FITTING ######

## package to plot data
pack = c("ggplot2","ggridges","lmerTest")
have = pack %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load it
suppressPackageStartupMessages(lapply(pack, require, character.only = TRUE))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CHANGE THIS WITH THE PATH WHERE YOU SAVE ALL THE SCRIPTS AND THE DATA
pathToFolder <- getwd()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM/c_RecoveryParams_fromRealFittedParams")
folder_plots<- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM/plots")



filelist = list.files(path = main_folder, pattern = ".RData")
filelist = file.path(main_folder, filelist)
meanParam <- data.frame()
meanParamSub <- data.frame()
chain<-data.frame()
for (p in 1:length(filelist)){
  load(filelist[p])
  chain1=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))
  #chain1$realD <- params[1]*1000
  chain1$realT <- unique(DataFit$theta)
  #chain1$realN <- params[3]
  #meanParam1<- data.frame(realD = params[1]*1000, realT = params[2], realN = params[3], thetaEst = mean(chain1[,c("thetaGaze.mu")]), dEst = mean(chain1[,c("b2.mu")]), noiseEst = mean(chain1[,c("alpha.mu")]))
  #meanParamSub1<- data.frame(subject = 1:30)
  #meanParamSub1$realD <- params[1]*1000
  #meanParamSub1$realT <- params[2]
  #meanParamSub1$realN <- params[3]
  #for (s in 1:30){
  #  meanParamSub1$thetaEst[meanParamSub1$subject==s] = mean(chain1[,c( paste( c("thetaGaze[",toString(s),"]"), collapse = ""))])
  #  meanParamSub1$dEst[meanParamSub1$subject==s] = mean(chain1[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))])
  # meanParamSub1$noiseEst[meanParamSub1$subject==s] = mean(chain1[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
  #}
  #meanParam<- rbind(meanParam,meanParam1)
  #meanParamSub<- rbind(meanParamSub,meanParamSub1)
  chain<-rbind(chain,chain1)
}

#chain11= chain[chain$realT %in% c(0.2,0.4,0.6,0.8),]
#meanParam11 = meanParam[meanParam$realT %in% c(0.2,0.4,0.6,0.8),]
#meanParamSub11 = meanParamSub[meanParamSub$realT %in% c(0.2,0.4,0.6,0.8),]


#chain$realD2= factor(chain$realD, labels = c("delta==6", "delta==10","delta==14"))
chain$realT2 = factor(chain$realT, labels = c("0.2","0.4","0.8"))
# basic example
png( file.path(folder_plots, "aDDMtheta_FromReal.png"), width = 4*300, height = 3*300,res = 300)
#dev.new(width = 8, height = 4)
ggplot(chain, aes(x = thetaGaze.mu, y=realT2, fill = realT2)) +
  geom_density_ridges() +
  scale_x_continuous(breaks=c(0.17,0.4,0.8), labels=c("0.2","0.4","0.8")) +
  xlab(expression( theta ))+  ylab( expression("Posterior distribution of the mean") ) +
  #ggtitle("Group Level")+
  geom_vline(xintercept = 0.17 ,linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.3 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 0.4 ,linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.5 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 0.8 , linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.7 , linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.8 , linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  scale_fill_brewer(name=expression("Generating" ~theta),palette="Blues")+
  theme_ridges() + 
  theme_bw() +
  #facet_grid(realD2~realN2, labeller = label_parsed)+
  #theme( plot.title = element_text(hjust = 0.5))+
  #theme(legend.position = "none")+
  #theme_classic()+
  theme(text = element_text(family = 'Arial'),
        axis.text.y=element_blank(),
        strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
  theme(panel.grid = element_blank())
dev.off()



chain$realT2= factor(chain$realT, labels = c("theta==0.2","theta==0.4",
                                             "theta==0.6", "theta==0.8"))
# basic example
png( file.path(folder_plots, "aDDMdrift.png"), width = 6*300, height = 5.3*300,res = 300)
ggplot(chain, aes(x = (b2.mu), y=factor(realD), fill = factor(realD))) +
  geom_density_ridges() +
  #xlim(c(0.0005,0.02))
  scale_x_continuous(breaks=c(6,10,14))+
  xlab(expression( delta ))+  ylab( expression("Posterior distribution of the mean") ) +
  geom_vline(xintercept = 6 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 10 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 14 ,linetype = 'dashed',colour = 'dark gray')+
  #ggtitle("Group Level")+
  labs(fill=" ")+
  facet_grid(realT2~realN2, labeller = label_parsed)+
  scale_fill_brewer(name=expression("Generating" ~delta),palette="Reds")+
  theme_ridges() +
  theme_bw() +
  #theme( plot.title = element_text(hjust = 0.5))+
  #theme(legend.position = "none")+
  #theme_classic()+
  theme(text = element_text(family = 'Arial'),
        axis.text.y=element_blank(),
        strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
  theme(panel.grid = element_blank())
dev.off()


chain$realN3 = factor(chain$realN, labels = c("0.3", "0.45", "0.6"))
png( file.path(folder_plots, "aDDMsigma.png"), width = 6*300, height = 5.3*300,res = 300)
ggplot(chain, aes(x = alpha.mu, y=(realN3), fill = (realN3))) +
  geom_density_ridges() +
  scale_x_continuous(breaks=sort(unique(meanParam$realN))*sqrt(1000), label = c("0.3", "0.45", "0.6")) +
  scale_y_discrete( label = c("0.3", "0.45", "0.6")) +
  xlab(expression( sigma ))+  ylab( expression("Posterior distribution of the mean") ) +
  #ggtitle("Group Level")+
  geom_vline(xintercept = 0.3004164 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 0.4427189 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 0.6324555 ,linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  scale_fill_brewer(name=expression("Generating" ~sigma),palette="Greens")+
  theme_ridges() + 
  theme_bw() +
  facet_grid(realT2~realD2, labeller = label_parsed)+
  #theme( plot.title = element_text(hjust = 0.5))+
  #theme(legend.position = "none")+
  #theme_classic()+
  theme(text = element_text(family = 'Arial'),
        axis.text.y=element_blank(),
        strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
  theme(panel.grid = element_blank())
dev.off()
