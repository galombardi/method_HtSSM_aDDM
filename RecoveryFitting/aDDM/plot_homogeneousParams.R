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

main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM/a_RecoveryParams_homogeneousParams")
folder_plots<- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM/plots")


filelist = list.files(path = main_folder, pattern = ".RData")
filelist = file.path(main_folder, filelist)
meanParam <- data.frame()
meanParamSub <- data.frame()
chain<-data.frame()
for (p in 1:length(filelist)){
  load(filelist[p])
  chain1=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))
  chain1$realD <- params[1]*1000
  chain1$realT <- params[2]
  chain1$realN <- params[3]
  meanParam1<- data.frame(realD = params[1]*1000, realT = params[2], realN = params[3], thetaEst = mean(chain1[,c("thetaGaze.mu")]), dEst = mean(chain1[,c("b2.mu")]), noiseEst = mean(chain1[,c("alpha.mu")]))
  meanParamSub1<- data.frame(subject = 1:30)
  meanParamSub1$realD <- params[1]*1000
  meanParamSub1$realT <- params[2]
  meanParamSub1$realN <- params[3]
  for (s in 1:30){
    meanParamSub1$thetaEst[meanParamSub1$subject==s] = mean(chain1[,c( paste( c("thetaGaze[",toString(s),"]"), collapse = ""))])
    meanParamSub1$dEst[meanParamSub1$subject==s] = mean(chain1[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))])
    meanParamSub1$noiseEst[meanParamSub1$subject==s] = mean(chain1[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
  }
  meanParam<- rbind(meanParam,meanParam1)
  meanParamSub<- rbind(meanParamSub,meanParamSub1)
  chain<-rbind(chain,chain1)
}

#chain11= chain[chain$realT %in% c(0.2,0.4,0.6,0.8),]
#meanParam11 = meanParam[meanParam$realT %in% c(0.2,0.4,0.6,0.8),]
#meanParamSub11 = meanParamSub[meanParamSub$realT %in% c(0.2,0.4,0.6,0.8),]


chain$realD2= factor(chain$realD, labels = c("delta==6", "delta==10","delta==14"))
chain$realN2 = factor(chain$realN, labels = c("sigma==0.3", "sigma==0.45", "sigma==0.6"))
# basic example
png( file.path(folder_plots, "aDDMtheta.png"), width = 6*300, height = 4.8*300,res = 300)
#dev.new(width = 8, height = 4)
ggplot(chain, aes(x = thetaGaze.mu, y=factor(realT), fill = factor(realT))) +
  geom_density_ridges() +
  scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8)) +
  xlab(expression( theta ))+  ylab( expression("Posterior distribution of the mean") ) +
  #ggtitle("Group Level")+
  geom_vline(xintercept = 0.2 ,linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.3 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 0.4 ,linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.5 ,linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 0.6 , linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.7 , linetype = 'dashed',colour = 'dark gray')+
  geom_vline(xintercept = 0.8 , linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  scale_fill_brewer(name=expression("Generating" ~theta),palette="Blues")+
  theme_ridges() + 
  theme_bw() +
  facet_grid(realD2~realN2, labeller = label_parsed)+
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


chainD<-data.frame()
for (t in unique(chain$realT)){
  for(n in unique(chain$realN)){
    vectReal= sort(unique(chain$realD[chain$realT==t & chain$realN==n]))*1000
    for (d in 1:(length(vectReal)-1)){
      chain1=chain[chain$realT==t & chain$realN==n & chain$realD*1000==vectReal[d],]
      chain1$diff= (chain$b2.mu[chain$realT==t & chain$realN==n & chain$realD*1000==vectReal[d+1]] - chain$b2.mu[chain$realT==t & chain$realN==n & chain$realD*1000==vectReal[d]])
      chain1$realDiff = vectReal[d]
      chainD = rbind(chainD,chain1)
    }
  }
}

chainD$realDiff2= factor(chainD$realDiff, labels = c("10-6","14-10"))
lines=unique(chainD$realDiff2)
png( file.path(folder_plots, "differenceD.png"), width = 6*300, height = 5.3*300,res = 300)
ggplot(chainD, aes(x = diff, y=factor(realDiff2), fill = factor(realDiff2))) +
  geom_density_ridges() +
  scale_x_continuous(breaks=c(0,4,8), labels = c("0","4","8"),limits=c(-1,9)) +
  xlab(expression( "Difference in" ~delta) )+  ylab( expression( "Posterior distribution of difference in mean") )+
  #ggtitle("Group Level")+
  geom_hline(yintercept = as.numeric(lines[1]), colour = 'black')+
  geom_hline(yintercept = as.numeric(lines[2]), colour = 'black')+
  geom_density_ridges() +
  geom_vline(xintercept = 4 ,linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.4427189 ,linetype = 'dashed',colour = 'dark gray')+
  #geom_vline(xintercept = 0.6324555 ,linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  scale_fill_brewer(name=expression("Generating \n difference in" ~delta),palette="YlOrBr")+
  theme_ridges() +
  theme_bw() +
  facet_grid(realT2~realN2, labeller = label_parsed)+
  #theme( plot.title = element_text(hjust = 0.5))+
  #theme(legend.position = "none")+
  #theme_classic()+
  theme(text = element_text(family = 'Arial'),
        axis.text.y=element_blank(),
        strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
  theme(panel.grid = element_blank())
dev.off()




DataFit$subject<- as.numeric(ordered(DataFit$SubjectNum))
chaintemp<-chain[chain$realT==0.2 & chain$realD==10 & chain$realN==0.0140,]
### prepare empirical data
dataSingleSubj = data.frame(subject = sort( rep(unique(DataFit$subject), (length(chaintemp[,c("b2.mu")])) )) )

subj <- unique(DataFit$subject)
for (s in 1:length(subj)){
  subNum = subj[s]
  dataSingleSubj$drift[dataSingleSubj$subject==subNum] = chaintemp[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))]
  dataSingleSubj$theta[dataSingleSubj$subject==subNum] = (chaintemp[,c( paste( c("thetaGaze[",toString(s),"]"), collapse = ""))])
  dataSingleSubj$noise[dataSingleSubj$subject==subNum] = (chaintemp[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
  dataSingleSubj$nDT[dataSingleSubj$subject==subNum] = (chaintemp[,c( paste( c("theta.p[",toString(s),"]"), collapse = ""))])
  dataSingleSubj$bias[dataSingleSubj$subject==subNum] = (chaintemp[,c( paste( c("bias[",toString(s),"]"), collapse = ""))])
}


ggplot() +
  geom_density(data=dataSingleSubj, aes(x = theta), color="blue2",fill="blue2", alpha=0.5, linetype="dashed") +
  scale_x_continuous(breaks=c(0,0.3,0.6), labels=c("0","0.3","0.6"),limits= c(0,0.7)) +
  scale_y_continuous(breaks=c(0,20), labels=c("",""),limits= c(0,20)) +
  #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
  #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
  xlab(expression( "Posterior distribution of the mean" ~theta )) +  ylab(expression("Real" ~ theta )) +
  #ggtitle("Group Level")+
  geom_vline(xintercept = 0.2 ,linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  #scale_fill_brewer(palette="Blues")+
  #theme_ridges() + 
  facet_wrap(subject~.)+
  theme_bw() +
  theme( plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(text = element_text(family = 'Arial',size=20)) +
  theme(panel.grid = element_blank())




ggplot() +
  geom_density(data=dataSingleSubj, aes(x = drift), color="red2",fill="red2", alpha=0.5, linetype="dashed") +
  #scale_x_continuous(breaks=c(0,0.3,0.6), labels=c("0","0.3","0.6"),limits= c(0,0.7)) +
  #scale_y_continuous(breaks=c(0,20), labels=c("",""),limits= c(0,20)) +
  #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
  #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
  xlab(expression( "Posterior distribution of the mean" ~theta )) +  ylab(expression("Real" ~ theta )) +
  #ggtitle("Group Level")+
  geom_vline(xintercept = 10 ,linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  #scale_fill_brewer(palette="Blues")+
  #theme_ridges() + 
  facet_wrap(subject~.)+
  theme_bw() +
  theme( plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(text = element_text(family = 'Arial',size=20)) +
  theme(panel.grid = element_blank())


ggplot() +
  geom_density(data=dataSingleSubj, aes(x = noise), color="green2",fill="green2", alpha=0.5, linetype="dashed") +
  #scale_x_continuous(breaks=c(0,0.3,0.6), labels=c("0","0.3","0.6"),limits= c(0,0.7)) +
  #scale_y_continuous(breaks=c(0,20), labels=c("",""),limits= c(0,20)) +
  #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
  #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
  xlab(expression( "Posterior distribution of the mean" ~theta )) +  ylab(expression("Real" ~ theta )) +
  #ggtitle("Group Level")+
  geom_vline(xintercept = 0.0140*sqrt(1000) ,linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  #scale_fill_brewer(palette="Blues")+
  #theme_ridges() + 
  facet_wrap(subject~.)+
  theme_bw() +
  theme( plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(text = element_text(family = 'Arial',size=20)) +
  theme(panel.grid = element_blank())





chain11 = chain[chain$realD==10 & chain$realT==0.4 & chain$realN==0.0140,]
DataFit$subject<- as.numeric(ordered(DataFit$SubjectNum))
### prepare empirical data
dataSingleSubj = data.frame(subject = sort( rep(1:30, (length(chain11[,c("b2.mu")])) ) ))

subj <- 1:30
for (s in 1:length(subj)){
  subNum = subj[s]
  dataSingleSubj$drift[dataSingleSubj$subject==subNum] = chain11[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))]
  dataSingleSubj$theta[dataSingleSubj$subject==subNum] = (chain11[,c( paste( c("thetaGaze[",toString(s),"]"), collapse = ""))])
  dataSingleSubj$noise[dataSingleSubj$subject==subNum] = (chain11[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
  dataSingleSubj$nDT[dataSingleSubj$subject==subNum] = (chain11[,c( paste( c("theta.p[",toString(s),"]"), collapse = ""))])
  dataSingleSubj$bias[dataSingleSubj$subject==subNum] = (chain11[,c( paste( c("bias[",toString(s),"]"), collapse = ""))])
}



ggplot() +
  geom_density(data=dataSingleSubj, aes(x = drift), fill="firebrick3",color="black",alpha=0.9) +
  scale_x_continuous(breaks=c(6,12,14), limits= c(6,15)) +
  #scale_y_continuous(breaks=c(0,20), labels=c("",""),limits= c(0,20)) +
  #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
  #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
  xlab(expression(delta~ "parameter" )) +  ylab("Posterior distribution of the mean") +
  #ggtitle("Posterior distribution of the mean at the individual level")+
  geom_vline(xintercept = 10 ,linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  #scale_fill_brewer(palette="Blues")+
  #theme_ridges() + 
  facet_wrap(subject~.)+
  theme_bw() +
  theme( plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(text = element_text(family = 'Arial',size=20),
        axis.text.y=element_blank()) +
  theme(panel.grid = element_blank())




ggplot() +
  geom_density(data=dataSingleSubj, aes(x = theta), fill="dodgerblue3",color="black",alpha=0.9)+
  scale_x_continuous(breaks=c(0,0.35,0.6), labels=c("0","0.35","0.6"),limits= c(0,0.7)) +
  #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
  #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
  xlab(expression(theta~ "parameter" )) +  ylab("Posterior distribution of the mean") +
  #ggtitle("Posterior distribution of the mean at the individual level")+
  geom_vline(xintercept = 0.4 ,linetype = 'dashed',colour = 'dark gray')+
  labs(fill=" ")+
  #scale_fill_brewer(palette="Blues")+
  #theme_ridges() + 
  facet_wrap(subject~.)+
  theme_bw() +
  theme( plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(text = element_text(family = 'Arial',size=20),
        axis.text.y=element_blank()) +
  theme(panel.grid = element_blank())

# diffParamsSub<- data.frame()
# for( s in unique(meanParamSub$subject)){
# for (t in unique(meanParamSub$realT[meanParamSub$subject==s])){
#   for(n in unique(meanParamSub$realN[meanParamSub$subject==s])){
#     vectReal= sort(meanParamSub$realD[meanParamSub$realT==t & meanParamSub$realN==n & meanParamSub$subject==s])
#     vectEst= sort(meanParamSub$dEst[meanParamSub$realT==t & meanParamSub$realN==n & meanParamSub$subject==s])/1000 
#     diffParamsSub1 = data.frame(realD = c(vectReal[2]-vectReal[1], vectReal[3]-vectReal[2] ), dEst = c(vectEst[2]-vectEst[1], vectEst[3]-vectEst[2]) )
#     diffParamsSub = rbind(diffParamsSub,diffParamsSub1)
#   }
# }
# }
# 
# png( file.path(folder_plots, "differenceD.png"), width = 400, height = 500)
# ggplot(diffParamsSub, aes( x = dEst)) +
#   stat_ecdf(color="red2",size=1.5)+
#   coord_cartesian(xlim = c(-0.001,0.009)) +
#   scale_x_continuous(breaks=c(0,0.004,0.008), label= c("0", "0.004","0.008")) +
#   #scale_y_continuous(breaks=sort(unique(meanParam$realD))) +
#   geom_vline(xintercept = unique(diffParamsSub$realD),linetype = 'dashed',colour = 'dark gray')+
#   xlab(expression("Difference between " ~delta~" estimates")) +  ylab('ECDF') +
#   ggtitle("Real difference 0.004")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial',size=20), plot.title = element_text(size = 20)) +
#   theme(panel.grid = element_blank())
# dev.off()
# 
# 
# 
# 

# chainT<-data.frame()
#   for (t in unique(chain$realD)){
#     for(n in unique(chain$realN)){
#       vectReal= sort(unique(chain$realT[chain$realD==t & chain$realN==n]))
#       for (d in 1:(length(vectReal)-1)){
#         chain1=chain[chain$realD==t & chain$realN==n & chain$realT==vectReal[d],]
#         chain1$diff= chain$thetaGaze.mu[chain$realD==t & chain$realN==n & chain$realT==vectReal[d+1]] - chain$thetaGaze.mu[chain$realD==t & chain$realN==n & chain$realT==vectReal[d]]
#         chain1$realDiff = vectReal[d]
#         chainT = rbind(chainT,chain1)
#       }
#     }
#   }
# 
# chainT$realDiff2= factor(chainT$realDiff, labels = c("0.4-0.2","0.6-0.4","0.8-0.6"))
# # basic example
# ggplot(chainT, aes(x = diff, y= (realDiff2), fill = (realDiff2))) +
#   geom_density_ridges() +
#   scale_x_continuous(breaks=c(0,0.2,0.4), limits=c(-0.1,0.5)) +
#   xlab(expression( "Posterior distribution of the mean "~ sigma) )+  ylab( expression( "Real "~ sigma) )+
#   #ggtitle("Group Level")+
#   geom_vline(xintercept = 0.2 ,linetype = 'dashed',colour = 'dark gray')+
#   #geom_vline(xintercept = 0.4427189 ,linetype = 'dashed',colour = 'dark gray')+
#   #geom_vline(xintercept = 0.6324555 ,linetype = 'dashed',colour = 'dark gray')+
#   labs(fill=" ")+
#   scale_fill_brewer(palette="Blues")+
#   theme_ridges() + 
#   theme_bw() +
#   facet_grid(realD2~realN2, labeller = label_parsed)+
#   theme( plot.title = element_text(hjust = 0.5))+
#   theme(legend.position = "none")+
#   theme(text = element_text(family = 'Arial',size=20)) +
#   theme(panel.grid = element_blank())
# 
# 
# 
# 
# 
# 
# 

# 
# diffParamsSubT<- data.frame()
# for( s in unique(meanParamSub$subject)){
#   for (t in unique(meanParamSub$realD[meanParamSub$subject==s])){
#     for(n in unique(meanParamSub$realN[meanParamSub$subject==s])){
#       vectReal= sort(meanParamSub$realT[meanParamSub$realD==t & meanParamSub$realN==n & meanParamSub$subject==s])
#       vectEst= sort(meanParamSub$thetaEst[meanParamSub$realD==t & meanParamSub$realN==n & meanParamSub$subject==s])
#       diffParamsSubT1 = data.frame(realT = c(vectReal[2]-vectReal[1], vectReal[3]-vectReal[2] ), thetaEst = c(vectEst[2]-vectEst[1], vectEst[3]-vectEst[2]) )
#       diffParamsSubT = rbind(diffParamsSubT,diffParamsSubT1)
#     }
#   }
# }
# 
# png( file.path(folder_plots, "differenceT.png"), width = 400, height = 500)
# ggplot(diffParamsSubT, aes( x = thetaEst)) +
#   stat_ecdf(color="blue2",size=1.5)+
#   coord_cartesian(xlim = c(-0.01,0.45)) +
#   scale_x_continuous(breaks=c(0,0.2,0.4), label= c("0", "0.2","0.4")) +
#   #scale_y_continuous(breaks=sort(unique(meanParam$realD))) +
#   geom_vline(xintercept = unique(diffParamsSubT$realT),linetype = 'dashed',colour = 'dark gray')+
#   xlab(expression("Difference between " ~theta~" estimates")) +  ylab('ECDF') +
#   ggtitle("Real difference 0.2")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial',size=20), plot.title = element_text(size = 20)) +
#   theme(panel.grid = element_blank())
# dev.off()
# 
# 
# diffParamsSubN<- data.frame()
# for( s in unique(meanParamSub$subject)){
#   for (t in unique(meanParamSub$realT[meanParamSub$subject==s])){
#     for(n in unique(meanParamSub$realD[meanParamSub$subject==s])){
#       vectReal= sort(meanParamSub$realN[meanParamSub$realT==t & meanParamSub$realD==n & meanParamSub$subject==s])
#       vectEst= sort(meanParamSub$noiseEst[meanParamSub$realT==t & meanParamSub$realD==n & meanParamSub$subject==s])/sqrt(1000)
#       diffParamsSubN1 = data.frame(realN = c(vectReal[2]-vectReal[1], vectReal[3]-vectReal[2] ), noiseEst = c(vectEst[2]-vectEst[1], vectEst[3]-vectEst[2]) )
#       diffParamsSubN = rbind(diffParamsSubN,diffParamsSubN1)
#     }
#   }
# }
# 
# 
# 
# d<- data.frame(realN= unique(diffParamsSubN$realN), r=c(0.0045,0.006) )
# 
# diffParamsSubN$diff<- diffParamsSubN$realN- diffParamsSubN$noiseEst
# png( file.path(folder_plots, "differenceN.png"), width = 400, height = 500)
# ggplot(diffParamsSubN, aes( x = diff)) +
#   stat_ecdf(color="green2",size=1.5)+
#   coord_cartesian(xlim = c(-0.04,0.04)) +
#   #scale_x_continuous(breaks=c(0,0.2,0.4), label= c("0", "0.2","0.4")) +
#   geom_vline(xintercept = 0,linetype = 'dashed',colour = 'dark gray')+
#   xlab(expression("Difference between " ~delta~" estimates")) +  ylab('ECDF') +
#   ggtitle("Real difference 0.004")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial',size=20), plot.title = element_text(size = 20)) +
#   #facet_grid(.~realN)+
#   theme(panel.grid = element_blank())
# dev.off()
# 
# 
# 
# dev.new(width = 8, height = 4)
# ggplot(meanParam, aes( y = dEst/1000, x = realD)) +
#   geom_point()+
#   coord_cartesian(ylim = c(0.003,0.017),xlim = c(0.003,0.017)) +
#   scale_x_continuous(breaks=sort(unique(meanParam$realD))) +
#   scale_y_continuous(breaks=sort(unique(meanParam$realD))) +
#   geom_abline(intercept = 0 , slope = 1,linetype = 'dashed',colour = 'dark gray')+
#   xlab('Real drift') +  ylab('Mean drift posterior distribution') +
#   ggtitle("Group level")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) +
# theme(panel.grid = element_blank())
# 
# 
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParam11, aes( y = dEst/1000, x = realD)) +
#   geom_point()+
#   coord_cartesian(ylim = c(0.003,0.017),xlim = c(0.003,0.017)) +
#   scale_x_continuous(breaks=sort(unique(meanParam11$realD))) +
#   scale_y_continuous(breaks=sort(unique(meanParam11$realD))) +
#   geom_abline(intercept = 0 , slope = 1,linetype = 'dashed',colour = 'dark gray')+
#   xlab('Real drift') +  ylab('Mean drift posterior distribution') +
#   ggtitle("Group level")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParamSub11, aes( y = dEst/1000, x = realD)) +
#   geom_point()+
#   coord_cartesian(ylim = c(0.003,0.017),xlim = c(0.003,0.017)) +
#   scale_x_continuous(breaks=sort(unique(meanParam11$realD))) +
#   scale_y_continuous(breaks=sort(unique(meanParam11$realD))) +
#   geom_abline(intercept = 0 , slope = 1,linetype = 'dashed',colour = 'dark gray')+
#   xlab('Real drift') +  ylab('Mean drift posterior distribution') +
#   ggtitle("Individual level")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# 
# 
# 
# 
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParam11, aes( y = thetaEst, x = realT)) +
#   geom_point()+
#   coord_cartesian(ylim = c(0,1),xlim = c(0,1)) +
#   scale_x_continuous(breaks=sort(unique(meanParam11$realT))) +
#   scale_y_continuous(breaks=sort(unique(meanParam11$realT))) +
#   geom_abline(intercept = 0 , slope = 1,linetype = 'dashed',colour = 'dark gray')+
#   xlab(expression("Real" ~ theta )) +  ylab(expression( "Mean" ~theta~ "posterior distribution" )) +
#   ggtitle("Group level")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParamSub11, aes( y = thetaEst, x = realT)) +
#   geom_point()+
#   coord_cartesian(ylim = c(0,1),xlim = c(0,1)) +
#   scale_x_continuous(breaks=sort(unique(meanParam11$realT))) +
#   scale_y_continuous(breaks=sort(unique(meanParam11$realT))) +
#   geom_abline(intercept = 0 , slope = 1,linetype = 'dashed',colour = 'dark gray')+
#   xlab(expression("Real" ~ theta )) +  ylab(expression( "Mean" ~theta~ "posterior distribution" )) +
#   ggtitle("Individual level")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# 
# 
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParam11, aes( y = noiseEst/sqrt(1000), x = realN)) +
#   geom_point()+
#   coord_cartesian(ylim = c(0.012,0.027),xlim = c(0.012,0.027)) +
#   scale_x_continuous(breaks=sort(unique(meanParam$realN))) +
#   scale_y_continuous(breaks=sort(unique(meanParam$realN))) +
#   geom_abline(intercept = 0 , slope = 1,linetype = 'dashed',colour = 'dark gray')+
#   xlab('Real noise') +  ylab('Mean noise posterior distribution') +
#   ggtitle("Group level")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParamSub11, aes( y = noiseEst/sqrt(1000), x = realN)) +
#   geom_point()+
#   coord_cartesian(ylim = c(0.012,0.027),xlim = c(0.012,0.027)) +
#   scale_x_continuous(breaks=sort(unique(meanParam$realN))) +
#   scale_y_continuous(breaks=sort(unique(meanParam$realN))) +
#   geom_abline(intercept = 0 , slope = 1,linetype = 'dashed',colour = 'dark gray')+
#   xlab('Real noise') +  ylab('Mean noise posterior distribution') +
#   ggtitle("Individual level")+
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# 
# 
# 
# 
# ##### PLOT OF CORRELATION #######
# # function for t-test and plot the correlation
# source("~/HaDDM_recoveryFitFramingData/ggplot_smooth_fun.R")
# meanParam$diffN <- (meanParam$dEst/1000 - meanParam$realD) / (meanParam$dEst/1000 + meanParam$realD)
# meanParam$diff <- (meanParam$dEst - meanParam$realD*1000) 
# 
# meanParamSub$diffN <- (meanParamSub$dEst/1000 - meanParamSub$realD) / (meanParamSub$dEst/1000 + meanParamSub$realD) 
# meanParamSub$diff <- (meanParamSub$dEst - meanParamSub$realD*1000) 
# 
# 
# dev.new(width = 5, height =5)
# ggplot(meanParam, aes(x=diffN , y=realT)) +
#   geom_point(shape=3) +    # Use hollow circles
#   geom_smooth(method=lm) +    # Add linear regression line
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) + 
#   xlab(expression("Real" ~ theta )) +
#   ylab('Difference in drift estimate and real') +
# theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# 
# 
# 
# 
# dev.new(width = 5, height =5)
# ggplot(meanParamSub, aes(y=diff , x=realT)) +
#   geom_point(shape=3) +    # Use hollow circles
#   geom_smooth(method=lm) +    # Add linear regression line
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) + 
#   xlab(expression("Real" ~ theta )) +
#   ylab('Difference in drift estimate and real') +
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# 
# 
# dev.new(width = 5, height =5)
# ggplot(meanParamSub, aes(y=diffN , x=realT)) +
#   geom_point(shape=3) +    # Use hollow circles
#   geom_smooth(method=lm) +    # Add linear regression line
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) + 
#   xlab(expression("Real" ~ theta )) +
#   ylab('Normalized difference in drift estimate and real') +
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# 
# 
# 
# 
# 
# 
# 
# dev.new(width = 5, height =5)
# ggplot(meanParamSub, aes(y=diff , x=realD)) +
#   geom_point(shape=3) +    # Use hollow circles
#   geom_smooth(method=lm) +    # Add linear regression line
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) + 
#   xlab('Real drift')+
#   ylab('Difference in drift estimate and real') +
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# 
# 
# dev.new(width = 5, height =5)
# ggplot(meanParamSub, aes(y=diffN , x=realD)) +
#   geom_point(shape=3) +    # Use hollow circles
#   geom_smooth(method=lm) +    # Add linear regression line
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) + 
#   xlab('Real drift')+
#   ylab('Normalized difference in drift estimate and real') +
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# 
# 
# 
# 
# dev.new(width = 5, height =5)
# ggplot(meanParamSub, aes(y=diff , x=realN)) +
#   geom_point(shape=3) +    # Use hollow circles
#   geom_smooth(method=lm) +    # Add linear regression line
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) + 
#   xlab('Real noise')+
#   ylab('Difference in drift estimate and real') +
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# 
# 
# dev.new(width = 5, height =5)
# ggplot(meanParamSub, aes(y=diffN , x=realN)) +
#   geom_point(shape=3) +    # Use hollow circles
#   geom_smooth(method=lm) +    # Add linear regression line
#   stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) + 
#   xlab('Real noise')+
#   ylab('Normalized difference in drift estimate and real') +
#   theme_bw() +
#   theme(text = element_text(family = 'Arial')) 
# 
# 
# diffDreg<- lm(diff ~ realD*realT*realN, data = meanParamSub)
# diffDreg<- lmer(diff ~ realD*realT*realN + (1 + realD*realT*realN | subject), data = meanParamSub)
# 
# meanParamSub$realDs<- scale(meanParamSub$realD)
# meanParamSub$realTs<- scale(meanParamSub$realT)
# meanParamSub$realNs<- scale(meanParamSub$realN)
# diffDreg<- lmer(diff ~ realDs*realTs*realN + (1 + realDs*realTs*realNs | subject), data = meanParamSub, control=lmerControl(optCtrl=list(maxfun= 1000000))) 
# 
# diffDNreg<- lmer(diffN ~ realDs*realTs*realN + (1 + realDs*realTs*realNs | subject), data = meanParamSub, control=lmerControl(optCtrl=list(maxfun= 1000000))) 
# 
# 
# 
# 
# 
# 
# #meanParamADDM<- meanParam
# #load("~/HtSSM_expdataFit/recoveryFitting_fixedParams/meanParams_estimates.Rda")
# meanParam20<- data.frame(diff = c(meanParamADDM$diff,meanParam$diffh*2,meanParam$difft*2), 
#                          diffN = c(meanParamADDM$diffN,meanParam$diffNh,meanParam$diffNt), 
#                          type = c(rep("drift-aDDM",length(meanParamADDM$diff)), rep("weightT-HtSSM",length(meanParam$difft)),rep("weightH-HtSSM",length(meanParam$diffh)) ) )
# 
# for (t in unique(meanParam20$type)){
# meanParam20$diffN[meanParam20$type==t]<- meanParam20$diff[meanParam20$type==t]/
#           ( max(meanParam20$diff[meanParam20$type==t]) - min(meanParam20$diffN[meanParam20$type==t]))
# }
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParam20, aes( y = diff, x= factor(type),fill= factor(type))) +
#   geom_boxplot()+
#   geom_jitter()+
#   coord_cartesian(ylim = c(-2,2)) +
#   geom_hline(yintercept = 0 ,linetype = 'dashed',colour = 'dark gray')+
#   xlab('Theta Real') +  ylab('Theta estimate') +
#   ggtitle("Drift rate = 0.002")+
#   theme_bw() +
#   #facet_grid(.~type)+
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# 
# 
# dev.new(width = 8, height = 4)
# fig2 <- ggplot(meanParam20, aes( y = diffN,  x= factor(type),fill= factor(type))) +
#   geom_boxplot()+
#   geom_jitter()+
#   coord_cartesian(ylim = c(-0.2,1)) +
#   geom_hline(yintercept = 0 ,linetype = 'dashed',colour = 'dark gray')+
#   xlab('Theta Real') +  ylab('Theta estimate') +
#   ggtitle("Drift rate = 0.002")+
#   theme_bw() +
#   #facet_grid(.~type)+
#   theme(text = element_text(family = 'Arial')) 
# #theme(panel.grid = element_blank())
# print(fig2) 
# 
# anova1<-aov(diffN ~ type, data = meanParam20)
# 
# other<-wilcox.test(meanParam20$diffN[meanParam20$type=="drift-aDDM"], meanParam20$diffN[meanParam20$type=="weightT-HtSSM"], alternative = "two.sided")
# 



