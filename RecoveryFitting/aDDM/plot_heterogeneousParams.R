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

main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM/b_RecoveryParams_heterogeneousParams")
folder_plots<- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM/plots")


filelist = file.path(main_folder,"results_HaDDM_heterogParams.RData")
  
  load(filelist)
  chain=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))
  
  
  d <- 12/1000
  theta <- 0.35
  noise <- 0.41/sqrt(1000)
  bias <- 0
  ndt<- 0.3
  
  noiseDist<- rnorm(20000,noise,0.0008)
  biasDist<- rnorm(20000,bias,0.01)
  thetaDist<- rnorm(20000,theta,0.05)
  dDist<- rnorm(20000,d,0.0005)
  
  
  
  distr <- data.frame( noise= noiseDist, bias=biasDist, theta= thetaDist,drift = dDist )
  
  distrSubj <- data.frame(theta=unique(DataFit$theta),drift =unique(DataFit$drift) )

  
  DataFit$subject<- as.numeric(ordered(DataFit$SubjectNum))
  ### prepare empirical data
  dataSingleSubj = data.frame(subject = sort( rep(unique(DataFit$subject), (length(chain[,c("b2.mu")])) ) ))
  
  subj <- unique(DataFit$subject)
  for (s in 1:length(subj)){
    subNum = subj[s]
    dataSingleSubj$drift[dataSingleSubj$subject==subNum] = chain[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))]
    dataSingleSubj$theta[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("thetaGaze[",toString(s),"]"), collapse = ""))])
    dataSingleSubj$noise[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
    dataSingleSubj$nDT[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("theta.p[",toString(s),"]"), collapse = ""))])
    dataSingleSubj$bias[dataSingleSubj$subject==subNum] = (chain[,c( paste( c("bias[",toString(s),"]"), collapse = ""))])
  }
  
  
  
  
   # basic example
  png( file.path(folder_plots, ".."), width = 800, height = 500)
  #dev.new(width = 8, height = 4)
  ggplot() +
    geom_histogram(data=distr, aes(x = theta, y= ..density..), color="blue", binwidth = 0.02) +
    geom_density(data=chain, aes(x = thetaGaze.mu), color="red",fill="red", alpha=0.5, linetype="dashed",size=1) +
    scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6), limits= c(0,0.6)) +
    scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression( "Posterior distribution of the mean" ~theta )) +  ylab(expression("Real" ~ theta )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = mean(distr$theta) ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    #theme( plot.title = element_text(hjust = 0.5))+
    #theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial',size=20)) +
    theme(panel.grid = element_blank())
  dev.off()
  
  ggplot() +
    geom_histogram(data=distr, aes(x = theta, y= ..density..), color="blue", binwidth = 0.015) +
    geom_density(data=chain, aes(x = thetaGaze.mu), color="red",fill="red", alpha=0.5, linetype="dashed",size=1) +
    scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6), limits= c(0,0.6)) +
    scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression( "Posterior distribution of the mean" ~theta )) +  ylab(expression("Real" ~ theta )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = 0.3 ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    #theme( plot.title = element_text(hjust = 0.5))+
    #theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial',size=20)) +
    theme(panel.grid = element_blank())
  
  
  ggplot() +
    geom_histogram(data=distrSubj, aes(x = drift, y= ..density..), color="blue", binwidth = 0.0001) +
    geom_density(data=chain, aes(x = b2.mu/1000), color="red",fill="red", alpha=0.5, linetype="dashed",size=1) +
    #scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8)) +
    # xlab(expression( "Posterior distribution of the mean" ~theta )) +  ylab(expression("Real" ~ theta )) +
    #ggtitle("Group Level")+
    #geom_vline(xintercept = 0.2 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.3 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.4 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.5 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.6 , linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.7 , linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.8 , linetype = 'dashed',colour = 'dark gray')+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial',size=20)) +
    theme(panel.grid = element_blank())
  

  ggplot() +
    geom_density(data=chain, aes(x = alpha.mu), color="blue") +
    geom_density(data=distr, aes(x = noise*sqrt(1000)), color="red") +
    #scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8)) +
    # xlab(expression( "Posterior distribution of the mean" ~theta )) +  ylab(expression("Real" ~ theta )) +
    #ggtitle("Group Level")+
    #geom_vline(xintercept = 0.2 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.3 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.4 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.5 ,linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.6 , linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.7 , linetype = 'dashed',colour = 'dark gray')+
    #geom_vline(xintercept = 0.8 , linetype = 'dashed',colour = 'dark gray')+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial',size=20)) +
    theme(panel.grid = element_blank())
  
  
  
  png( file.path(folder_plots, "aDDMdistrIndiviTheta.png"), width = 5.5*300, height = 6.6*300,res = 300)
  #dev.new(width = 8, height = 4)
  ggplot() +
    geom_density(data=dataSingleSubj, aes(x = theta), fill="dodgerblue3",color="black",alpha=0.9)+
    scale_x_continuous(breaks=c(0,0.35,0.6), labels=c("0","0.35","0.6"),limits= c(0,0.7)) +
    #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(theta)) +  ylab("Posterior distribution of the mean") +
    #ggtitle("Posterior distribution of the mean at the individual level")+
    geom_vline(data= DataFit,aes(xintercept = theta) ,linetype = 'dashed',colour = 'dark gray')+
   labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    facet_wrap(subject~.,nrow = 6, ncol = 5)+
    theme_bw() +
    #theme( plot.title = element_text(hjust = 0.5))+
    #theme(legend.position = "none")+
    #theme_classic()+
    theme(text = element_text(family = 'Arial'),
          axis.text.y=element_blank(),
          strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
    theme(panel.grid = element_blank())
  dev.off()
  
  
  
  
  png( file.path(folder_plots, "aDDMdistrIndiviDrift.png"), width = 5.5*300, height = 6.6*300,res = 300)
  ggplot() +
    geom_density(data=dataSingleSubj, aes(x = drift), fill="firebrick3",color="black",alpha=0.9) +
    scale_x_continuous(breaks=c(6,12,14), limits= c(6,15)) +
    #scale_y_continuous(breaks=c(0,20), labels=c("",""),limits= c(0,20)) +
    #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(delta)) +  ylab("Posterior distribution of the mean") +
    #ggtitle("Posterior distribution of the mean at the individual level")+
    geom_vline(data= DataFit,aes(xintercept = drift*1000) ,linetype = 'dashed',colour = 'dark gray')+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    facet_wrap(subject~.,nrow = 6, ncol = 5)+
    theme_bw() +
    theme(text = element_text(family = 'Arial'),
          axis.text.y=element_blank(),
          strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
    theme(panel.grid = element_blank())
  dev.off()
  
  
  png( file.path(folder_plots, "aDDMdistrIndiviNoise.png"),  width = 5.5*300, height = 6.6*300,res = 300)
  ggplot() +
    geom_density(data=dataSingleSubj, aes(x = noise), color="black",fill="springgreen4", alpha=0.9) +
    scale_x_continuous(breaks=c(0.3,mean(distr$noise)*sqrt(1000),0.6), labels= c("0.3","0.4","0.6"),limits= c(0.2,0.7)) +
    #scale_y_continuous(breaks=c(0,20), labels=c("",""),limits= c(0,30)) +
    #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(sigma)) +  ylab("Posterior distribution of the mean") +
    #ggtitle("Posterior distribution of the mean at the individual level")+
    geom_vline(data= DataFit,aes(xintercept = noise*sqrt(1000)) ,linetype = 'dashed',colour = 'dark gray')+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    facet_wrap(subject~.,nrow = 6, ncol = 5)+
    theme_bw() +
    theme(text = element_text(family = 'Arial'),
          axis.text.y=element_blank(),
          strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
    theme(panel.grid = element_blank())
  dev.off()
  

  
  
  
  
  ptheta<- ggplot() +
    geom_histogram(data=distr, aes(x = theta, y= ..density..), fill="dodgerblue3", alpha=1,binwidth = 0.01) +
    scale_x_continuous(breaks=c(0,0.35,1), limits= c(0,1)) +
    scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(theta )) +  ylab(expression("density" )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = mean(distr$theta) ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial'),
          axis.text.y=element_blank()) +
  theme(panel.grid = element_blank())
  
  
  
  pdrift<- ggplot() +
    geom_histogram(data=distr, aes(x = drift*1000, y= ..density..), fill="firebrick3",alpha=1, binwidth = 0.1) +
    scale_x_continuous(breaks=c(6,12,14), limits= c(6,15)) +
    #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(delta )) +  ylab(expression(" " )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = mean(distr$drift)*1000 ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial'),
          axis.text.y=element_blank()) +
    theme(panel.grid = element_blank())
  
  pnoise<- ggplot() +
    geom_histogram(data=distr, aes(x = noise*sqrt(1000), y= ..density..), fill="springgreen4",alpha=1, binwidth = 0.01) +
    scale_x_continuous(breaks=c(0.3,mean(distr$noise)*sqrt(1000),0.6), labels= c("0.3","0.4","0.6"),limits= c(0.2,0.7)) +
    #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(sigma )) +  ylab(expression("" )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = mean(distr$noise)*sqrt(1000) ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial'),
          axis.text.y=element_blank()) +
    theme(panel.grid = element_blank())
  
  png( file.path(folder_plots, "aDDMparamsDistr.png"), width = 5*300, height = 2*300,res = 300)
  grid.arrange(ptheta,pdrift,pnoise,nrow=1)
  dev.off()
  
  
  
  pthetaC<- ggplot() +
    geom_density(data=dataSingleSubj, aes(x = theta), fill="dodgerblue3",color="black", alpha=0.9) +
    scale_x_continuous(breaks=c(0,0.35,1), limits= c(0,1),labels= c("0","0.35","1")) +
    scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(theta )) +  ylab(expression("density" )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = mean(distr$theta) ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial',size=20),
          axis.text.y=element_blank()) +
    theme(panel.grid = element_blank())
  
  
  
  pdriftC<- ggplot() +
    geom_density(data=dataSingleSubj, aes(x = drift), fill="firebrick3",color="black",alpha=0.9) +
    scale_x_continuous(breaks=c(6,12,14), limits= c(6,15)) +
    #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(delta )) +  ylab(expression(" " )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = mean(distr$drift)*1000 ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    #scale_fill_brewer(palette="Blues")+
    #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial',size=20),
          axis.text.y=element_blank()) +
    theme(panel.grid = element_blank())
  
  pnoiseC<- ggplot() +
    geom_density(data=dataSingleSubj, aes(x = noise),fill="springgreen4",color="black",alpha=0.9) +
    scale_x_continuous(breaks=c(0.3,mean(distr$noise)*sqrt(1000),0.6), labels= c("0.3","0.4","0.6"),limits= c(0.2,0.7)) +
    #scale_color_manual("Legend Title", limits=c("Weekly Forecast", "Main Forecast"), values = c("blue","red")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("red", "white"))))+
    xlab(expression(sigma )) +  ylab(expression("" )) +
    #ggtitle("Group Level")+
    geom_vline(xintercept = mean(distr$noise)*sqrt(1000) ,linetype = 'dashed',colour = 'dark gray')+
    geom_hline(yintercept = 0 ,colour = 'white', size=1.5)+
    geom_hline(yintercept = 0 ,colour = 'black',size=0.2)+
    labs(fill=" ")+
    scale_fill_hue(l=40)+
  #theme_ridges() + 
    theme_bw() +
    theme( plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(text = element_text(family = 'Arial',size=20),
          axis.text.y=element_blank()) +
    theme(panel.grid = element_blank())
  
  png( file.path(folder_plots, "aDDMgroupLevFitDistr.png"), width = 700, height = 200)
  grid.arrange(pthetaC,pdriftC,pnoiseC,nrow=1, top =textGrob("Posterior distributions of the mean at the group level",hjust = 0.69,gp=gpar(fontsize=20,font=1)))
  dev.off()
  
  
  
  
  
  
  
