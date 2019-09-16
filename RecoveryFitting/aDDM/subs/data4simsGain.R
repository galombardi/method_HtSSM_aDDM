rm(list=ls())
pack = "gtools"
have = pack %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( pack[!have] ) }
library(gtools)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CHANGE THIS WITH THE PATH WHERE YOU SAVE ALL THE SCRIPTS AND THE DATA
pathToFolder <- getwd()

# DATA FILE NAME
mydata <- "Data_RiskySafe.Rda"
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# load example data of a bynary food choice task
main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM")

load(file.path(main_folder, mydata))

### prepare data set for creating fixation matrices and for the simulations
Ev <- Data$Ev[Data$InitialFixation == 1]
fix.nums <- Data$NumFixations[Data$InitialFixation == 1]

Ev2 <- as.integer(cut(Ev, quantile(Ev, probs = 0:10/10, na.rm = TRUE), include.lowest=TRUE))

Data$Ev2 <- rep(Ev2,fix.nums)
Data$RT <- Data$RT * 1000

subs <- unique(Data$SubjectNum)
for (subject in subs){

	randfix<-1 #do you want to add random fixations when the data runs out?
	
	data <- Data[Data$SubjectNum == subject & Data$TrialType == 1,]
	lower_bound<-0 #lower RT bound in ms
	upper_bound<-100000 #upper RT bound in ms
	#throw out "fast guesses" and really long outliers
	data<-data[data$RT>lower_bound & data$RT<upper_bound,]

	### Vector for 4 ROIs
	data$roi <- data$FixateMonGamble
	data$roi[data$FixateMonSafe == 1] <- 2
	data$roi[data$FixateProbGamble == 1] <- 3
	data$roi[data$FixateProbSafe == 1] <- 4
	
	### maximun number of fixations
	dmaxfix <- max(data$NumFixations)
	
	### bin fixations on the safe option and the gamble according to expected value
	#basic features of the data
	drois<-max(data$roi) #how many rois?
	dtrials<-nrow(data[data$FixationNum ==1,]) #how many trials total?
	temp<-data[data$MiddleFixation == 1,]
	
	#prepare data matrix
	data_mat<-matrix(0,nrow=dtrials,ncol=(2+drois+dmaxfix*2)) #choice,rt,roi values,rois,fixation lengths
	
	n=0
	for (i in 1:nrow(data)){
		if (data$FixationNum[i]==1){
			n<-n+1
			data_mat[n,1]<- data$trialCount[i]
			#data_mat[n,1]<- n
			data_mat[n,2]<- data$GambleChosen[i] #1=Gamble chosen, 0=Safe chosen
			data_mat[n,3]<- data$RT[i] #in milliseconds
			data_mat[n,4]<- data$Ev[i] #Expected value (same for gamble and safe)
			data_mat[n,5]<- data$Ev2[i] #Expected value in n bins
			data_mat[n,6]<- data$TrialType[i] #To check that this matrix if Gain Frame trials
			data_mat[n,7]<- data$roi[i] #1= Gamble, 0= Safe

			data_mat[n,(7+dmaxfix)]<-data$fixationTime[i]
			
			
		}
		if (data$FixationNum[i]>1){
			data_mat[n,(6+data$FixationNum[i])]<-data$roi[i]
			data_mat[n,(6+dmaxfix+data$FixationNum[i])]<-data$fixationTime[i]
		}
	}
	
	#fill in missing fixation locations and durations for 4 itemsData_noATT
	####fill in missing fixation locations
	prois<- data$roi
	for (i in 1:nrow(data_mat)){
		for (j in 7:(6+dmaxfix)){
			if (data_mat[i,j]==0) (data_mat[i,j]=(sample(prois[prois!=data_mat[i,j-1]],1))) 
		}
	}
	
	

	### fill extra fixations durations as a function of the Ev2 
	new.fix.MonGamble <- vector(mode="numeric", length=0) 
	new.fix.MonSafe <- vector(mode="numeric", length=0)
	new.fix.ProbGamble <- vector(mode="numeric", length=0) 
	new.fix.ProbSafe <- vector(mode="numeric", length=0)
   
	for (j in 1: length(unique(data$Ev2))) {
		trial.index <-  data_mat[data_mat[,5] == j,1]
		trials <- length(trial.index)
		n.extra <- trials * dmaxfix	
		
		if (length(temp$fixationTime[temp$roi == 1  & temp$Ev2 == j]) > 0 )  {
			a <- temp$fixationTime[temp$roi == 1 & temp$Ev2 == j]
		}
		aa <- sample(a, n.extra ,replace=TRUE)
		dim(aa) <- c(trials ,dmaxfix )
		aa <- cbind(aa,trial.index)
		new.fix.MonGamble <- rbind(new.fix.MonGamble,aa)

		
		if (length(temp$fixationTime[temp$roi ==2  & temp$Ev2 == j]) > 0)  {
			b <- temp$fixationTime[temp$roi == 2 & temp$Ev2 == j]
		}
		bb <- sample(b, n.extra ,replace=TRUE)
		dim(bb) <- c(trials ,dmaxfix )
		bb <- cbind(bb,trial.index)
		new.fix.MonSafe <- rbind(new.fix.MonSafe,bb)

		if (length(temp$fixationTime[temp$roi == 3  & temp$Ev2 == j]) > 0 )  {
			c <- temp$fixationTime[temp$roi == 3 & temp$Ev2 == j]
		}
		cc <- sample(c, n.extra ,replace=TRUE)
		dim(cc) <- c(trials ,dmaxfix )
		cc <- cbind(cc,trial.index)
		new.fix.ProbGamble <- rbind(new.fix.ProbGamble,cc)

		if (length(temp$fixationTime[temp$roi == 4  & temp$Ev2 == j]) > 0 )  {
			d <- temp$fixationTime[temp$roi == 4 & temp$Ev2 == j]
		}
		dd <- sample(d, n.extra ,replace=TRUE)
		dim(dd) <- c(trials ,dmaxfix )
		dd <- cbind(dd,trial.index)
		new.fix.ProbSafe <- rbind(new.fix.ProbSafe,dd)

		
	}

	new.fix.MonGamble <-new.fix.MonGamble[order(new.fix.MonGamble[,ncol(new.fix.MonGamble)]),]	
	new.fix.MonSafe <-new.fix.MonSafe[order(new.fix.MonSafe[,ncol(new.fix.MonSafe)]),]
	new.fix.ProbGamble <-new.fix.ProbGamble[order(new.fix.ProbGamble[,ncol(new.fix.ProbGamble)]),]
	new.fix.ProbSafe <-new.fix.ProbSafe[order(new.fix.ProbSafe[,ncol(new.fix.ProbSafe)]),]
	
	### combine extra fixations according to the assigned locations
	ind.MonGamble <- data_mat[,7:(6+dmaxfix)] == 1
	ind.MonSafe <- data_mat[,7:(6+dmaxfix)] == 2
	ind.ProbGamble <- data_mat[,7:(6+dmaxfix)] == 3
	ind.ProbSafe <- data_mat[,7:(6+dmaxfix)] == 4

	extra.fix <- ind.MonGamble*new.fix.MonGamble[,1:dmaxfix] + ind.MonSafe*new.fix.MonSafe[,1:dmaxfix] +
	ind.ProbGamble*new.fix.ProbGamble[,1:dmaxfix] + ind.ProbSafe*new.fix.ProbSafe[,1:dmaxfix]
	
	###combine real and extra fixations
	ind.real.fix <- data_mat[,(7+dmaxfix): ncol(data_mat) ] > 0
	ind.extra.fix <- data_mat[,(7+dmaxfix): ncol(data_mat)] == 0
	data_mat[,(7+dmaxfix): ncol(data_mat)] <- ind.real.fix* data_mat[,(7+dmaxfix): ncol(data_mat)] + ind.extra.fix* extra.fix 
	
    subFolder<-file.path(pathToFolder,"method_HtSSM_aDDM/RecoveryFitting/aDDM/subs",toString(subject))
	dir.create(subFolder)
	sub.dir <- paste(c(subFolder,"/data_preppedGain.Rda"), collapse = "")
	save(data_mat,file= sub.dir)
}
