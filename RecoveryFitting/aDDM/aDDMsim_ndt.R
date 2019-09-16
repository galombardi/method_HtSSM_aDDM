aDDM.sim <- function(params,nsim, data_mat, data){

	# repeat fixation data nsim times 
	data_mat <- data_mat[rep(seq_len(nrow(data_mat)), each =nsim),]
	data<-data[rep(seq_len(nrow(data)), each =nsim),]
	maxfix<-(ncol(data_mat)-6)/2 
	#maximum number of fixations in a given trial
	#define matrices needed to calculate the big drift in the decision-process
	#this will take in account ALL the simulated decisions
	roi_mat<-data_mat[,7:(6+maxfix)]
	fixlength_mat<-data_mat[,(7+maxfix):ncol(data_mat)]
	ev_mat <- ((data$Ev)/100)*matrix(1,nrow=nrow(data_mat),ncol=maxfix)
	
	ce_mat <- ((data$Ce)/100) * matrix(1,nrow=nrow(data_mat),ncol=maxfix)

	#big drift for all the decisions with all the d and theta parameter values from the random grid
	drift_mat <- params[1] * (
        (roi_mat==1) *  ( ce_mat - params[2] * ev_mat)  +
        (roi_mat==2) * (params[2] * ce_mat - ev_mat)  +
        (roi_mat==3) *  (ce_mat -  params[2] * ev_mat) +
        (roi_mat==4) * ( params[2] * ce_mat - ev_mat) )


	#vectors store simulated results
	n.fix <- rep(NA,nrow(data_mat))
	choice <- rep(NA,nrow(data_mat)) ### 1 = gamble chosen, 0 = safe chosen 
	fix.time <- rep(NA,nrow(data_mat))
	last.roi <- rep(NA,nrow(data_mat))
	last.fix <- rep(NA,nrow(data_mat))
	fix.monGamble <- rep(NA,nrow(data_mat))
	fix.monSafe <- rep(NA,nrow(data_mat))
	fix.probGamble <- rep(NA,nrow(data_mat))
	fix.probSafe <- rep(NA,nrow(data_mat))
	fix.gamble <- rep(NA,nrow(data_mat))
	fix.safe <- rep(NA,nrow(data_mat))
		
	for (i in 1:nrow(data_mat)){
		nroi<-roi_mat[i,]
		nfixlength<-fixlength_mat[i,]
		ndrift<-drift_mat[i,]
		ntemp = cumsum(nfixlength)
		index = rep(0,max(ntemp))
		index[ntemp[1:length(ntemp)-1]+1] = 1
		index[1] = 1
		index = cumsum(index)
		bigdrift = ndrift[index]
		maxt<-max(ntemp) #maximum possible RT (in ms)
		
		trt<-Inf
		noise <- rnorm(maxt,0, params[3] )

		evidence<- bigdrift + noise
		RDV <- params[4] + cumsum(evidence)
		absRDV<-abs(RDV)
		trt<-min(which(absRDV>=1))#find the first time that the evidence exceeds a magnitude of 1
		ifelse(trt==Inf,trt<-maxt,trt<-trt)#set maxt as the finishing time if time runs out
		
		### decision time and choice to the left or to the right
		n.fix[i] <- index[trt]
		choice[i] <- ceiling(RDV[trt]/100000000)
		fix.time[i] <- trt
		last.roi[i] <- nroi[n.fix[i]]
		
		### total fixation time on the left and the right item
		last.fix[i] <- trt - sum(nfixlength[1:n.fix[i]-1])
		fix.seq <- c(nfixlength[1:n.fix[i]-1], last.fix[i]) 
		
		fix.monGamble[i] <- sum( (nroi[1:n.fix[i]] == 1) * fix.seq )
		fix.monSafe[i] <- sum( (nroi[1:n.fix[i]] == 2) * fix.seq )
		fix.probGamble[i] <- sum( (nroi[1:n.fix[i]] == 3) * fix.seq )
		fix.probSafe[i] <- sum( (nroi[1:n.fix[i]] == 4) * fix.seq )
		
		fix.gamble[i] <- fix.monGamble[i] + fix.probGamble[i]
		fix.safe[i] <- fix.monSafe[i] + fix.probSafe[i]
		
	} 
	trial.num <- data_mat[,1]
	trialType <- data_mat[,6]
	Ev <- data_mat[,4]
	Ce <- data$Ce
    #qlag <- data$qlag

    ndt <- params[5] ### create ndt variable in data
    
	RT <- fix.time + ndt
	attlag <- fix.gamble - fix.safe
	sims <- data.frame(trial.num, trialType,Ev, Ce, n.fix, choice, fix.time, ndt, RT, last.roi, fix.monGamble,fix.probGamble, fix.monSafe, fix.probSafe, fix.gamble, fix.safe,attlag)
	return <- sims
}
