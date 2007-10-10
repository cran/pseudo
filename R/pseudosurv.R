"pseudosurv" <-
function(time, event, tmax){
	
	if(any(is.na(time)))
		stop("missing values in 'time' vector")
		
	if(any(time)<0)
		stop("'time' must be nonnegative")
	
	if(any(is.na(event)))
		stop("missing values in 'event' vector")
	
	if(any(event!=0 & event!=1))
		stop("'event' must be a 0/1 variable (alive/dead)")
		
	if(missing(tmax)) 
		tmax <- unique(time[event==1])
	
	if(any(is.na(tmax)))
		stop("missing values in 'tmax' vector")
	
	if (sum(tmax > max(time)) > 0) 
	   stop ("tmax greater than largest observation time")
	   
	tmax <- sort(tmax)
	ltmax <- length(tmax)
	howmany <- length(time)
	

	## preparing the output
	pseudo <- as.data.frame(matrix(data = NA, ncol = length(tmax)+3, nrow = howmany))
	pseudo[,1] <- 1:howmany
	pseudo[,2] <- time
	pseudo[,3] <- event
	names(pseudo) <- c("id","time", "event",paste("tmax =", round(tmax,3), sep=""))

	# sort in time
	pseudo <- pseudo[order(pseudo$time,-pseudo$event),]

	# time points chosen	
	tu <- unique(pseudo$time[pseudo$event==1])
	ltu <- length(tu)
	tu <- matrix(tu,byrow=TRUE,ncol=ltu,nrow=ltmax)
	tgiven <- matrix(tmax,byrow=FALSE,ncol=ltu,nrow=ltmax)
	inx <- apply(tgiven>=tu,1,sum)
	
	# KM, leave one out
	KM.omit <- surv.omit(pseudo)
	KM.omit <- KM.omit[,inx,drop=FALSE]
	
	# KM, all cases
	KM.tot <- surv.tot(pseudo)[inx]
	KM.tot <- matrix(KM.tot,byrow=TRUE,nrow=howmany,ncol=length(tmax))
	
	# pseudo-observations
	pseu <- howmany*KM.tot - (howmany-1)*KM.omit
	
	# back to original order
	pseudo[,-(1:3)] <- pseu
	pseudo <- pseudo[order(pseudo$id),]
	pseudo <- pseudo[,-1]
	return(pseudo)

}

