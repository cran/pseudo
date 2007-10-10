"pseudomean" <-
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
		tmax <- max(time[event==1])

	if(is.na(tmax))
		stop("missing value of 'tmax'")

	
	rmtime <- ifelse(time >= tmax ,tmax,time)
	rmdead <- ifelse(time >= tmax ,0, event)

	if(sum(rmdead)==0)
		stop("no events occured before time 'tmax'")
    
	howmany <- length(rmtime)
    	
    
	## preparing the output
    	pseudo <- as.data.frame(matrix(data = NA, ncol = 6, nrow = howmany))
    	pseudo[,1] <- 1:howmany
    	pseudo[,2] <- rmtime
    	pseudo[,3] <- time
    	pseudo[,4] <- event
    	pseudo[,5] <- rmdead
    	names(pseudo) <- c("id","time","timet","eventt","event","psumean")
    
    	# sort in time
    	pseudo <- pseudo[order(pseudo$time,-pseudo$event),]
	 
    
	# RM, leave one out
	RM.omit <- surv.omit(pseudo,tmax)
	
	#RM, all cases
	RM.tot <- surv.tot(pseudo,tmax)

	# pseudo-observations
	pseu <- howmany*RM.tot - (howmany-1)*RM.omit

	#back to original order
	pseudo[,-(1:5)] <- pseu
	pseudo <- pseudo[order(pseudo$id),]
	pseudo <- pseudo[,-c(1,2,5)]
	names(pseudo)[1:2] <- c("time","event")
	return(pseudo)    
}

