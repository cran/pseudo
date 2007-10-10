"pseudoci" <-
function(time, event, tmax){

	if(any(is.na(time)))
		stop("missing values in 'time' vector")
		
	if(any(time)<0)
		stop("'time' must be nonnegative")
	
	if(any(is.na(event)))
		stop("missing values in 'event' vector")
	
	if(any(event!=0 & event!=1 & event!=2))
		stop("'event' must be a 0/1/2 variable (alive/cause 1/cause 2)")
		
	if(missing(tmax)) 
		tmax <- unique(time[event!=0])
	
	if(any(is.na(tmax)))
		stop("missing values in 'tmax' vector")
	
	if (sum(tmax > max(time)) > 0) 
	   stop ("tmax greater than largest observation time")
   
	tmax <- sort(tmax)
	ltmax <- length(tmax)
	howmany <- length(time)

	# preparing the output
	pseudo <- as.data.frame(matrix(data = NA, ncol = 2*ltmax+3, nrow = howmany))
	pseudo[,1] <- 1:howmany
	pseudo[,2] <- time
	pseudo[,3] <- event
	
	names(pseudo) <- c("id","time", "event", paste(rep(c("r1","r2"),ltmax),rep(round(tmax,3),each=2),sep=",t="))
	
	# sort in time
	pseudo <- pseudo[order(pseudo$time,-pseudo$event),]
	
	# time points chosen	
	tu <- unique(pseudo$time[pseudo$event!=0])
	ltu <- length(tu)
	tu <- matrix(tu,byrow=TRUE,ncol=ltu,nrow=ltmax)
	tgiven <- matrix(tmax,byrow=FALSE,ncol=ltu,nrow=ltmax)
	inx <- apply(tgiven>=tu,1,sum)

	# CI, leave one out
	pseu <- ci.omit(pseudo)
	CI1 <- pseu$C1[,inx,drop=FALSE]
	CI2 <- pseu$C2[,inx,drop=FALSE]
	CI.omit <- matrix(rbind(CI1,CI2),ncol=2*ltmax,nrow=nrow(CI1))

	# CI, all cases
	CI.tot <- ci.tot(pseudo)[,inx]
	CI.tot <- matrix(CI.tot,byrow=TRUE,ncol=2*ltmax,nrow=howmany)
	
      	## Pseudo-observations
      	pseudo[,4:(3+2*ltmax)] <- howmany*CI.tot - (howmany-1)*CI.omit

	#back to original order
   	pseudo <- pseudo[order(pseudo$id),]
   	pseudo <- pseudo[,-1]
	
	return(pseudo)
}

