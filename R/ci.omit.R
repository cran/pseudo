"ci.omit" <-
function(pseudo,tmax){
	
	#calculate cum. inc. function, leave one out
	
	howmany <- nrow(pseudo)
	
	d1 <- as.numeric(pseudo$event==1)
	d2 <- as.numeric(pseudo$event==2)
	event <- as.numeric(pseudo$event>0)
	
	td <- pseudo$time[event==1]
	lt.temp <- c(td[-1],td[length(td)]+1)
	lt <- which(td!=lt.temp)
	
	#km - i
	Y1 <- matrix(howmany:1,byrow=TRUE,ncol=howmany,nrow=howmany)
	Y2 <- matrix((howmany-1):0,byrow=TRUE,ncol=howmany,nrow=howmany)
	Y <- upper.tri(Y1,diag=FALSE)*Y1+lower.tri(Y2,diag=TRUE)*Y2
	N <- matrix(event,byrow=TRUE,ncol=howmany,nrow=howmany)
	Ndiag <- diag(diag(N))
	N <- N - Ndiag
	
	N1 <- matrix(d1,byrow=TRUE,ncol=howmany,nrow=howmany)
	Ndiag1 <- diag(diag(N1))
	N1 <- N1 - Ndiag1
	
	cum1 <- N1/Y
	
	N2 <- matrix(d2,byrow=TRUE,ncol=howmany,nrow=howmany)
	Ndiag2 <- diag(diag(N2))
	N2 <- N2 - Ndiag2
	
	cum2 <- N2/Y
	
	kmji <- (Y-N)/Y
		
	km <- t(apply(kmji,1,cumprod))
	

	#corrected value for the last time - last value carried forward 
	aje <- which(is.na(km[howmany,]))
	if(length(aje)>0){
		kir <- min(aje)
		km[howmany,kir:ncol(km)] <- km[howmany,kir-1] 
	}
	
	km <- cbind(rep(1,nrow(km)),km[,-ncol(km)])
	
	C1 <- t(apply(cum1*km,1,cumsum))
	C2 <- t(apply(cum2*km,1,cumsum))
	
	#only for deaths, one value per tie
	C1 <- C1[,event==1]
	C1 <- C1[,lt]
	C2 <- C2[,event==1]
	C2 <- C2[,lt]
	list(td=unique(td),C1=C1,C2=C2)	
}

