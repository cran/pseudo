"ci.tot" <-
function(pseudo){
	#calculate cum. inc. function, all cases
	
	howmany <- nrow(pseudo)
	
	d1 <- as.numeric(pseudo$event==1)
	d2 <- as.numeric(pseudo$event==2)
	event <- as.numeric(pseudo$event>0)
	
	td <- pseudo$time[event==1]
	lt.temp <- c(td[-1],td[length(td)]+1)
	lt <- which(td!=lt.temp)
	
	#km - i
	Y <- howmany:1
	N <- event
	N1 <- d1
	N2 <- d2
	cum1 <- N1/Y
	
	cum2 <- N2/Y
	
	kmji <- (Y-N)/Y
		
	km <- cumprod(kmji)
	
	km <- c(1,km[-length(km)])
	
	C1 <- cumsum(cum1*km)
	C2 <- cumsum(cum2*km)
	
	#only for deaths, one value per tie
	C1 <- C1[event==1]
	C1 <- C1[lt]
	C2 <- C2[event==1]
	C2 <- C2[lt]
	rbind(C1,C2)
}

