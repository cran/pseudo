\name{pseudosurv}
\alias{pseudosurv}

\title{Pseudo-observations for the Kaplan-Meier estimate}
\description{Computes pseudo-observations for modeling survival function based on the Kaplan-Meier estimator.
}
\usage{
pseudosurv(time,event, tmax)
}
\arguments{
\item{time}{the follow up time.
}
\item{event}{
the status indicator: 0=alive, 1=dead.}
\item{tmax}{
a vector of time points at which the pseudo-observations are to be computed. If missing, the pseudo-observations are
reported at each event time.
}
}

\details{
The function calculates the pseudo-observations for the value of the survival function at prespecified time-points for each individual.
The pseudo-observations can be used for fitting a regression model with a generalized estimating equation.
No missing values in either \code{time} or \code{event}
vector are allowed.

Please note that the output of the function has changed and the usage is thus no longer the same as in the reference paper - the new usage is described in the example below.
Similar (faster) version of the function is available in the R-package prodlim (\code{jackknife}).
}

\value{
A list containing the following objects:
\item{time}{The ordered time points at which the pseudo-observations are evaluated.}
\item{pseudo}{A matrix. Each row belongs to one individual (ordered as in the original data set),
 each column presents a time point (ordered in time). }
}

\examples{
library(KMsurv)
data(bmt)

#calculate the pseudo-observations
cutoffs <- c(50,105,170,280,530)
pseudo <- pseudosurv(time=bmt$t2,event=bmt$d3,tmax=cutoffs)


#rearrange the data into a long data set
b <- NULL
for(it in 1:length(pseudo$time)){
	b <- rbind(b,cbind(bmt,pseudo = pseudo$pseudo[,it],
	     tpseudo = pseudo$time[it],id=1:nrow(bmt)))
}
b <- b[order(b$id),]



#fit a Cox model using GEE
library(geepack)
summary(fit <- geese(pseudo~as.factor(tpseudo)+as.factor(group)+
        as.factor(z8)+z1,data=b,scale.fix=TRUE,family=gaussian,
        jack=TRUE, mean.link="cloglog",corstr="independence"))

#The results using the AJ variance estimate
round(cbind(mean = fit$beta,SD = sqrt(diag(fit$vbeta.ajs)),
	Z = fit$beta/sqrt(diag(fit$vbeta.ajs)),	PVal =
	2-2*pnorm(abs(fit$beta/sqrt(diag(fit$vbeta.ajs))))),4)
}

\references{
Klein J.P., Gerster M., Andersen P.K., Tarima S., POHAR PERME, M.: "SAS and R Functions to Compute Pseudo-values for Censored Data Regression." \emph{Comput. methods programs biomed.}, 2008, 89 (3): 289-300 
}


\seealso{
\code{\link{pseudomean}},
\code{\link{pseudoci}},
\code{\link{pseudoyl}}
}

\keyword{survival}
