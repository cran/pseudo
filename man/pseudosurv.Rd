\name{pseudosurv}
\alias{pseudosurv}
\alias{surv.tot}
\alias{surv.omit}

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
}

\value{
A data frame. The first two columns contain the follow up time and the status indicator as given by the user. The following
columns present the pseudo-observations for each of the required (sorted) time points.
}

\examples{
library(KMsurv)
data(bmt)

#calculate the pseudo-observations
cutoffs <- c(50,105,170,280,530)
pseudo <- pseudosurv(time=bmt$t2,event=bmt$d3,tmax=cutoffs)

#rearrange the data
b <- NULL
for(j in 3:ncol(pseudo)){
	b <- rbind(b,cbind(bmt,pseudo=pseudo[,j],tpseudo=cutoffs[j-2],
	     id=1:nrow(bmt)))
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
Klein J.P., Gerster M., Andersen P.K., Tarima S.: "SAS and R Functions to Compute Pseudo-values for Censored Data Regression." \emph{Department of Biostatistics, University of Copenhagen,} research report 07/11.
}



\seealso{
\code{\link{pseudomean}},
\code{\link{pseudoci}}
}

\keyword{survival}
