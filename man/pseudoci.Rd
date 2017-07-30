\name{pseudoci}
\alias{pseudoci}


\title{Pseudo-observations for the cumulative incidence function}
\description{Computes pseudo-observations for modeling competing risks based on the cumulative incidence function.
}
\usage{
pseudoci(time,event, tmax)
}
\arguments{
\item{time}{the follow up time.
}
\item{event}{
the cause indicator, use 0 as censoring code and integers to name the other causes.}
\item{tmax}{
a vector of time points at which the pseudo-observations are to be computed. If missing, the pseudo-observations are
reported at each event time.
}
}

\details{
The function calculates the pseudo-observations for the cumulative incidence function for each individual and each risk at all the required time points. 
The pseudo-observations can be used for fitting a regression model with a generalized estimating equation. No missing values in either \code{time} or \code{event}
vector are allowed.

Please note that the output of the function has changed and the usage is thus no longer the same as in the reference paper - the new usage is described in the example below.
Similar (faster) version of the function is available in the R-package prodlim (\code{jackknife}).
}


\value{
A list containing the following objects:
\item{time}{The ordered time points at which the pseudo-observations are evaluated.}
\item{cause}{The ordered codes for different causes.}
\item{pseudo}{A list of matrices - a matrix for each of the causes, ordered by codes. Each row of a matrix belongs to one individual (ordered as in the original data set),
 each column presents a time point (ordered in time). }
}


\examples{
library(KMsurv)
data(bmt)

#calculate the pseudo-observations
cutoffs <- c(50,105,170,280,530)
bmt$icr <- bmt$d1 +  bmt$d3
pseudo <- pseudoci(time=bmt$t2,event=bmt$icr,tmax=cutoffs)

#rearrange the data into a long data set, use only pseudo-observations for relapse (icr=2)
b <- NULL
for(it in 1:length(pseudo$time)){
	b <- rbind(b,cbind(bmt,pseudo = pseudo$pseudo[[2]][,it],
	     tpseudo = pseudo$time[it],id=1:nrow(bmt)))
}
b <- b[order(b$id),]


# fit the model
library(geepack)
fit <- geese(pseudo ~ as.factor(tpseudo) + as.factor(group) + as.factor(z8) +
	z1 - 1, data =b, id=id, jack = TRUE, scale.fix=TRUE, family=gaussian,
	mean.link = "cloglog", corstr="independence")

#The results using the AJ variance estimate
cbind(mean = round(fit$beta,4), SD = round(sqrt(diag(fit$vbeta.ajs)),4),
	Z = round(fit$beta/sqrt(diag(fit$vbeta.ajs)),4),
	PVal = round(2-2*pnorm(abs(fit$beta/sqrt(diag(fit$vbeta.ajs)))),4))

}

\references{
Klein J.P., Gerster M., Andersen P.K., Tarima S., POHAR PERME, M.: "SAS and R Functions to Compute Pseudo-values for Censored Data Regression." \emph{Comput. methods programs biomed.}, 2008, 89 (3): 289-300 
}



\seealso{
\code{\link{pseudoyl}},
\code{\link{pseudomean}},
\code{\link{pseudosurv}}
}

\keyword{survival}
