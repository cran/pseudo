\name{pseudoci}
\alias{pseudoci}
\alias{ci.omit}
\alias{ci.tot}

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
the status indicator: 0=alive, 1=dead (risk 1), 2= dead (risk 2).}
\item{tmax}{
a vector of time points at which the pseudo-observations are to be computed. If missing, the pseudo-observations are
reported at each event time.
}
}

\details{
The function calculates the pseudo-observations for the cumulative incidence function for each individual and each risk at all the required time points. 
The pseudo-observations can be used for fitting a regression model with a generalized estimating equation.
}

\value{
A data frame. The first two columns contain the follow up time and the status indicator as given by the user. The following
columns present the pseudo-observations for each of the risks at all the required (sorted) time points.
}

\examples{
library(KMsurv)
data(bmt)

#calculate the pseudo-observations
cutoffs <- c(50,105,170,280,530)
bmt$icr <- bmt$d1 +  bmt$d3
pseudo <- pseudoci(time=bmt$t2,event=bmt$icr,tmax=cutoffs)

#rearrange the data, use only pseudo-observations for relapse
rel_mask <- c(-1,50,-1,105,-1,170,-1,280,-1,530)
b <- NULL
for(j in 3:ncol(pseudo)){
	b <- rbind(b,cbind(bmt,pseudo = pseudo[,j],
	     tpseudo = rel_mask[j-2],id=1:nrow(bmt)))
}
b <- b[order(b$id),]
b <- b[b$tpseudo != -1,]

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
Klein J.P., Gerster M., Andersen P.K., Tarima S.: "SAS and R Functions to Compute Pseudo-values for Censored Data Regression." \emph{Department of Biostatistics, University of Copenhagen,} research report 07/11.
}



\seealso{
\code{\link{pseudomean}},
\code{\link{pseudosurv}}
}

\keyword{survival}
