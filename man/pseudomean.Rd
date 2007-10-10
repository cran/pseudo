\name{pseudomean}
\alias{pseudomean}

\title{Pseudo-observations for the restricted mean}
\description{Computes pseudo-observations for modeling survival function based on the restricted mean.
}
\usage{
pseudomean(time,event, tmax)
}
\arguments{
\item{time}{the follow up time.
}
\item{event}{
the status indicator: 0=alive, 1=dead.}
\item{tmax}{
the maximum cut-off point for the restricted mean. If missing or larger than the maximum follow up time, it is
replaced by the maximum follow up time. 
}
}

\details{
The function calculates the pseudo-observations for the restricted mean survival for each individual at prespecified time-points.
The pseudo-observations can be used for fitting a regression model with a generalized estimating equation.
}

\value{
A data frame. The first two columns contain the follow up time and the status indicator as given by the user. The third
column presents the computed pseudo-observations.
}


\examples{
library(KMsurv)
data(bmt)

#compute the pseudo-observations:
pseudo = pseudomean(time=bmt$t2, event=bmt$d3,tmax=2000)

#arrange the data
a <- cbind(bmt,pseudo = pseudo$psumean,id=1:nrow(bmt))

#fit a regression model for the mean time

library(geepack)
summary(fit <- geese(pseudo ~ z1 + as.factor(z8) + as.factor(group),
	data = a, id=id, jack = TRUE, family=gaussian, 
	corstr="independence", scale.fix=FALSE))


#rearrange the output
round(cbind(mean = fit$beta,SD = sqrt(diag(fit$vbeta.ajs)),
	Z = fit$beta/sqrt(diag(fit$vbeta.ajs)),	PVal =
	2-2*pnorm(abs(fit$beta/sqrt(diag(fit$vbeta.ajs))))),4)
}


\references{
Klein J.P., Gerster M., Andersen P.K., Tarima S.: "SAS and R Functions to Compute Pseudo-values for Censored Data Regression." \emph{Department of Biostatistics, University of Copenhagen,} research report 07/11.
}




\seealso{
\code{\link{pseudosurv}},
\code{\link{pseudoci}}
}

\keyword{survival}
