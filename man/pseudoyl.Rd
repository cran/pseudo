\name{pseudoyl}
\alias{pseudoyl}

\title{Pseudo-observations for the expected  number of years lost}
\description{Computes pseudo-observations for modeling using the number of years lost.
}
\usage{
pseudoyl(time,event, tmax)
}
\arguments{
\item{time}{the follow up time.
}
\item{event}{
the cause indicator, use 0 as censoring code and integers to name the other causes.}
\item{tmax}{
the maximum cut-off point time = the upper limit of the integral of the cumulative incidence function. If missing or larger than the maximum follow up time, it is
replaced by the maximum follow up time. 
}
}

\details{
The function calculates the pseudo-observations for the expected number of years lost for each individual.
The pseudo-observations can be used for fitting a regression model with a generalized estimating equation.
No missing values in either \code{time} or \code{event}
vector are allowed.
}


\value{
A list containing the following objects:
\item{cause}{The ordered codes for different causes.}
\item{pseudo}{A list of vectors- a vector for each of the causes, ordered by codes. Each value of a vector belongs to one individual (ordered as in the original data set).}
}




\examples{
library(KMsurv)
data(bmt)
bmt$icr <- bmt$d1 +  bmt$d3


#compute the pseudo-observations:
pseudo = pseudoyl(time=bmt$t2, event=bmt$icr,tmax=2000)

#arrange the data - use pseudo observations for cause 2
a <- cbind(bmt,pseudo = pseudo$pseudo[[2]],id=1:nrow(bmt))

#fit a regression model for cause 2

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
Andersen P.K.: "A note on the decomposition of number of life years lost according to causes of death." \emph{Research report, Department of Biostatistics, University of Copenhagen}, 2012 (2)
}




\seealso{
\code{\link{pseudoci}},
\code{\link{pseudomean}},
\code{\link{pseudosurv}}
}

\keyword{survival}
