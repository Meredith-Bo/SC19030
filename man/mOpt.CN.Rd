% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mOpt.CN.R
\name{mOpt.CN}
\alias{mOpt.CN}
\title{Finding the optimal numerber of Beta distribution mixture}
\usage{
mOpt.CN(tData, L)
}
\arguments{
\item{L}{the maximum of the estimation region (matrix)}

\item{tdata}{A transformation original data (vector)}
}
\value{
number of optimal weights
}
\description{
A function to calculate the optimal number of weights
}
\examples{
\dontrun{
data(windspeed)
tdata<-windspeed$H10
n <- length(tdata)
Fn <- ecdf(tdata)
ep <- 3/(8*length(data))	
ecdf.vec <- Fn(tdata)	
L <- diag( n / ( ( ecdf.vec + ep)*(1 + ep - ecdf.vec ) ) )
mOpt.CN(tdata,L)
}
}
