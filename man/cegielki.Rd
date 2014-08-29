\name{cegielki}
\alias{cegielki}
\alias{bricks}
\alias{lessThanCurve}
\alias{mniejszywa}
\title{Density plot and ecdf plot}
\usage{
cegielki(dane, tytul =  deparse(substitute(dane)), 
  osx = c(19,35), osy=c(0,4))

bricks(dane, tytul =  deparse(substitute(dane)), 
  osx = c(19,35), osy=c(0,4))

mniejszywa(dane, tytul =  deparse(substitute(dane)), 
  osx = c(19,35), osy=c(0,4))

lessThanCurve(dane, tytul =  deparse(substitute(dane)), 
  osx = c(19,35), osy=c(0,4))
}
\arguments{
\item{dane}{Numeric verctor od datapoints to be presented}
\item{tytul}{Title of the plot}
\item{osx}{Limits for x axis}
\item{osy}{Limits for y axis}
}
\value{
Plot with density of ecdf of the vector 'dane'.
}
\description{
Plot with density of ecdf of the vector 'dane'.
}
