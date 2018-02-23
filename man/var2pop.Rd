\name{var2pop}
\alias{var2pop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian hipotesis varians 2 populasi}
\description{Pengujian nilai kesamaan varians untuk 2 populasi}
\usage{
var2pop(data1, data2, sigma1, sigma2, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{sampel populasi 1}
  \item{data2}{sampel populasi 2}
  \item{sigma1}{varians populasi 1}
  \item{sigma2}{varians populasi 2}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji berdistribusi peluang F}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
var2pop(data1, data2, 50, 60, 0.05)
}
