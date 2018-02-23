\name{varians1pop}
\alias{varians1pop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian hipotesis varians 1 populasi}
\description{Pengujian hipotesis varians 1 populasi untuk 3 hipotesis}
\usage{
varians1pop(data, var.pop, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{sampel data}
  \item{var.pop}{varians populasi}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji berdistribusi peluang Chi-Square}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
varians1pop(data, 60, 0.05)
}
