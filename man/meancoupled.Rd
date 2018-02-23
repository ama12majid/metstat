\name{meancoupled}
\alias{meancoupled}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian hipotesis rata-rata 2 populasi berpasangan}
\description{Pengujian hipotesis rata-rata untuk 2 populasi yang saling berpasangan}
\usage{
meancoupled(data1, data2, dif, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{sampel populasi 1}
  \item{data2}{sampel populasi 2}
  \item{dif}{nilai hipotesis (H0: beda rata-rata=dif)}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji berdistribusi peluang t}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
meancoupled(data1, data2, 0, 0.05)
}
