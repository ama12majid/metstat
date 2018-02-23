\name{mean2sample.s}
\alias{mean2sample.s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian rata-rata 2 populasi sampel kecil}
\description{Pengujian hipotesis rata-rata 2 populasi degan sampel berukuran kecil, dengan asumsi varians populasi untuk kedua populasi berbeda}
\usage{
mean2sample.s(data1, data2, dif, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{data sampel 1}
  \item{data2}{data sampel 2}
  \item{dif}{nilai hipotesis (H0: mu1-miu2=dif)}
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
mean2sample.s(data1, data2, 0, 0.05)
}
