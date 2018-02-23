\name{mean2sample.sigma}
\alias{mean2sample.sigma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian rata-rata 2 populasi sampel kecil}
\description{Pengujian hipotesis rata-rata 2 populasi degan sampel berukuran kecil, dengan asumsi varians populasi untuk kedua populasi diketahui}
\usage{
mean2sample.sigma(data1, data2, dif, sigma1, sigma2, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{sampel populasi 1}
  \item{data2}{sampel populasi 2}
  \item{dif}{nilai hipotesis (H0: mu1-miu2=dif)}
  \item{sigma1}{varians populasi 1}
  \item{sigma2}{varians populasi2}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji berdistribusi peluang normal}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
mean2sample.sigma(data1, data2, 0, 60, 50, 0.05)
}
