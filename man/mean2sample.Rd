\name{mean2sample}
\alias{mean2sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian rata-rata 2 populasi, dengan jumlah sampel kecil, varians sama}
\description{Pengujian rata-rata 2 populasi dengan jumlah sampel kecil dan asumsi varians populasi sama untuk kedua populasi.}
\usage{
mean2sample(data1, data2, dif, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{sampel data 1}
  \item{data2}{sampel data 2}
  \item{dif}{nilai hipotesis (H0: miu1-miu2=dif)}
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
mean2sample(data1, data2, 0, 0.05)
}
