\name{mean2pop.s}
\alias{mean2pop.s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian hipotesis rata-rata 2 populasi (varians tidak diketahui, var1=var2)}
\description{Pengujian rata-rata 2 populasi dengan kondisi: varians kedua populasi tidak diketahui, varians kedua populasi diketahui besarnya sama.}
\usage{
mean2pop.s(data1, data2, dif, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{data sampel 1}
  \item{data2}{data sampel 2}
  \item{dif}{nilai hipotesis (H0: miu1-miu2=diff}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji normal}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
  mean2pop.s(data1, data2, 0, 0.05)
}
