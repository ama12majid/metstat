\name{mean2pop}
\alias{mean2pop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian Rata-Rata 2 Populasi (Varians diketahui)}
\description{Pengujian hipotesis rata-rata untuk 2 populasi, di mana varians masing-masing populasi diketahui.}
\usage{
mean2pop(data1, data2, dif, var.pop1, var.pop2, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{data untuk sampel 1}
  \item{data2}{data untuk sampel 2}
  \item{dif}{nilai hipotesis (H0: miu1-miu2=diff)}
  \item{var.pop1}{varians populasi 1}
  \item{var.pop2}{varians populasi 2}
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
mean2pop(data1, data2, 0, 50, 60, 0.05)
}
