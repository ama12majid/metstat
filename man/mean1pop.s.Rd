\name{mean1pop.s}
\alias{mean1pop.s}
\title{Uji Rata-Rata 1 Populasi dengan Sampel Kecil}
\description{Pengujian hitpotesis rata-rata dengan banyaknya populasi adalah 1 dan sampel berukuran kurang dari 30 sampel.}
\usage{mean1pop(data, mean.pop, alpha)}
\arguments{
  \item{data1}{Data sampel}
  \item{mean.pop}{Rata-rata hipotesis (H0: mu=mean.pop)}
  \item{alpha}{tingkat signifikansi}
}
\details{
Pengujian menggunakan statistik uji t.
}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{mean1pop(data, 50, 100, 0.05)}
