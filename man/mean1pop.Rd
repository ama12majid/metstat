\name{mean1pop}
\alias{mean1pop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Uji Rata-Rata 1 Populasi dengan Sampel Besar}
%%  ~~function to do ... ~~
\description{Pengujian hitpotesis rata-rata dengan banyaknya populasi adalah 1 dan minimum sampel adalah 30
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mean1pop(data1, mean.pop, var.pop, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{Data sampel}
  \item{mean.pop}{Rata-rata hipotesis (H0: mu=mean.pop)}
  \item{var.pop}{Varians populasi}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji normal}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
mean1pop(data, 50, 100, 0.05)
}
