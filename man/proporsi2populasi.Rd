\name{proporsi2populasi}
\alias{proporsi2populasi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian proporsi 2 populasi}
\description{Pengujian hipotesis proporsi untuk 2 populasi berdistribusi normal}
\usage{
proporsi2populasi(data1, data2, dif, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{sampel biner populasi 1}
  \item{data2}{sampel biner populasi 2}
  \item{dif}{nilai hipotesis (H0: p1-p2=dif)}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji normal}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
proporsi2populasi(data1, data2, 0, 0.05)
}
