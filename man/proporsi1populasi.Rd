\name{proporsi1populasi}
\alias{proporsi1populasi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pengujian hipotesis proporsi 1 populasi}
\description{Pengujian hipotesis proporsi 1 populasi dengan syarat: data yang digunkan bersifat biner (1=berhasil dan 0=gagal), dan minimal sampel sebanyak 30.}
\usage{
proporsi1populasi(data, p0, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data biner sampel populasi}
  \item{p0}{nilai hipotesis (H0: p=p0)}
  \item{alpha}{tingkat signifikansi}
}
\details{Pengujian menggunakan statistik uji normal}
\references{Walpole}
\author{Muhammad Abdul Majid}
\seealso{http://letme-escape.blogspot.co.id}
\examples{
proporsi1populasi(data, 0.25, 0.05)
}
