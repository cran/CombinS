\name{CombS}
\alias{CombS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Combinatory Method (s) for the construction of rectangular PBIB designs
}
\description{
The application of the Combinatory Method (s), with s chosen in [2, l-1], on rectangular association scheme to obtain the configuration and the parameters of the PBIB design associated.
}
\usage{
CombS(n, l, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of lines of the association schemes array
}
  \item{l}{
Number of columns of the association schemes array
}
  \item{s}{
Number of the token treatments from the same row of the association scheme
}
}
\details{
For 2 < s < l, we obtain a rectangular PBIB design.

For s = l, we obtain a singular group divisible designs.}
\value{
A LIST :
\item{PBIB }{The configuration of the PBIB.}
\item{Type }{The type of the design}
\item{V }{Number of treatments.}
\item{B }{Number of blocs.}
\item{R }{Repetition of each treatment.}
\item{K }{Size of blocs.}
\item{lamda }{Vector of m-lambda.}
\item{Resolvable }{Is the design Resolvable ?}
}
\references{
Imane Rezgui, Z. Gheribi-Aoulmi (2014). New construction method of rectangular partially balanced incomplete block designs and singular group divisible designs, Journal of Mathematics and Statistics, 10, 45- 48. http://www.thescipub.com/jmss.toc 

M.N. Vartak 1955. On an application of Kronecker product of Matrices to Statistical designs. Ann. Math. Stat.,26(420-438).
}
\author{
Mohamed Laib, Imane Rezgui, Zebida Gheribi-Aoulmi and Herve Monod
}
\note{
Mohamed Laib <laib.med@gmail.com>
}


\examples{
n<-3
l<-3
s<-2
CombS(l,n,s)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line