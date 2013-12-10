\name{CombS}
\alias{CombS}

\title{
The combinatory method(s) for constructing partially balanced incomplete block designs
}
\description{
Construction of partially balanced incomplete block designs associated with rectangular right anglular association schemes (m) and we derive their parameters.
}
\usage{
CombS(n, l, S)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of lines in the association schemes.
}
  \item{l}{
Number of colons in the association schemes.
}
  \item{S}{
Number of treatments token from the same row of the association scheme.
}
}
\details{
For 2 < S < l associated with a rectangular association scheme, allow to obtain a rectangular PBIB design.

For S = l, the combinatory method (s) can also be used for the construction of singular group divisible designs.
}
\value{
A LIST :
\item{PBIB }{The configuration of the PBIB.}
\item{Type }{The type of the design}
\item{V }{Number of treatments in the PBIB.}
\item{B }{Number of blocs.}
\item{R }{Repetition of each treatment.}
\item{K }{Size of each blocks.}
\item{lamda }{Vector of m-lambda.}
}
\references{
Imane Rezgui, Z.Gheribi-Aoulmi and H. Monod, New association schemes with 4, 5 and 7 associated classes and their associated partially balanced incomplete block designs; Advances and Applications in Discrete Mathematics Vol.12 Issue 2 (to appear).
}
\author{Mohamed LAIB, Imane Rezgui, Zoubida Gheribi-Aoulmi and Herve Monod}
\examples{
n<-3
l<-4
#Rectangular PBIB designs :
S<-3
CombS(n,l,S)
#Singular group divisible designs :
S<-4
CombS(n,l,S)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
