\name{GPBIB4A}
\alias{GPBIB4A}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Combinatory Method (s) for the construction of generalized rectangular right angular (4) design with lambda4 = 0
}
\description{
The function GPBIB4A gives the configuration and the parametres of the design obtained by the first construction method of GPBIB_4 (see 3.1.1 of the paper rezgui et al (2015)).
}
\usage{
GPBIB4A(n, l, s, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of lines of the association schemes array.
}
  \item{l}{
Number of columns of the association schemes array.
}
  \item{s}{
Number of the token treatments from the same row of the association scheme.
}
  \item{w}{
Number of the association scheme arrays.
}
}
\details{
For s = l, the previous method gives configuration of nested group divisible designs.
}
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
Imane Rezgui, Z. Gheribi-Aoulmi and H. Monod (2015). U-type Designs via New Generalized Partially Balanced Incomplete Block Designs with m = 4, 5 and 7 Associated Classes, Applied mathematics, 6, 242-264. http://dx.doi.org/10.4236/am.2015.62024
}
\author{
Mohamed Laib, Imane Rezgui, Zebida Gheribi-Aoulmi and Herve Monod.
}
\note{
For w=2, the GPBIB_4 is a rectangular right angular (4) (PBIB_4) .
}


\seealso{
Imane Rezgui, Z.Gheribi-Aoulmi and H. Monod, New association schemes with 4, 5 and 7 associated classes and their associated partially balanced incomplete block designs; Advances and Applications in Discrete Mathematics Vol.12 Issue 2 197-206.
}
\examples{
n<-3
l<-3
s<-3
w<-3
GPBIB4A(n, l, s, w)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
