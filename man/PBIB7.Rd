\name{PBIB7}
\alias{PBIB7}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construction method of rectangular right angular PBIB(7) design.
}
\description{
Construction of   PBIB designs based on rectangular right angular association scheme (7).
}
\usage{
PBIB7(n, l, S, ty="a")
}

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
\item{ty}{
The method used to obtain design (there are two methods "a" & "b").
}
}
\details{
Method "b" : The lambda are distinct. For more details see. (Imane Rezgui et al. New association schemes with 4, 5 and 7 associated classes and their associated partially balanced incomplete block designs.)
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
\author{
Mohamed LAIB, Imane Rezgui, Zoubida Gheribi-Aoulmi and Herve Monod
}

\examples{
n<-3
l<-4
S<-3
PBIB7(n,l,S,ty="a")
#Or
PBIB7(n,l,S,ty="b")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
