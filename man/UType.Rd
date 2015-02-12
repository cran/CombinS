\name{UType}
\alias{UType}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
U-type design via some PBIB designs
}
\description{
We apply the Fang algorithm on our constructed designs to obtain the configuration and the parameters of the U-type design associated.
}
\usage{
UType(lst)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lst}{
The output of one of our package functions.
}
}

\value{
A LIST :
\item{v }{Number of runs.}
\item{r}{Number of factors.}
\item{UtypeDesign }{The configuration of the U-type design.}
}
\references{

K.T. Fang, R.Li and A.Sudjanto (2006). Design ans Modeling for Computer Experiments. Taylor & Francis Group, LLC London.
}
\author{
Mohamed Laib, Imane Rezgui, Zebida Gheribi-Aoulmi and Herve Monod.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
M<-GPBIB4A(4,4,2,2)
UType(M)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
