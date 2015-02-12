\name{CombinS-package}
\alias{CombinS-package}
\alias{CombinS}
\docType{package}
\title{
Construction Methods of some Series of PBIB Designs.
}
\description{
In this package, we apply the Combinatory Method (s) on some association scheme with m=2,3,4,5 and 7 associated classes to obtain the configuration of the Partially Balanced Incomplete Bloc (PBIB) design based on these association schemes. Furthermore, we apply the Fang algorithm on these designs to obtain their associated U-type designs.
}
\details{
\tabular{ll}{
Package: \tab CombinS\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2015-02-12\cr
License: \tab  GPL-3\cr
}
}

\note{
This version is a generalisation for v=wnl treatments of the previous one; In the version 1.0 we used rectangular right angular (m) association schemes with (v=2nl) and m=4,5 and 7 associated classes.
 
The Association schemes used in this R-package are :

Rectangular association scheme;

Generalized rectangular right angular association scheme (4);

Generalized rectangular right angular association scheme (5);

Generalized rectangular right angular association scheme (7).
}
\author{
Mohamed Laib, Imane Rezgui, Zebida Gheribi-Aoulmi and Herve Monod.

Maintainer: Mohamed Laib <laib.med@gmail.com>

}
\references{
Imane Rezgui, Z. Gheribi-Aoulmi and H. Monod (2015). U-type Designs via New Generalized Partially Balanced Incomplete Block Designs with m = 4, 5 and 7 Associated Classes, Applied mathematics, 6, 242-264. http://dx.doi.org/10.4236/am.2015.62024

Imane Rezgui, Z. Gheribi-Aoulmi (2014). New construction method of rectangular partially balanced incomplete block designs and singular group divisible designs, Journal of Mathematics and Statistics, 10, 45- 48. http://www.thescipub.com/jmss.toc 

M.N. Vartak 1955. On an application of Kronecker product of Matrices to Statistical designs. Ann. Math. Stat.,26(420-438).

K.T. Fang, R.Li and A.Sudjanto (2006). Design ans Modeling for Computer Experiments. Taylor & Francis Group, LLC London.
}


\examples{
n<-3
l<-3
s<-2
RECT<-CombS(l,n,s)
UType(RECT)
}