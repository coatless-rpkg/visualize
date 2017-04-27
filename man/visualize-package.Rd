\name{visualize-package}
\alias{visualize-package}
\alias{visualize}
\docType{package}
\title{
Graphs the pdf or pmf and highlights what area or probability is present in defined locations.
}
\description{
Graphs the pdf or pmf and highlights what area or probability is present in defined locations. Supports lower, bounded, and upper calculations. Also provided on the graph is the mean and variance of the distribution. 
}
\details{
\tabular{ll}{
Package: \tab visualize\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-12-21\cr
License: \tab MIT License\cr
}

}
\author{
James Balamuta

Maintainer: James Balamuta <james.balamuta@gmail.com>
}

\keyword{ package }
\examples{
#Binomial distribution evaluated at lower tail.
visualize.it('binom', stat = 2, params = list(n=4,p=.5), section ="lower")

#Set to shade inbetween a bounded region.
visualize.it(dist = 'norm', stat = c(-1,1), list(mu=0,sd=1), section="bounded")

#Gamma distribution evaluated at upper tail.
visualize.it(dist = 'gamma', stat = 2, params = list(alpha=2,beta=1),section="upper")

}
