\name{visualize-package}
\alias{visualize-package}
\alias{visualize}
\docType{package}
\title{
Graph Probability Distributions with User Supplied Parameters and Stats.
}
\description{
Graphs the pdf or pmf and highlights what area or probability is present in user defined locations. Visualize is able to provide lower tail, bounded, two tail, and upper tail calculations. Supports strict and equal to inequalities.  Also provided on the graph is the mean and variance of the distribution.
}
\details{
\tabular{ll}{
Package: \tab visualize\cr
Type: \tab Package\cr
Version: \tab 4.0\cr
Date: \tab 2013-05-20\cr
License: \tab MIT License\cr
}
}
\author{
James Balamuta

Maintainer: James Balamuta <james.balamuta@gmail.com>
}
\note{
Update Notes: \cr

v4.0 - "Seeing is Split" \cr
* New feature: Two tail evaluations for both continuous and discrete distributions! \cr
* New feature: Highlighted shading in continuous distributions! \cr
* New feature: Start and End Indicators! \cr
* Documentation update \cr

v3.0 - "Seeing is Strict" \cr
* New feature: Evaluate probabilities with strict inequalities! \cr
* Rewrote sections to increase efficiency and reliability. \cr

v2.2 - Hotfix - "Seeing is Correct" \cr
* Corrects rendering issue with supplied stat not meeting certain criteria. \cr

v2.1 - Hotfix - "Seeing is Off" \cr 
* Corrects minor graphing labels. \cr
* Revised error handling.\cr

v2.0 - "Seeing is Believing"  \cr
* New Continuous Distributions: Cauchy, F, Log Normal, Logistic, T, and Wilcox. \cr
* New feature: Bar graph rendering for discrete probability distributions. \cr
* Rewritten Distribution Wrapper\cr

v1.0 - "Seeing is Magic"  \cr
* Supported Continuous Distributions: Chi-squared, beta, gamma, exponential, uniform, and normal. \cr
* Supported Discrete Distributions: Geometric, binomial, poisson, hypergeometric, and negative binomial. \cr
* Graph the PDF or PMF of a probability function and determine the probability enclosed. \cr
* See characteristics of the distribution requested (Parameters, Mean, and Variance). 
}
\keyword{ package }
\examples{
##visualize.it acts as the general wrapper.
##For guided application of visualize, see the visualize.distr_name list.
#Binomial distribution evaluated at lower tail.
visualize.it(dist = 'binom', stat = 2, 
params = list(size = 4,prob = .5),
 section ="lower", strict = TRUE)
visualize.binom(stat = 2, size = 4, prob =.5, section ="lower", strict = TRUE)

#Set to shade inbetween a bounded region.
visualize.it(dist = 'norm', stat = c(-1,1), list(mu=0,sd=1), section="bounded")
visualize.norm(stat = c(-1,1), mu = 0,sd = 1, section ="bounded")

#Gamma distribution evaluated at upper tail.
visualize.it(dist = 'gamma', stat = 2, params = list(alpha = 2, theta = 1),section="upper")
visualize.gamma(stat = 2, alpha = 2, theta = 1, section="upper")

}
