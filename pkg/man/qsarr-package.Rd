\name{qsarr-package}
\alias{qsarr-package}
\alias{qsarr}
\docType{package}
\title{
qsaR in R - A flexible framework for working with QSAR and QSPR in R
}
\description{
Quantitative Structure Activity Relationships (QSARs) are mathematical models that are used to predict measures of biological response from physical characteristics of the structure of chemicals (known as molecular descriptors).
The package it’s goanna contain tools for:
data splitting
pre-processing 
model tuning using resampling
variable importance estimation
Automated results report

as well as other functionality.

The ultimate goal of qsaR is to develop a package that can make quantitative structure-activity relationship or related models easily and automatically in R. qsaR incorporates information and tools from various sources into a logical workflow, independent which underlying programs are used do generate the descriptors.

}
\details{
\tabular{ll}{
Package: \tab qsarr\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-11-20\cr
License: \tab BSD\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(cox2)
import(data,cox2IC50,cox2Descr)
}
