\name{convertSingleParam}
\alias{convertSingleParam}

\title{Provides estimators that allows list of parameters}
\description{Converts estimators allowing single parameters to estimators allowing a list of parameters. The resulting function can be passed to the argument \code{estimator} in the cross-validation functions, see \cite{See Also}.
}

\usage{
convertSingleParam(estimator)
}

\arguments{
\item{estimator}{the function to be converted, i.e. a function providing a local estimate. The function must have the arguments \code{Y}, \code{param} and \code{...}, where \code{Y} will be the observations, and \code{param} a single parameter of arbitrary type. Hence \code{\link[=list]{lists}} can be used when multiple parameter of different types are needed. It has to return either a vector with the estimated change-points or a list containing the named entries \code{cps} and \code{value}. In this case \code{cps} has to be a numeric vector with the estimated change-points as before and \code{value} has to be a list of length one entry longer than \code{cps} giving the locally estimated values. An example is given below.}
}

\value{
a function that can be passed to the argument \code{estimator} in the cross-validation functions, see the functions listed in \cite{See Also}
}

\seealso{
\code{\link{crossvalidationCP}}, \code{\link{VfoldCV}}, \code{\link{COPPS}}, \code{\link{CV1}}, \code{\link{CVmod}}
}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. In preparation.
}

\examples{
# wrapper around pelt to demonstrate an estimator that allows a single parameter only
singleParamEstimator <- function(Y, param, minseglen = 1, ...) {
  if (is.numeric(param)) {
    ret <- changepoint::cpt.mean(data = Y, penalty = "Manual", pen.value = param, method = "PELT",
                                 minseglen = minseglen)
  } else {
    ret <- changepoint::cpt.mean(data = Y, penalty = param, method = "PELT", minseglen = minseglen)
  }
  
  list(cps = ret@cpts[-length(ret@cpts)], value = as.list(ret@param.est$mean))
}
# conversion to an estimator that is suitable for crossvalidationCP() etc.
estimatorMultiParam <- convertSingleParam(singleParamEstimator)
crossvalidationCP(rnorm(100), estimator = estimatorMultiParam, param = list("SIC", "MBIC"))
}

\keyword{nonparametric}
