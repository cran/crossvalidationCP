\name{COPPS}
\alias{COPPS}
\alias{CV1}
\alias{CVmod}

\title{Cross-validation with Order-Preserved Sample-Splitting}
\description{Tuning parameters are selected by a generalised \acronym{COPPS} procedure. All functions use Order-Preserved Sample-Splitting, meaning that the folds will be the odd and even indexed observations. The three functions differ in which cross-validation criterion they are using. \code{COPPS} is the original \acronym{COPPS} procedure \cite{Zou et al. (2020)}, i.e. uses \link[=criterionL2loss]{quadratic error loss}. \code{CV1} and \code{CVmod} use \link[=criterionL1loss]{absolute error loss} and the \link[=criterionMod]{modified quadratic error loss}, respectively.}

\usage{
COPPS(Y, param = 5L, estimator = optimalPartitioning,
      output = c("param", "fit", "detailed"), ...)
CV1(Y, param = 5L, estimator = optimalPartitioning,
    output = c("param", "fit", "detailed"), ...)
CVmod(Y, param = 5L, estimator = optimalPartitioning,
      output = c("param", "fit", "detailed"), ...)
}

\arguments{
\item{Y}{the observations, can be any data type that supports the function \code{\link{length}} and the operator \code{\link[=[]{[]}} and can be passed to \code{estimator} and the \code{\link[=criteria]{cross-validation criterion}}, e.g. a numeric vector or a list. Support for \code{\link[=matrix]{matrices}}, i.e. for multivariate data, is planned but not implemented so far}
\item{param}{a \code{\link{list}} giving the possible tuning parameters. Alternatively, a single integer which will be interpreted as the maximal number of change-points and converted to \code{as.list(0:param)}}
\item{estimator}{a function providing a local estimate. For pre-implemented estimators see \link{estimators}. The function must have the arguments \code{Y}, \code{param} and \code{...}, where \code{Y} will be a subset of the observations, and \code{param} and \code{...} will be the corresponding arguments of the called function. Note that \code{...} will be passed to \code{estimator} and the \code{\link[=criterion]{cross-validation criterion}}. The return value must be either a list of length \code{length(param)} with each entry containing the estimated change-point locations for the given entry in \code{param} or a list containing the named entries \code{cps} and \code{value}. In this case \code{cps} has to be a list of the estimated change-points as before and \code{value} has to be a list of the locally estimated values for each entry in \code{param}, i.e. each list entry has to be a list itself of length one entry longer than the corresponding entry in \code{cps}. The function \code{\link{convertSingleParam}} offers the conversion of an estimator allowing a single parameter into an estimator allowing multiple parameters}
\item{output}{a string specifying the output, either \code{"param"}, \code{"fit"} or \code{"detailed"}. For details what they mean see \cite{Value}}
\item{...}{additional parameters that are passed to \code{estimator} and the \code{\link[=criterion]{cross-validation criterion}}}
}

\value{
if \code{output == "param"}, the selected tuning parameter, i.e. an entry from \code{param}. If \code{output == "fit"}, a list with the entries \code{param}, giving the selected tuning parameter, and \code{fit}. The named entry \code{fit} is a list giving the returned fit obtained by applying \code{estimator} to the whole data \code{Y} with the selected tuning parameter. The returned value is transformed to a list with an entry \code{cps} giving the estimated change-points and, if provided by \code{estimator}, an entry \code{value} giving the estimated local values. If \code{output == "detailed"}, the same as for \code{output == "fit"}, but additionally the entries \code{CP}, \code{CVodd},  and \code{CVeven} giving the calculated cross-validation criteria for all \code{parameter} entries. \code{CVodd} and \code{CVeven} are the criteria when the odd / even observations are in the test set, respectively. \code{CP} is the sum of those two.
}

\seealso{
\link{estimators}, \link{criteria}, \code{\link{convertSingleParam}}
}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. \emph{arXiv:2112.03220}.

Zou, C., Wang, G., and Li, R. (2020) Consistent selection of the number of change-points via sample-splitting. \emph{The Annals of Statistics}, \bold{48}(1), 413--439.
}

\examples{
# call with default parameters:
# 2-folds cross-validation with ordereded folds, absolute error loss,
# optimal partitioning, and possible parameters being 0 to 5 change-points
CV1(Y = rnorm(100))
# the same, but with modified error loss
CVmod(Y = rnorm(100))
# the same, but with quadratic error loss, indentical to COPPS procedure
COPPS(Y = rnorm(100))

# more interesting data and more detailed output
set.seed(1L)
Y <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
CV1(Y = Y, output = "detailed")
# finds the correct change-points at 50, 100, 150
# (plus the start and end points 0 and 200)

# list of parameters, only allowing 1 or 2 change-points
CVmod(Y = Y, param = as.list(1:2))

# COPPS potentially fails to provide a good selection when large changes occur at odd locations
# Example 1 in (Pein and Shah, 2021), see Section 2.2 in this paper for more details
set.seed(1)
exampleY <- rnorm(102, c(rep(10, 46), rep(0, 5), rep(30, 51)))
# misses one change-point
COPPS(Y = exampleY) 

# correct number of change-points when modified criterion (or absolute error loss) is used
CVmod(Y = exampleY)

# PELT as a local estimator instead of optimal partitioning
# param must contain parameters that are acceptable for the given estimator
CV1(Y = Y, estimator = pelt, output = "detailed", param = list("SIC", "MBIC", 3 * log(length(Y))))

# argument minseglen of pelt specified in ...
CVmod(Y = Y, estimator = pelt, output = "detailed", param = list("SIC", "MBIC", 3 * log(length(Y))),
                  minseglen = 30)
}

\keyword{nonparametric}
