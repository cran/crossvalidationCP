\name{crossvalidationCP}
\alias{crossvalidationCP}

\title{Cross-validation in change-point regression}
\description{Generic function for cross-validation to select tuning parameters in change-point regression. It selects among a list of parameters the one with the smallest cross-validation criterion for a given method. The cross-validation criterion, the estimator, and the the folds can be specified by the user.
}

\usage{
crossvalidationCP(Y, param = 5L, folds = 5L, estimator = leastSquares,
                  criterion = criterionL1loss,
                  output = c("param", "fit", "detailed"), ...)
}

\arguments{
\item{Y}{the observations, can be any data type that supports the function \code{\link{length}} and the operator \code{\link[=[]{[]}} and can be passed to \code{estimator} and \code{criterion}, e.g. a numeric vector or a list. Support for \code{\link[=matrix]{matrices}}, i.e. for multivariate data, is planned but not implemented so far}
\item{param}{a \code{\link{list}} giving the possible tuning parameters. Alternatively, a single integer which will be interpreted as the maximal number of change-points and converted to \code{as.list(0:param)}. All values have to be acceptable values for the specified \code{estimator}}
\item{folds}{either a \code{\link{list}}, a single integer or the string \code{"COPPS"} specifying the folds. If a \code{\link{list}}, each entry should be an integer vector with values between \code{1} and \code{length(Y)} giving the indices of the observations in the fold. A single integer specifies the number of folds and ordered folds are automatically created, i.e. fold \code{i} will be \code{seq(i, length(Y), folds)}. \code{"COPPS"} means that a generalised \acronym{COPPS} procedure \cite{Zou et al. (2020)} will be used, i.e. 2-fold cross-validation with Order-Preserved Sample-Splitting, meaning that the folds will be the odd and even indexed observations. Note that observations will be given in reverse order to the cross-validation criterion when the odd-indexed observations are in the test set. This allows criteria such as the \link[=criterionMod]{modified criterion}, where for the odd-indexed the first and for the even-indexed the last observation is removed}
\item{estimator}{a function providing a local estimate. For pre-implemented estimators see \link{estimators}. The function must have the arguments \code{Y}, \code{param} and \code{...}, where \code{Y} will be a subset of the observations, and \code{param} and \code{...} will be the corresponding arguments of the called function. Note that \code{...} will be passed to \code{estimator} and \code{criterion}. The return value must be either a list of length \code{length(param)} with each entry containing the estimated change-point locations for the given entry in \code{param} or a list containing the named entries \code{cps} and \code{value}. In this case \code{cps} has to be a list of the estimated change-points as before and \code{value} has to be a list of the locally estimated values for each entry in \code{param}, i.e. each list entry has to be a list itself of length one entry longer than the corresponding entry in \code{cps}. The function \code{\link{convertSingleParam}} offers the conversion of an estimator allowing a single parameter into an estimator allowing multiple parameters}
\item{criterion}{a function providing the cross-validation criterion. For pre-implemented criteria see \link{criteria}. The function must have the arguments \code{testset}, \code{estset} and \code{value}. \code{testset} and \code{estset} are the observations of one segment that are in the test and estimation set, respectively. \code{value} is the local parameter on the segment if provided by \code{estimator}, otherwise \code{NULL}. Additionally, \code{...} is possible and potentially necessary to absorb arguments, since the argument \code{...} of \code{crossvalidationCP} will be passed to \code{estimator} and \code{criterion}. It must return a single numeric. All return values will be summed accordingly and \code{\link{which.min}} will be called on the vector to determine the parameter with the smallest criterion, hence some \code{NaN} values etc. are allowed}
\item{output}{a string specifying the output, either \code{"param"}, \code{"fit"} or \code{"detailed"}. For details what they mean see \cite{Value}}
\item{...}{additional parameters that are passed to \code{estimator} and \code{criterion}}
}

\value{
if \code{output == "param"}, the selected tuning parameter, i.e. an entry from \code{param}. If \code{output == "fit"}, a list with the entries \code{param}, giving the selected tuning parameter, and \code{fit}. The named entry \code{fit} is a list giving the returned fit obtained by applying \code{estimator} to the whole data \code{Y} with the selected tuning parameter. The retured value is transformed to a list with an entry \code{cps} giving the estimated change-points and, if provided by \code{estimator}, an entry \code{value} giving the estimated local values. If \code{output == "detailed"}, the same as for \code{output == "fit"}, but additionally an entry \code{CP} giving all calculated cross-validation criteria. Those values are summed over all folds
}

\seealso{
\link{estimators}, \link{criteria}, \code{\link{convertSingleParam}}, \code{\link{VfoldCV}}, \code{\link{COPPS}}, \code{\link{CV1}}, \code{\link{CVmod}}
}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. \emph{arXiv:2112.03220}.

Zou, C., Wang, G., and Li, R. (2020) Consistent selection of the number of change-points via sample-splitting. \emph{The Annals of Statistics}, \bold{48}(1), 413--439.
}

\examples{
# call with default parameters:
# 5-fold cross-validation with absolute error loss, least squares estimation,
# and possible parameters being 0 to 5 change-points
# a simpler access to it is offered by VfoldCV()
crossvalidationCP(Y = rnorm(100))

# more interesting data and more detailed output
set.seed(1L)
Y <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
crossvalidationCP(Y = Y, output = "detailed")
# finds the correct change-points at 50, 100, 150
# (plus the start and end points 0 and 200)

# list of parameters, only allowing 1 or 2 change-points
crossvalidationCP(Y = Y, param = as.list(1:2))

# reducing the number of folds to 3
ret <- crossvalidationCP(Y = Y, folds = 3L, output = "detailed")
# the same but with explicitly specified folds
identical(crossvalidationCP(Y = Y, folds = list(seq(1, 200, 3), seq(2, 200, 3), seq(3, 200, 3)),
                            output = "detailed"), ret)
                            
# 2-fold cross-validation with Order-Preserved Sample-Splitting
ret <- crossvalidationCP(Y = Y, folds = "COPPS", output = "detailed")

# a simpler access to it is offered by CV1()
identical(CV1(Y = Y, output = "detailed"), ret)

# different criterion: quadratic error loss
ret <- crossvalidationCP(Y = Y, folds = "COPPS", output = "detailed", criterion = criterionL2loss)

# same as COPPS procedure; as offered by COPPS()
identical(COPPS(Y = Y, output = "detailed"), ret)

# COPPS potentially fails to provide a good selection when large changes occur at odd locations
# Example 1 in (Pein and Shah, 2021), see Section 2.2 in this paper for more details
set.seed(1)
exampleY <- rnorm(102, c(rep(10, 46), rep(0, 5), rep(30, 51)))
# misses one change-point
crossvalidationCP(Y = exampleY, folds = "COPPS", criterion = criterionL2loss) 

# correct number of change-points when modified criterion (or absolute error loss) is used
(ret <- crossvalidationCP(Y = exampleY, folds = "COPPS", criterion = criterionMod)) 

# a simpler access to it is offered by CVmod() 
identical(CVmod(Y = exampleY), ret)

# manually given criterion; identical to criterionL1loss()
testCriterion <- function(testset, estset, value = NULL, ...) {
  if (!is.null(value)) {
    return(sum(abs(testset - value)))
  }
  
  sum(abs(testset - mean(estset)))
}
identical(crossvalidationCP(Y = Y, criterion = testCriterion, output = "detailed"),
          crossvalidationCP(Y = Y, output = "detailed"))
          
# PELT as a local estimator instead of least squares estimation
# param must contain parameters that are acceptable for the given estimator
crossvalidationCP(Y = Y, estimator = pelt, output = "detailed",
                  param = list("SIC", "MBIC", 3 * log(length(Y))))

# argument minseglen of pelt specified in ...
crossvalidationCP(Y = Y, estimator = pelt, output = "detailed",
                  param = list("SIC", "MBIC", 3 * log(length(Y))), minseglen = 60)
}

\keyword{nonparametric}
