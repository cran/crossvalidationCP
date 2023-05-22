\name{VfoldCV}
\alias{VfoldCV}

\title{V-fold cross-validation}
\description{Selects the number of change-points by minimizing a V-fold cross-validation criterion. The criterion, the estimator, and the number of folds can be specified by the user.
}

\usage{
VfoldCV(Y, V = 5L, Kmax = 8L, adaptiveKmax = TRUE, tolKmax = 3L, estimator = leastSquares,
        criterion = criterionL1loss, output = c("param", "fit", "detailed"), ...) 
}

\arguments{
\item{Y}{the observations, can be any data type that supports the function \code{\link{length}} and the operator \code{\link[=[]{[]}} and can be passed to \code{estimator} and \code{criterion}, e.g. a numeric vector or a list. Support for \code{\link[=matrix]{matrices}}, i.e. for multivariate data, is planned but not implemented so far}
\item{V}{a single integer giving the number of folds. Ordered folds will automatically be created, i.e. fold \code{i} will be \code{seq(i, length(Y), folds)}}
\item{Kmax}{a single integer giving maximal number of change-points}
\item{adaptiveKmax}{a single logical indicating whether \code{Kmax} should be chosen adaptively. If true \code{Kmax} will be double if the estimated number of change-points is not at least \code{Kmax - tolKmax}}
\item{tolKmax}{a single integer specifiying how much the estimated number of change-points have to be smaller than \code{Kmax}}
\item{estimator}{a function providing a local estimate. For pre-implemented estimators see \link{estimators}. The function must have the arguments \code{Y}, \code{param} and \code{...}, where \code{Y} will be a subset of the observations, \code{param} will be \code{list(0:Kmax)}, and \code{...} will be the argument \code{...} of \code{VfoldCV}. Note that \code{...} will be passed to \code{estimator} and \code{criterion}. The return value must be either a list of length \code{length(param)} with each entry containing the estimated change-point locations for the given entry in \code{param} or a list containing the named entries \code{cps} and \code{value}. In this case \code{cps} has to be a list of the estimated change-points as before and \code{value} has to be a list of the locally estimated values for each entry in \code{param}, i.e. each list entry has to be a list itself of length one entry longer than the corresponding entry in \code{cps}. The function \code{\link{convertSingleParam}} offers the conversion of an estimator allowing a single parameter into an estimator allowing multiple parameters. From the currently pre-implemented estimators only \code{\link{leastSquares}} accepts \code{param == list(0:Kmax)}. Estimators that allow \code{param} to differ from \code{list(0:Kmax)} can be used in \code{\link{crossvalidationCP}}}
\item{criterion}{a function providing the cross-validation criterion. For pre-implemented criteria see \link{criteria}. The function must have the arguments \code{testset}, \code{estset} and \code{value}. \code{testset} and \code{estset} are the observations of one segment that are in the test and estimation set, respectively. \code{value} is the local parameter on the segment if provided by \code{estimator}, otherwise \code{NULL}. Additionally, \code{...} is possible and potentially necessary to absorb arguments, since the argument \code{...} of \code{VfoldCV} will be passed to \code{estimator} and \code{criterion}. It must return a single numeric. All return values will be summed accordingly and \code{\link{which.min}} will be called on the vector to determine the parameter with the smallest criterion. Hence some \code{NaN} values etc. are allowed}
\item{output}{a string specifying the output, either \code{"param"}, \code{"fit"} or \code{"detailed"}. For details what they mean see \cite{Value}}
\item{...}{additional parameters that are passed to \code{estimator} and \code{criterion}}
}

\value{
if \code{output == "param"}, the selected number of change-points, i.e. an integer between \code{0} and \code{Kmax}. If \code{output == "fit"}, a list with the entries \code{param}, giving the selected number of change-points, and \code{fit}. The named entry \code{fit} is a list giving the returned fit obtained by applying \code{estimator} to the whole data \code{Y} with the selected tuning parameter. The returned value is transformed to a list with an entry \code{cps} giving the estimated change-points and, if provided by \code{estimator}, an entry \code{value} giving the estimated local values. If \code{output == "detailed"}, the same as for \code{output == "fit"}, but additionally an entry \code{CP} giving all calculated cross-validation criteria. Those values are summed over all folds
}

\seealso{
\link{estimators}, \link{criteria}, \code{\link{convertSingleParam}}
}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. \emph{arXiv:2112.03220}.
}

\examples{
# call with default parameters:
# 5-fold cross-validation with absolute error loss, least squares estimation,
# and 0 to 5 change-points
VfoldCV(Y = rnorm(100))

# more interesting data and more detailed output
set.seed(1L)
Y <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
VfoldCV(Y = Y, output = "detailed")
# finds the correct change-points at 50, 100, 150
# (plus the start and end points 0 and 200)

# reducing the number of folds to 3
VfoldCV(Y = Y, V = 3L, output = "detailed")

# reducing the maximal number of change-points to 2
VfoldCV(Y = Y, Kmax = 2)

# different criterion: modified error loss
VfoldCV(Y = Y, output = "detailed", criterion = criterionMod)

# manually given criterion; identical to criterionL1loss()
testCriterion <- function(testset, estset, value = NULL, ...) {
  if (!is.null(value)) {
    return(sum(abs(testset - value)))
  }
  
  sum(abs(testset - mean(estset)))
}
identical(VfoldCV(Y = Y, criterion = testCriterion, output = "detailed"),
          VfoldCV(Y = Y, output = "detailed"))
}

\keyword{nonparametric}
