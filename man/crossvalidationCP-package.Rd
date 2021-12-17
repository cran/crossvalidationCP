\name{crossvalidationCP-package}
\alias{crossvalidationCP-package}
\docType{package}

\title{Cross-validation for change-point regression}

\description{Implements the cross-validation methodology from \cite{Pein and Shah (2021)}. The approach can be customised by providing cross-validation criteria, estimators for the change-point locations and local parameters, and freely chosen folds. Pre-implemented estimators and criteria are available. It also includes our own implementation of the \acronym{COPPS} procedure \cite{Zou et al. (2020)}. By default, 5-fold cross-validation with ordered folds, absolute error loss, and optimal partitioning for estimating the change-point locations is used.
}

\details{
The main function is \code{\link{crossvalidationCP}}. It selects among a list of parameters the one with the smallest cross-validation criterion for a given method. The user can freely choose the folds, the local estimator and the criterion. Several pre-implemented \link{estimators} and \link{criteria} are available. Estimators have to allow a list of parameters at the same time. One can use \code{\link{convertSingleParam}} to convert a function allowing only a single parameter to a function that allows a list of parameters.

A ssimpler, but more limited access is given by the functions \code{\link{VfoldCV}}, \code{\link{COPPS}}, \code{\link{CV1}} and \code{\link{CVmod}}. \code{\link{VfoldCV}} performs V-fold cross-validation, where the tuning parameter is directly the number of change-points. \code{\link{COPPS}} implements the \acronym{COPPS} procedure \cite{Zou et al. (2020)}, i.e. 2-fold cross-validation with Order-Preserved Sample-Splitting and the tuning parameter being again the number of change-points. \code{\link{CV1}} and \code{\link{CVmod}} do the same, but with \link[=criterionL1loss]{absolute error loss} and the \link[=criterionMod]{modified quadratic error loss}, see (15) and (16) in \cite{Pein and Shah (2021)}, instead of \link[=criterionL2loss]{quadratic error loss}.

Note that \code{\link{COPPS}} can be problematic when larger changes occur at odd locations. For a detailed discussion, why standard quadratic error loss can lead to misestimation, see Section 2 in \cite{Pein and Shah (2021)}. By default, we recommend to use \link[=criterionL1loss]{absolute error loss} and 5-fold cross-validation as offered by \code{\link{VfoldCV}}.

So far only univariate data is supported, but support for multivariate data is planned.}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. \emph{arXiv:2112.03220}.

Zou, C., Wang, G., and Li, R. (2020) Consistent selection of the number of change-points via sample-splitting. \emph{The Annals of Statistics}, \bold{48}(1), 413--439.
}

\keyword{package}
\keyword{nonparametric}

\seealso{
\code{\link{crossvalidationCP}}, \link{estimators}, \link{criteria}, \code{\link{convertSingleParam}}, \code{\link{VfoldCV}}, \code{\link{COPPS}}, \code{\link{CV1}}, \code{\link{CVmod}}
}

\examples{
# call with default parameters:
# 5-fold cross-validation with absolute error loss, optimal partitioning,
# and possible parameters being 0 to 5 change-points
Y <- rnorm(100)
(ret <- crossvalidationCP(Y = Y))
# a simpler, but more limited access to it is offered by VfoldCV()
identical(VfoldCV(Y = Y), ret)

# more interesting data and more detailed output
set.seed(1L)
Y <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
VfoldCV(Y = Y, output = "detailed")
# finds the correct change-points at 50, 100, 150
# (plus the start and end points 0 and 200)

# reducing the maximal number of change-points to 2
VfoldCV(Y = Y, Kmax = 2)

# crossvalidationCP is more flexible and allows a list of parameters
# here only 1 or 2 change-points are allowed
crossvalidationCP(Y = Y, param = as.list(1:2))

# reducing the number of folds to 3
ret <- VfoldCV(Y = Y, V = 3L, output = "detailed")
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

# PELT as a local estimator instead of optimal partitioning
# param must contain parameters that are acceptable for the given estimator
crossvalidationCP(Y = Y, estimator = pelt, output = "detailed",
                  param = list("SIC", "MBIC", 3 * log(length(Y))))

# argument minseglen of pelt specified in ...
crossvalidationCP(Y = Y, estimator = pelt, output = "detailed",
                  param = list("SIC", "MBIC", 3 * log(length(Y))), minseglen = 60)
}
