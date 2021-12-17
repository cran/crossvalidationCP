\name{criteria}
\alias{criteria}
\alias{criterion}
\alias{criterionL2loss}
\alias{criterionL1loss}
\alias{criterionMod}

\title{Pre-implemented cross-validation criteria}
\description{\code{criterionL1loss}, \code{criterionMod} and \code{criterionL2loss} compute the cross-validation criterion with L1-loss, the modified criterion and the criterion with L2-loss for univariate data, see (15), (16), and (6) in \cite{Pein and Shah (2021)}, respectively. If \code{value} is given (i.e. \code{value =! NULL}), then \code{value} replaces the empirical means. All criteria can be passed to the argument \code{criterion} in the cross-validation functions, see the functions listed in \cite{See Also}.
}

\usage{
criterionL1loss(testset, estset, value = NULL, ...)
criterionMod(testset, estset, value = NULL, ...)
criterionL2loss(testset, estset, value = NULL, ...)
}

\arguments{
\item{testset}{a numeric vector giving the observations in the test set / fold. For \code{criterionMod}, if \code{length(testset) == 1L}, \code{NaN} will be returned, see \cite{Details}}
\item{estset}{a numeric vector giving the observations in the estimation set}
\item{value}{a single numeric giving the local value on the segment or \code{NULL}. If \code{NULL} the value will be \code{mean(estset)}}
\item{...}{unused}
}

\value{a single numeric}

\details{
\code{criterionMod} requires that the minimal segment length is at least \code{2}. So far the only pre-implemented estimators that allows for such an option are \code{\link{pelt}} and \code{\link{binseg}}, where one can specify \code{minseglen} in \code{...}.
}

\seealso{
\code{\link{crossvalidationCP}}, \code{\link{VfoldCV}}, \code{\link{COPPS}}, \code{\link{CV1}}, \code{\link{CVmod}}
}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. \emph{arXiv:2112.03220}.
}

\examples{
# all functions can be called directly, e.g.
Y <- rnorm(100)
criterionL1loss(testset = Y[seq(1, 100, 2)], estset = Y[seq(2, 100, 2)])

# but their main purpose is to serve as the criterion in the cross-validation functions, e.g.
crossvalidationCP(rnorm(100), criterion = criterionL1loss)
}

\keyword{nonparametric}
