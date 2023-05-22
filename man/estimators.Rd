\name{estimators}
\alias{estimators}
\alias{leastSquares}
\alias{optimalPartitioning}
\alias{pelt}
\alias{binseg}
\alias{wbs}

\title{Pre-implemented estimators}
\description{Pre-implemented change-point estimators that can be passed to the argument \code{estimator} in the cross-validation functions, see the functions listed in \cite{See Also}.
}

\usage{
leastSquares(Y, param, ...)
pelt(Y, param, ...)
binseg(Y, param, ...)
wbs(Y, param, ...)
}

\arguments{
\item{Y}{a numeric vector giving the observations}
\item{param}{a \code{\link{list}} giving the possible tuning parameters. See \cite{Details} to see which tuning parameters are allowed for which function}
\item{...}{additional arguments, see \cite{Details} to see which arguments are allowed for which function}
}

\value{
For \code{leastSquares} and \code{wbs} a list of length \code{length(param)} with each entry containing the estimated change-point locations for the given entry in \code{param}. For the other functions a list containing the named entries \code{cps} and \code{value}, with \code{cps} a list of the estimated change-points as before and \code{value} a list of the locally estimated values for each entry in \code{param}, i.e. each list entry is a list itself of length one entry longer than the corresponding entry in \code{cps}.
}

\details{
\code{leastSquares} implements least squares estimation by using the segment neighbourhoods algorithm with functional pruning from \cite{Rigaill (20015)}, see also \cite{Auger and Lawrence (1989)} for the original segment neighbourhoods algorithm. It calls \code{\link[fpopw]{Fpsn}}. Each list entry in \code{param} has to be a single integer giving the number of change-points.

\code{optimalPartitioning} is outdated. It will give the same results as \code{leastSquares}, but is slower. It is part of the package for backwards compatibility only.

\code{pelt} implements \acronym{PELT} \cite{(Killick et al., 2012)}, i.e. penalised maximum likelihood estimation computed by a pruned dynamic program. For each list entry in \code{param} it calls \code{\link[changepoint]{cpt.mean}} with \code{method = "PELT"} and \code{penalty = param[[i]]} or when \code{param[[i]]} is a numeric with \code{penalty = "Manual"} and \code{pen.value = param[[i]]}. Hence, each entry in \code{param} must be a single numeric or an argument that can be passed to \code{penalty}. Additionally \code{minseglen} can be specified in \code{...}, by default \code{minseglen = 1}.

\code{binseg} implements binary segmentation \cite{(Vostrikova, 1981)}. The call is the same as for \code{pelt}, but with \code{method = "BinSeg"}. Additionally, the maximal number of change-points \code{Q} can be specified in \code{...}, by default \code{Q = 5}. Alternatively, each list entry of \code{param} can be a list itself containing the named entries \code{penalty} and \code{Q}. Note that this estimator differs from binary segmentation in \cite{Zou et al. (2020)}, it requires a penalty instead of a given number of change-points. Warnings that \code{Q} is chosen too small are suppressed when \code{Q} is given in \code{param}, but not when it is a global parameter specified in \code{...} or \code{Q = 5} by default.

\code{wbs} implements wild binary segmentation \cite{(Fryzlewicz, 2014)}. It calls \code{\link[wbs]{changepoints}} with \code{th.const = param}, hence \code{param} has to be a list of positive scalars. Additionally, \code{...} will be passed.
}

\seealso{
\code{\link{crossvalidationCP}}, \code{\link{VfoldCV}}, \code{\link{COPPS}}, \code{\link{CV1}}, \code{\link{CVmod}}
}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. \emph{arXiv:2112.03220}.

Rigaill, G. (2015) A pruned dynamic programming algorithm to recover the best segmentations with 1 to Kmax change-points. \emph{Journal de la Societe Francaise de Statistique} \bold{156}(4), 180--205.

Auger, I. E., Lawrence, C. E. (1989) Algorithms for the Optimal Identification of Segment Neighborhoods. \emph{Bulletin of Mathematical Biology}, \bold{51}(1), 39--54.

Killick, R., Fearnhead, P., Eckley, I. A. (2012) Optimal detection of changepoints with a linear computational cost. \emph{Journal of the American Statistical Association}, \bold{107}(500), 1590--1598.

Vostrikova, L. Y. (1981). Detecting 'disorder' in multidimensional random processes. \emph{Soviet Mathematics Doklady},
\bold{24}, 55--59.

Fryzlewicz, P. (2014) Wild binary segmentation for multiple change-point detection. \emph{The Annals of Statistics}, \bold{42}(6), 2243--2281.

Zou, C., Wang, G., and Li, R. (2020). Consistent selection of the number of change-points via sample-splitting. \emph{The Annals of Statistics}, \bold{48}(1), 413--439.
}

\examples{
# all functions can be called directly, e.g.
leastSquares(Y = rnorm(100), param = 2)

# but their main purpose is to serve as a local estimator in the cross-validation functions, e.g.
crossvalidationCP(rnorm(100), estimator = leastSquares)

# param must contain values that are suitable for the given estimator
crossvalidationCP(rnorm(100), estimator = pelt, param = list("SIC", "MBIC"))
}

\keyword{nonparametric}
