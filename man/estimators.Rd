\name{estimators}
\alias{estimators}
\alias{optimalPartitioning}
\alias{pelt}
\alias{binseg}
\alias{wbs}
\alias{smuce}
\alias{fdrseg}

\title{Pre-implemented estimators}
\description{Pre-implemented change-point estimators that can be passed to the argument \code{estimator} in the cross-validation functions, see the functions listed in \cite{See Also}.
}

\usage{
optimalPartitioning(Y, param, ...)
pelt(Y, param, ...)
binseg(Y, param, ...)
wbs(Y, param, ...)
smuce(Y, param, ...)
fdrseg(Y, param, ...)
}

\arguments{
\item{Y}{a numeric vector giving the observations}
\item{param}{a \code{\link{list}} giving the possible tuning parameters. See \cite{Details} to see which tuning parameters are allowed for which function}
\item{...}{additional arguments, see \cite{Details} to see which arguments are allowed for which function}
}

\value{
For \code{optimalPartitioning} and \code{wbs} a list of length \code{length(param)} with each entry containing the estimated change-point locations for the given entry in \code{param}. For the other functions a list containing the named entries \code{cps} and \code{value}, with \code{cps} a list of the estimated change-points as before and \code{value} a list of the locally estimated values for each entry in \code{param}, i.e. each list entry is a list itself of length one entry longer than the corresponding entry in \code{cps}.
}

\details{
\code{optimalPartitioning} implements optimal partitioning \cite{(Jackson et al., 2005)} using the segment neighbourhoods algorithm from \cite{Auger and Lawrence (1989)}. Each list entry in \code{param} has to be a single integer giving the number of change-points. It calls \code{\link[changepoint]{cpt.mean}} with \code{method = "SegNeigh"}.

\code{pelt} implements \acronym{PELT} \cite{(Killick et al., 2012)}, i.e. penalised maximum likelihood estimation computed by a pruned dynamic program. For each list entry in \code{param} it calls \code{\link[changepoint]{cpt.mean}} with \code{method = "PELT"} and \code{penalty = param[[i]]} or when \code{param[[i]]} is a numeric with \code{penalty = "Manual"} and \code{pen.value = param[[i]]}. Hence, each entry in \code{param} must be a single numeric or an argument that can be passed to \code{penalty}. Additionally \code{minseglen} can be specified in \code{...}, by default \code{minseglen = 1}.

\code{binseg} implements binary segmentation \cite{(Vostrikova, 1981)}. The call is the same as for \code{pelt}, but with \code{method = "BinSeg"}. Additionally, the maximal number of change-points \code{Q} can be specified in \code{...}, by default \code{Q = 5}. Alternatively, each list entry of \code{param} can be a list itself containing the named entries \code{penalty} and \code{Q}. Note that this estimator differs from binary segmentation in \cite{Zou et al. (2020)}, it requires a penalty instead of a given number of change-points. Warnings that \code{Q} is chosen too small are suppressed when \code{Q} is given in \code{param}, but not when it is a global parameter specified in \code{...} or \code{Q = 5} by default.

\code{wbs} implements wild binary segmentation \cite{(Fryzlewicz, 2014)}. It calls \code{\link[wbs]{changepoints}} with \code{th.const = param}, hence \code{param} has to be a list of positive scalars. Additionally, \code{...} will be passed.

\code{smuce} implements \acronym{SMUCE} \cite{(Frick et al., 2014)}. It calls \code{\link[stepR]{stepFit}} with \code{alpha = param[[i]]}, hence \code{param} has to be a list of probabilities, i.e. single numerics between 0 and 1, giving the significance levels. Additionally, \code{...} will be passed.

\code{fdrseg} implement \acronym{FDRSeg} \cite{(Li et al., 2016)}. It calls \code{\link[FDRSeg]{fdrseg}} with \code{alpha = param[[i]]}, hence param has to be a list of significance levels. Additionally, \code{...} will be passed.
}

\seealso{
\code{\link{crossvalidationCP}}, \code{\link{VfoldCV}}, \code{\link{COPPS}}, \code{\link{CV1}}, \code{\link{CVmod}}
}

\references{
Pein, F., and Shah, R. D. (2021) Cross-validation for change-point regression: pitfalls and solutions. In preparation.

Jackson, B., Scargle, J. D., Barnes, D., Arabhi, S., Alt, A., Gioumousis, P., Gwin, E., Sangtrakulcharoen, P., Tan, L., Tsai, T. T. (2005) An Algorithm for optimal partitioning of data on an interval. \emph{IEEE Signal Processing Letters}, \bold{12}(2), 105--108.

Auger, I. E., Lawrence, C. E. (1989) Algorithms for the Optimal Identification of Segment Neighborhoods. \emph{Bulletin of Mathematical Biology}, \bold{51}(1), 39--54.

Killick, R., Fearnhead, P., Eckley, I. A. (2012) Optimal detection of changepoints with a linear computational cost. \emph{Journal of the American Statistical Association}, \bold{107}(500), 1590--1598.

Vostrikova, L. Y. (1981). Detecting 'disorder' in multidimensional random processes. \emph{Soviet Mathematics Doklady},
\bold{24}, 55--59.

Fryzlewicz, P. (2014) Wild binary segmentation for multiple change-point detection. \emph{The Annals of Statistics}, \bold{42}(6), 2243--2281.

Frick, K., Munk, A., Sieling, H. (2014) Multiscale change-point inference. With discussion and rejoinder by the authors. \emph{Journal of the Royal Statistical Society, Series B}, \bold{76}(3), 495--580.

Li, H., Munk, A., Sieling, H. (2016) FDR-control in multiscale change-point segmentation. \emph{Electronic Journal of Statistics}, \bold{10}(1), 918--959.

Zou, C., Wang, G., and Li, R. (2020). Consistent selection of the number of change-points via sample-splitting. \emph{The Annals of Statistics}, \bold{48}(1), 413--439.
}

\examples{
# all functions can be called directly, e.g.
optimalPartitioning(Y = rnorm(100), param = 2)

# but their main purpose is to serve as a local estimator in the cross-validation functions, e.g.
crossvalidationCP(rnorm(100), estimator = optimalPartitioning)

# param must contain values that are suitable for the given estimator
crossvalidationCP(rnorm(100), estimator = pelt, param = list("SIC", "MBIC"))
}

\keyword{nonparametric}
