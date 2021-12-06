
convertSingleParam <- function(estimator) {
  function(Y, param, ...) {
    ret <- list()
    for (i in seq_along(param)) {
      ret[[i]] <- estimator(Y, param[[i]], ...)
    }

    if (is.list(ret[[1]])) {
      cps <- list()
      value <- list()
      for (i in seq_along(param)) {
       cps[[i]] <- ret[[i]]$cps
       value[[i]] <- ret[[i]]$value
      }
      ret <- list(cps = cps, value = value)
    }
    
    ret
  }
}

optimalPartitioning <- function(Y, param, ...) {
  maxK <- max(c(max(as.numeric(param)), 2))
  # changepoint::cpt.mean produces an error if Q == 2
  cps <- suppressWarnings(changepoint::cpt.mean(data = Y, penalty = "None",
                                                method = "SegNeigh", Q = maxK + 1))@cpts.full
  ret <- list()
  for (i in seq_along(param)) {
    if (param[[i]] == 0) {
      ret[[i]] <- numeric(0)
    } else {
      ret[[i]] <- cps[param[[i]], 1:param[[i]]]
    }
  }
  ret
}

.peltSingleParam <- function(Y, param, minseglen = 1, ...) {
  if (is.numeric(param)) {
    ret <- changepoint::cpt.mean(data = Y, penalty = "Manual", pen.value = param, method = "PELT",
                                 minseglen = minseglen)
  } else {
    ret <- changepoint::cpt.mean(data = Y, penalty = param, method = "PELT", minseglen = minseglen)
  }
  
  list(cps = ret@cpts[-length(ret@cpts)], value = as.list(ret@param.est$mean))
}

pelt <- function(Y, param, ...) {
  estimator <- convertSingleParam(.peltSingleParam)
  estimator(Y, param, ...)
}

.binsegSingleParam <- function(Y, param, Q = 5, minseglen = 1, ...) {
  if (is.list(param)) {
    if (is.numeric(param$penalty)) {
      ret <- suppressWarnings(changepoint::cpt.mean(data = Y, penalty = "Manual", pen.value = param$penalty,
                                                    method = "BinSeg", Q = param$Q, minseglen = minseglen))
    } else {
      ret <- suppressWarnings(changepoint::cpt.mean(data = Y, penalty = param$penalty, method = "BinSeg", Q = param$Q,
                                                    minseglen = minseglen))
    }
  } else {
    if (is.numeric(param)) {
      ret <- changepoint::cpt.mean(data = Y, penalty = "Manual", pen.value = param, method = "BinSeg",
                                   Q = Q, minseglen = minseglen)
    } else {
      ret <- changepoint::cpt.mean(data = Y, penalty = param, method = "BinSeg", Q = Q, minseglen = minseglen)
    }
  }
  
  list(cps = ret@cpts[-length(ret@cpts)], value = as.list(ret@param.est$mean))
}

binseg <- function(Y, param, ...) {
  estimator <- convertSingleParam(.binsegSingleParam)
  estimator(Y, param, ...)
}

wbs <- function(Y, param, ...) {
  ret <- wbs::changepoints(wbs::wbs(Y), th.const = param, ...)$cpt.th
  for (i in 1:length(ret)) {
    ret[[i]] <- sort(ret[[i]])
  }
  ret
}

.smuceSingleParam <- function(Y, param, ...) {
  ret <- stepR::stepFit(Y, alpha = param, ...)
  list(cps = ret$rightIndex[-length(ret$rightIndex)], value = as.list(ret$value))
}

smuce <- function(Y, param, ...) {
  estimator <- convertSingleParam(.smuceSingleParam)
  estimator(Y, param, ...)
}

.fdrsegSingleParam <- function(Y, param, ...) {
  ret <- FDRSeg::fdrseg(Y, alpha = param, ...)
  list(cps = ret$left[-1] - 1L, value = as.list(ret$value))
}

fdrseg <- function(Y, param, ...) {
  estimator <- convertSingleParam(.fdrsegSingleParam)
  estimator(Y, param, ...)
}
  
