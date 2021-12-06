
.COPPS <- function(Y, param = 5L, estimator = optimalPartitioning, criterion = criterionL2loss,
                  output = c("param", "fit", "detailed"), ...) {
  if (!is.list(param)) {
    if (is.numeric(param) && length(param) == 1) {
      if (!is.integer(param)) {
        param <- as.integer(param)
      }
      param <- as.list(0:param)
    } else {
      warning("param has to be a list or an integer of length 1, attempt converting it to a list")
      param <- as.list(param)
    }
  }
  
  if (is.matrix(Y)) {
    stop("Y being a matrix is not supported so far. Adding options for multivariate data is however planed.")
    # 
    # if (is.matr)
    #   vec <- testset - mean(estset)
    # diag(x = 1, length(testset), length(testset))
    # sum(t(vec) %*% W %*% vec)
  }
  
  n <- length(Y)
  n2 <- as.integer(n / 2)
  if (n2 < 1) {
    stop("length(Y) has to be at least 2")
  }
  
  indO <- seq(1, n2 * 2, 2)
  indE <- seq(2, n2 * 2, 2)
  
  CVodd <- numeric(length(param))
  estimation <- estimator(Y[indE], param = param, ...)
  
  if (!is.list(estimation)) {
    stop("return value of estimator has to be a list")
  }
  
  if (is.null(estimation$value)) {
    value <- NULL
    
    if (length(estimation) != length(param)) {
      stop("return value of estimator has to be a list of the same length as param or ",
           "a named list containing the entries cps and value, which itself have to be lists")
    }
    
    for (cps in estimation) {
      if(!is.numeric(cps) || !all(cps %in% 1:n2)) {
        stop("all change-points estimated by estimator has to be between 1 and length(Y)")
      }
    }
  } else {
    value <- estimation$value
    estimation <- estimation$cps
    
    if (!is.list(value) || !is.list(estimation)) {
      stop("return value of estimator has to be a list of the same length as param or ",
           "a named list containing the entries cps and value, which itself have to be lists")
    }
    
    if (length(value) != length(param) || length(estimation) != length(param)) {
      stop("estimator returned a list containing entries 'cps' and 'value', but those entries have to be lists ",
           "of the same length as 'param'")
    }
    
    for (cps in estimation) {
      if (!is.numeric(cps) || !all(cps %in% 1:n2)) {
        stop("all change-points estimated by estimator has to be between 1 and length(Y)")
      }
    }
    
    for (i in seq_along(value)) {
      if (!is.list(value[[i]]) || length(value[[i]]) != length(estimation[[i]]) + 1) {
        stop("estimator returned a list containing entries 'cps' and 'value', but each entry in 'value' ",
             "has to be a list itself and one entry longer than the corresponding entry in 'cps'")
      }
    }
  }
  
  for (i in seq_along(param)) {
    cps <- c(0, estimation[[i]], n2)
    
    for (k in (seq_along(cps)[-1] - 1)) {
      CVodd[i] <- CVodd[i] + criterion(testset = rev(Y[indO][(cps[k]  + 1):cps[k + 1]]),
                                       estset = rev(Y[indE][(cps[k]  + 1):cps[k + 1]]), 
                                       value = value[[i]][[k]], ...)
    }
  }
  
  CVeven <- numeric(length(param))
  estimation <- estimator(Y[indO], param = param, ...)
  
  if (!is.list(estimation)) {
    stop("return value of estimator has to be a list")
  }
  
  if (is.null(estimation$value)) {
    value <- NULL
    
    if (length(estimation) != length(param)) {
      stop("return value of estimator has to be a list of the same length as param or ",
           "a named list containing the entries cps and value, which itself have to be lists")
    }
    
    for (cps in estimation) {
      if(!is.numeric(cps) || !all(cps %in% 1:n2)) {
        stop("all change-points estimated by estimator has to be between 1 and length(Y)")
      }
    }
  } else {
    value <- estimation$value
    estimation <- estimation$cps
    
    if (!is.list(value) || !is.list(estimation)) {
      stop("return value of estimator has to be a list of the same length as param or ",
           "a named list containing the entries cps and value, which itself have to be lists")
    }
    
    if (length(value) != length(param) || length(estimation) != length(param)) {
      stop("estimator returned a list containing entries 'cps' and 'value', but those entries have to be lists ",
           "of the same length as 'param'")
    }
    
    for (cps in estimation) {
      if (!is.numeric(cps) || !all(cps %in% 1:n2)) {
        stop("all change-points estimated by estimator has to be between 1 and length(Y)")
      }
    }
    
    for (i in seq_along(value)) {
      if (!is.list(value[[i]]) || length(value[[i]]) != length(estimation[[i]]) + 1) {
        stop("estimator returned a list containing entries 'cps' and 'value', but each entry in 'value' ",
             "has to be a list itself and one entry longer than the corresponding entry in 'cps'")
      }
    }
  }
  
  for (i in seq_along(param)) {
    cps <- c(0, estimation[[i]], n2)
    
    for (k in (seq_along(cps)[-1] - 1)) {
      CVeven[i] <- CVeven[i] + criterion(testset = Y[indE][(cps[k]  + 1):cps[k + 1]],
                                         estset = Y[indO][(cps[k]  + 1):cps[k + 1]], 
                                         value = value[[i]][[k]], ...)
    }
  }
  
  CV <- CVodd + CVeven
  
  output <- match.arg(output)
  param <- param[[which.min(CV)]]
  
  if (output == "param") {
    ret <- param
  } else {
    fit <- estimator(Y, param = list(param), ...)
    
    if (is.null(fit$value)) {
      fit <- list(cps = c(0, fit[[1]], n))
    } else {
      fit <- list(cps = c(0, fit$cps[[1]], n), value = fit$value[[1]])
    }

    if (output == "fit") {
      ret <- list(param = param, fit = fit)
    } else {
      ret <- list(param = param, fit = fit, CV = CV, CVodd = CVodd, CVeven = CVeven)
    }
  }

  ret
}

COPPS <- function(Y, param = 5L, estimator = optimalPartitioning,
                output = c("param", "fit", "detailed"), ...) {
  .COPPS(Y = Y, param = param, estimator = estimator, criterion = criterionL2loss, output = output, ...)
}

CV1 <- function(Y, param = 5L, estimator = optimalPartitioning,
                output = c("param", "fit", "detailed"), ...) {
  .COPPS(Y = Y, param = param, estimator = estimator, criterion = criterionL1loss, output = output, ...)
}

CVmod <- function(Y, param = 5L, estimator = optimalPartitioning,
                  output = c("param", "fit", "detailed"), ...) {
  .COPPS(Y = Y, param = param, estimator = estimator, criterion = criterionMod, output = output, ...)
}
