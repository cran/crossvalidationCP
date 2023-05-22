
crossvalidationCP <- function(Y, param = 5L, folds = 5L, estimator = leastSquares,
                              criterion = criterionL1loss, output = c("param", "fit", "detailed"), ...) {
  if (length(folds) == 1 && folds == "COPPS") {
    return(.COPPS(Y = Y, param = param, estimator = estimator, criterion = criterion, output = output, ...))
  }
  
  if (!is.list(param)) {
    if (is.numeric(param) && length(param) == 1) {
      if (!is.integer(param)) {
        param <- as.integer(param)
      }
      
      if (param > length(Y) / 2) {
        stop("maximal number of change-points have to be smaller than length(Y) / 2")
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

  CV <- numeric(length(param))
  
  if (is.numeric(folds) && length(folds) == 1) {
    if (folds < 2 || folds > n / 2) {
      stop("if folds is an integer, it has to be between 2 and length(Y)/2")
    }
    V <- folds
    folds <- list()
    for (fold in 1:V) {
      folds[[fold]] <- seq(fold, n, V)
    }
  } else {
    if (!is.list(folds)) {
      stop("folds have to be either a list, a single integer or the string 'COPPS'")
    }
    
    for (fold in folds) {
      if(!is.numeric(fold) || !all(fold %in% 1:n)) {
        stop("each list entry of folds has to contain only numerics between 1 and length(Y)")
      }
    }
    
    V <- length(folds)
  }
  
  for (fold in 1:V) {
    estimation <- estimator(Y[ -folds[[fold]] ], param = param, ...)
    
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
        if(!is.numeric(cps) || !all(cps %in% 1:length(Y[ -folds[[fold]] ]))) {
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
        if (!is.numeric(cps) || !all(cps %in% 1:length(Y[ -folds[[fold]] ]))) {
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
      cps <- c(0, (1:n)[ -folds[[fold]] ][estimation[[i]]], n)
      
      for (k in (seq_along(cps)[-1] - 1)) {
        CV[i] <- CV[i] + criterion(testset = Y[ folds[[fold]] ][cps[k] < folds[[fold]] & folds[[fold]] <= cps[k + 1]],
                                   estset = Y[ -folds[[fold]] ][cps[k] < (1:n)[ -folds[[fold]] ] &
                                                                  (1:n)[ -folds[[fold]] ] <= cps[k + 1]], 
                                   value = value[[i]][[k]], ...)
      }
    }
  }
  
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
      ret <- list(param = param, fit = fit, CV = CV)
    }
  }

  ret
}

VfoldCV <- function(Y, V = 5L, Kmax = 8L, adaptiveKmax = TRUE, tolKmax = 3L, 
                    estimator = leastSquares, criterion = criterionL1loss,
                    output = c("param", "fit", "detailed"), ...) {
  output <- match.arg(output)
  
  if (adaptiveKmax) {
    while (Kmax < length(Y) - 2) {
      if (Kmax > length(Y) / 2 - 1) {
        Kmax <- length(Y) / 2 - 1
      }
      ret <- crossvalidationCP(Y = Y, param = Kmax, folds = V, estimator = estimator,
                               criterion = criterion, output = output, ...)
      if (output == "param") {
        numberCps <- ret
      } else {
        numberCps <- ret$param
      }
      
      if (numberCps < Kmax - tolKmax) {
        break
      }
      
      Kmax <- Kmax * 2
    }
  } else {
    ret <- crossvalidationCP(Y = Y, param = Kmax, folds = V, estimator = estimator,
                             criterion = criterion, output = output, ...)
  }
  
  ret
}
