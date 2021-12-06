
criterionL2loss <- function(testset, estset, value = NULL, ...) {
  if (!is.null(value)) {
    return(sum((testset - value)^2))
  }
  
  sum((testset - mean(estset))^2)
}

criterionL1loss <- function(testset, estset, value = NULL, ...) {
  if (!is.null(value)) {
    return(sum(abs(testset - value)))
  }
  
  sum(abs(testset - mean(estset)))
}

criterionMod <- function(testset, estset, value = NULL, ...) {
  if (!is.null(value)) {
    return(length(testset) / (length(testset) - 1) * sum((testset[-length(testset)] - value)^2))
  }
  
  # TODO: is this correct? do we want to remove the last value?
  
  length(testset) / (length(testset) - 1) * sum((testset[-length(testset)] - mean(estset))^2)
}

# TODO multivariate versions
