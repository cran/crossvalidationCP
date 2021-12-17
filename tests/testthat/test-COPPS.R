context("crossvalidationCP")

test_that("Y is tested and works", {
  expect_error(COPPS())
  expect_error(CV1(Y = NULL))
  expect_error(CVmod(Y = "s"))
  expect_error(CVmod(Y = rep("s", 20)))
  expect_error(COPPS(Y = as.list(rnorm(100))))
  expect_error(CV1(Y = matrix(rnorm(100), 2, 50)))
  
  set.seed(1)
  ret <- COPPS(Y = rnorm(100))
  expect_identical(ret, 0L)
  
  ret <- CV1(Y = rnorm(100))
  expect_identical(ret, 0L)
  
  ret <- CVmod(Y = rnorm(100))
  expect_identical(ret, 0L)
})

test_that("output is tested and works", {
  testY <- rnorm(100)
  expect_error(COPPS(Y = testY, output = 1))
  expect_error(COPPS(Y = testY, output = "test"))
  expect_error(COPPS(Y = testY, output = c("param", "fit")))
  
  testY <- 1:100
  expect_identical(COPPS(Y = testY, output = "param"),
                   COPPS(Y = testY))
  expect_identical(CVmod(Y = testY, output = "fit"),
                   CVmod(Y = testY, output = "fi"))
  expect_identical(CV1(Y = testY, output = "detailed")$param,
                   CV1(Y = testY))
  expect_identical(CV1(Y = testY, output = "detailed")$fit,
                   CV1(Y = testY, output = "fit")$fit)
})

test_that("fit is correct", {
  set.seed(4336L)
  testY <- rnorm(100)
  ret <- CV1(Y = testY, output = "fit")$fit
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 0L)[[1]], length(testY)))
  expect_equal(ret, compare)
  
  testY <- c(rnorm(50), rnorm(50, 5))
  ret <- CVmod(Y = testY, output = "fit")$fit
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 1L)[[1]], length(testY)))
  expect_equal(ret, compare)
  
  testY <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
  ret <- COPPS(Y = testY, output = "fit")$fit
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 3L)[[1]], length(testY)))
  expect_equal(ret, compare)
})

test_that("CV is correct", {
  set.seed(1376L)
  testY <- rnorm(100)
  
  ret <- COPPS(Y = testY, output = "detailed")$CV
  compare <- criterionL2loss(testset = testY[0:49 * 2 + 1], estset = testY[0:49 * 2 + 2]) +
    criterionL2loss(testset = testY[0:49 * 2 + 2], estset = testY[0:49 * 2 + 1])
  expect_equal(ret[1], compare)
  
  ret <- CV1(Y = testY, output = "detailed")$CV
  compare <- criterionL1loss(testset = testY[0:49 * 2 + 1], estset = testY[0:49 * 2 + 2]) +
    criterionL1loss(testset = testY[0:49 * 2 + 2], estset = testY[0:49 * 2 + 1])
  expect_equal(ret[1], compare)
  
  ret <- CVmod(Y = testY, output = "detailed")$CV
  compare <- criterionMod(testset = rev(testY[0:49 * 2 + 1]), estset = testY[0:49 * 2 + 2]) +
    criterionMod(testset = testY[0:49 * 2 + 2], estset = testY[0:49 * 2 + 1])
  expect_equal(ret[1], compare)
  
  ret <- COPPS(Y = testY, output = "detailed")$CV
  # 2 == optimalPartitioning(Y = testY[-(0:49 * 2 + 1)], param = 1L)[[1]] -> 4
  compare <- criterionL2loss(testset = testY[0:1 * 2 + 1], estset = testY[0:1 * 2 + 2]) +
    criterionL2loss(testset = testY[2:49 * 2 + 1], estset = testY[2:49 * 2 + 2])
  # 24 == optimalPartitioning(Y = testY[-(0:49 * 2 + 2)], param = 1L)[[1]] -> 47
  compare <- compare +
    criterionL2loss(testset = testY[0:23 * 2 + 2], estset = testY[0:23 * 2 + 1]) +
    criterionL2loss(testset = testY[24:49 * 2 + 2], estset = testY[24:49 * 2 + 1])
  expect_equal(ret[2], compare)
  
  ret <- CV1(Y = testY, output = "detailed")$CV
  # 2 == optimalPartitioning(Y = testY[-(0:49 * 2 + 1)], param = 1L)[[1]] -> 4
  compare <- criterionL1loss(testset = testY[0:1 * 2 + 1], estset = testY[0:1 * 2 + 2]) +
    criterionL1loss(testset = testY[2:49 * 2 + 1], estset = testY[2:49 * 2 + 2])
  # 24 == optimalPartitioning(Y = testY[-(0:49 * 2 + 2)], param = 1L)[[1]] -> 47
  compare <- compare +
    criterionL1loss(testset = testY[0:23 * 2 + 2], estset = testY[0:23 * 2 + 1]) +
    criterionL1loss(testset = testY[24:49 * 2 + 2], estset = testY[24:49 * 2 + 1])
  expect_equal(ret[2], compare)
  
  ret <- CVmod(Y = testY, output = "detailed")$CV
  # 2 == optimalPartitioning(Y = testY[-(0:49 * 2 + 1)], param = 1L)[[1]] -> 4
  compare <- criterionMod(testset = rev(testY[0:1 * 2 + 1]), estset = testY[0:1 * 2 + 2]) +
    criterionMod(testset = rev(testY[2:49 * 2 + 1]), estset = testY[2:49 * 2 + 2])
  # 24 == optimalPartitioning(Y = testY[-(0:49 * 2 + 2)], param = 1L)[[1]] -> 47
  compare <- compare +
    criterionMod(testset = testY[0:23 * 2 + 2], estset = testY[0:23 * 2 + 1]) +
    criterionMod(testset = testY[24:49 * 2 + 2], estset = testY[24:49 * 2 + 1])
  expect_equal(ret[2], compare)
})

test_that("param is tested and works", {
  testY <- 1:100
  expect_identical(CVmod(Y = testY, param = 5L, output = "detailed"),
                   CVmod(Y = testY, output = "detailed"))
  expect_identical(COPPS(Y = testY, param = as.list(0:5), output = "detailed"),
                   COPPS(Y = testY, output = "detailed"))
  
  expect_error(suppressWarnings(CV1(Y = testY, param = "test")))
  expect_error(suppressWarnings(CV1(Y = testY, param = list("test"))))
  
  expect_warning(ret <- CVmod(Y = testY, param = 1:3, output = "detailed"))
  expect_identical(ret, CVmod(Y = testY, param = as.list(1:3), output = "detailed"))
  
  expect_identical(CV1(Y = testY, param = 4L, output = "detailed"),
                   CV1(Y = testY, param = as.list(0:4), output = "detailed"))
  expect_identical(COPPS(Y = testY, param = 3, output = "detailed"),
                   COPPS(Y = testY, param = 3L, output = "detailed"))
  
  set.seed(245L)
  testY <- rnorm(100)
  expect_identical(COPPS(Y = testY, param = as.list(2:4)), 2L)
  expect_identical(CVmod(Y = testY, param = as.list(c(2, 3))), 3)
})

test_that("estimator is tested and works", {
  testY <- 1:100
  expect_identical(COPPS(Y = testY, estimator = optimalPartitioning, output = "detailed"),
                   COPPS(Y = testY, output = "detailed"))
  
  expect_error(CV1(Y = testY, estimator = 1))
  testEstimator <- function() {NA}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y) {NA}
  expect_error(COPPS(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param) {NA}
  expect_error(COPPS(Y = testY, estimator = testEstimator, test = 1))
  
  testEstimator <- function(Y, param, ...) {NA}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list()}
  expect_error(CV1(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(5, 5))}
  expect_error(COPPS(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(5, 7))}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(0, 6))}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(81, 6))}
  expect_error(COPPS(Y = testY, estimator = testEstimator))
  
  testEstimator <- function(Y, param, ...) {list(value = as.list(rep(1, 6)))}
  expect_error(CV1(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = 1, value = as.list(rep(1, 6)))}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 6)), value = 1)}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = list(), value = as.list(rep(1, 6)))}
  expect_error(COPPS(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 5)), value = as.list(rep(1, 6)))}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(-1, 6)), value = as.list(rep(1, 6)))}
  expect_error(COPPS(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 6)), value = as.list(rep(1, 5)))}
  expect_error(CVmod(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 6)), value = as.list(rep(1, 6)))}
  expect_error(CV1(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 3)), value = list(list(1), list(2), list(3)))}
  expect_error(COPPS(Y = testY, estimator = testEstimator, param = as.list(1:3)))
  
  testEstimator <- function(Y, param, ...) {list(cps = list(10, 25, 40),
                                                 value = list(as.list(0:1), as.list(0:1), as.list(0:1)))}
  expect_identical(COPPS(Y = c(rep(0, 50), rep(1, 50)), estimator = testEstimator,
                                     param = as.list(1:3), folds = 2L), 2L)
  
  ## existing estimators
  set.seed(8346L)
  testY <- c(rnorm(25), rnorm(25, 10))
  ret <- CV1(Y = testY, estimator = pelt, output = "detailed", param = list("SIC", 1e9))
  retCompare <- pelt(Y = testY, param = list("SIC", 1e9))
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- pelt(Y = testY[0:24 * 2 + 1], param = "SIC")
  # 13 == compareEst1$cps[[1]] -> 25
  compareEst2 <- pelt(Y = testY[0:24 * 2 + 2], param = "SIC")
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionL1loss(testset = testY[0:12 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionL1loss(testset = testY[13:24 * 2 + 2], value = compareEst1$value[[1]][[2]]) + 
    criterionL1loss(testset = rev(testY[0:11 * 2 + 1]), value = compareEst2$value[[1]][[1]]) +
    criterionL1loss(testset = rev(testY[12:24 * 2 + 1]), value = compareEst2$value[[1]][[2]])
  expect_equal(ret$CV[1], compare)
  
  
  ret <- COPPS(Y = testY, estimator = binseg, output = "detailed", param = list("SIC", 1e9))
  retCompare <- binseg(Y = testY, param = list("SIC", 1e9))
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- binseg(Y = testY[0:24 * 2 + 1], param = "SIC")
  # 13 == compareEst1$cps[[1]] -> 25
  compareEst2 <- binseg(Y = testY[0:24 * 2 + 2], param = "SIC")
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionL2loss(testset = testY[0:12 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionL2loss(testset = testY[13:24 * 2 + 2], value = compareEst1$value[[1]][[2]]) + 
    criterionL2loss(testset = testY[0:11 * 2 + 1], value = compareEst2$value[[1]][[1]]) +
    criterionL2loss(testset = testY[12:24 * 2 + 1], value = compareEst2$value[[1]][[2]])
  expect_equal(ret$CV[1], compare)
  
  ret <- CVmod(Y = 1:100, estimator = binseg,
                           param = list(list(penalty = "SIC", Q = 5), list(penalty = "SIC", Q = 20),
                                        1e9, list(penalty = 1e9, Q = 20)))
  expect_identical(ret, list(penalty = "SIC", Q = 20))
  
  
  ret <- CVmod(Y = testY, estimator = smuce, output = "detailed", param = list(0.01, 0.05, 0.5))
  retCompare <- smuce(Y = testY, param = 0.01)
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- smuce(Y = testY[0:24 * 2 + 1], param = 0.01)
  # 10, 13 == compareEst1$cps[[1]] -> 19, 25
  compareEst2 <- smuce(Y = testY[0:24 * 2 + 2], param = list(0.01, 0.05, 0.5))
  # 12 == compareEst2$cps[[1]] -crossvalidationCP> 24
  compare <- criterionMod(testset = testY[0:9 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionMod(testset = testY[10:12 * 2 + 2], value = compareEst1$value[[1]][[2]]) +
    criterionMod(testset = testY[13:24 * 2 + 2], value = compareEst1$value[[1]][[3]]) + 
    criterionMod(testset = rev(testY[0:11 * 2 + 1]), value = compareEst2$value[[1]][[1]]) +
    criterionMod(testset = rev(testY[12:24 * 2 + 1]), value = compareEst2$value[[1]][[2]])
  expect_equal(ret$CV[1], compare)
  
  
  set.seed(3955L)
  testY <- c(rnorm(25, 0, 0.1), rnorm(25, 10, 0.1))
  ret <- CVmod(Y = testY, estimator = fdrseg, output = "detailed", param = list(0.01, 0.05))
  retCompare <- fdrseg(Y = testY, param = 0.01)
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- fdrseg(Y = testY[0:24 * 2 + 1], param = 0.01)
  # 13 == compareEst1$cps[[1]] -> 25
  compareEst2 <- fdrseg(Y = testY[0:24 * 2 + 2], param = list(0.05, 0.5, 0.01))
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionMod(testset = testY[0:12 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionMod(testset = testY[13:24 * 2 + 2], value = compareEst1$value[[1]][[2]]) + 
    criterionMod(testset = rev(testY[0:11 * 2 + 1]), value = compareEst2$value[[3]][[1]]) +
    criterionMod(testset = rev(testY[12:24 * 2 + 1]), value = compareEst2$value[[3]][[2]])
  expect_equal(ret$CV[1], compare)
  
  
  ret <- COPPS(Y = testY, estimator = wbs, output = "detailed", param = list(1.0, 1.3))
  compare <- list(cps = c(0, wbs(Y = testY, param = list(1.3, 1, 0.5))[[2]], length(testY)))
  expect_equal(ret$fit, compare)
  
  ret <- COPPS(Y = testY, output = "detailed", estimator = wbs, param = list(1.3, 1.5))$CV
  # 13 == wbs(Y = testY[0:24 * 2 + 1], param = 1.3)[[1]] -> 25
  compare <- criterionL2loss(testset = testY[0:12 * 2 + 2], estset = testY[0:12 * 2 + 1]) +
    criterionL2loss(testset = testY[13:24 * 2 + 2], estset = testY[13:24 * 2 + 1])
  # 12 == wbs(Y = testY[0:24 * 2 + 2], param = 1.3)[[1]] -> 24
  compare <- compare +
    criterionL2loss(testset = testY[0:11 * 2 + 1], estset = testY[0:11 * 2 + 2]) +
    criterionL2loss(testset = testY[12:24 * 2 + 1], estset = testY[12:24 * 2 + 2])
  expect_equal(ret[2], compare)
})

test_that("... is tested and works", {
  testY <- 1:100
  expect_identical(COPPS(Y = testY, output = "detailed", test = 1),
                   COPPS(Y = testY, output = "detailed"))
  
  expect_error(CV1(Y = testY, output = "detailed", testset = 1))
  
  expect_error(CVmod(Y = testY, estimator = smuce, param = list(0.01, 0.05, 0.5), folds = 2))
  
  testEstimator <- function(Y, param, testvalue, ...) {list(cps = list(10, 25, 40),
                                                            value = list(list(testvalue, testvalue),
                                                                         as.list(0:1), as.list(0:1)))}
  expect_identical(COPPS(Y = testY, output = "detailed", param = as.list(1:3),
                                     estimator = testEstimator, testvalue = 10)$fit$value, list(10, 10))
  testCriterion <- function(value, testcrit, ...) {as.numeric(value != testcrit)}
  testEstimator <- function(Y, param, testvalue, ...) {list(cps = list(10, 25, 40),
                                                            value = list(as.list(0:1), as.list(0:1),
                                                                         list(testvalue, testvalue)))}
  expect_error(CV1(Y = testY, output = "detailed", param = as.list(1:3),
                   estimator = testEstimator, testvalue = 10,
                   criterion = testCriterion, testcrit = 10)$CV)
  
  set.seed(63894L)
  testY <- c(rnorm(50), rnorm(5, 3), rnorm(50))
  ret <- COPPS(Y = testY, estimator = pelt, param = list("SIC"), output = "detailed")
  expect_identical(length(ret$fit$cps), 4L)
})


test_that("matrices are allowed for argument Y", {
  # TODO
  # ret <- crossvalidationCP(Y = matrix(rnorm(100), 2, 50))
})

test_that("argument Y is tested and works", {
  # TODO
  # crossvalidationCP(Y = as.list(rnorm(100)))
})
