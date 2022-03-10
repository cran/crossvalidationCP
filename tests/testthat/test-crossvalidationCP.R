context("crossvalidationCP")

test_that("Y is tested and works", {
  expect_error(crossvalidationCP())
  expect_error(crossvalidationCP(Y = NULL))
  expect_error(crossvalidationCP(Y = "s"))
  expect_error(crossvalidationCP(Y = rep("s", 20)))
  expect_error(crossvalidationCP(Y = as.list(rnorm(100))))
  expect_error(crossvalidationCP(Y = matrix(rnorm(100), 2, 50)))
  
  set.seed(1)
  ret <- crossvalidationCP(Y = rnorm(100))
  expect_identical(ret, 1L)
})

test_that("output is tested and works", {
  testY <- rnorm(100)
  expect_error(crossvalidationCP(Y = testY, output = 1))
  expect_error(crossvalidationCP(Y = testY, output = "test"))
  expect_error(crossvalidationCP(Y = testY, output = c("param", "fit")))
  
  testY <- 1:100
  expect_identical(crossvalidationCP(Y = testY, output = "param"),
                   crossvalidationCP(Y = testY))
  expect_identical(crossvalidationCP(Y = testY, output = "fit"),
                   crossvalidationCP(Y = testY, output = "fi"))
  expect_identical(crossvalidationCP(Y = testY, output = "detailed")$param,
                   crossvalidationCP(Y = testY))
  expect_identical(crossvalidationCP(Y = testY, output = "detailed")$fit,
                   crossvalidationCP(Y = testY, output = "fit")$fit)
})

test_that("fit is correct", {
  set.seed(4336L)
  testY <- rnorm(100)
  ret <- crossvalidationCP(Y = testY, output = "fit")$fit
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 0L)[[1]], length(testY)))
  expect_identical(ret, compare)
  
  testY <- c(rnorm(50), rnorm(50, 5))
  ret <- crossvalidationCP(Y = testY, output = "fit")$fit
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 1L)[[1]], length(testY)))
  expect_identical(ret, compare)
  
  testY <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
  ret <- crossvalidationCP(Y = testY, output = "fit")$fit
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 3L)[[1]], length(testY)))
  expect_identical(ret, compare)
})

test_that("CV is correct", {
  set.seed(1376L)
  testY <- rnorm(100)
  ret <- crossvalidationCP(Y = testY, output = "detailed")$CV
  
  compare <- criterionL1loss(testset = testY[0:19 * 5 + 1], estset = testY[-(0:19 * 5 + 1)]) +
    criterionL1loss(testset = testY[0:19 * 5 + 2], estset = testY[-(0:19 * 5 + 2)]) +
    criterionL1loss(testset = testY[0:19 * 5 + 3], estset = testY[-(0:19 * 5 + 3)]) +
    criterionL1loss(testset = testY[0:19 * 5 + 4], estset = testY[-(0:19 * 5 + 4)]) +
    criterionL1loss(testset = testY[0:19 * 5 + 5], estset = testY[-(0:19 * 5 + 5)])
  expect_equal(ret[1], compare)
  
  ret <- crossvalidationCP(Y = testY, output = "detailed", folds = 2)$CV
  # 2 == optimalPartitioning(Y = testY[-(0:49 * 2 + 1)], param = 1L)[[1]] -> 4
  compare <- criterionL1loss(testset = testY[0:1 * 2 + 1], estset = testY[0:1 * 2 + 2]) +
    criterionL1loss(testset = testY[2:49 * 2 + 1], estset = testY[2:49 * 2 + 2])
  # 24 == optimalPartitioning(Y = testY[-(0:49 * 2 + 2)], param = 1L)[[1]] -> 47
  compare <- compare +
    criterionL1loss(testset = testY[0:22 * 2 + 2], estset = testY[0:23 * 2 + 1]) +
    criterionL1loss(testset = testY[23:49 * 2 + 2], estset = testY[24:49 * 2 + 1])
  expect_equal(ret[2], compare)
})

test_that("param is tested and works", {
  testY <- 1:100
  expect_identical(crossvalidationCP(Y = testY, param = 5L, output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  expect_identical(crossvalidationCP(Y = testY, param = as.list(0:5), output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  
  expect_error(suppressWarnings(crossvalidationCP(Y = testY, param = "test")))
  expect_error(suppressWarnings(crossvalidationCP(Y = testY, param = list("test"))))
  
  expect_warning(ret <- crossvalidationCP(Y = testY, param = 1:3, output = "detailed"))
  expect_identical(ret, crossvalidationCP(Y = testY, param = as.list(1:3), output = "detailed"))
  
  expect_identical(crossvalidationCP(Y = testY, param = 4L, output = "detailed"),
                   crossvalidationCP(Y = testY, param = as.list(0:4), output = "detailed"))
  expect_identical(crossvalidationCP(Y = testY, param = 3, output = "detailed"),
                   crossvalidationCP(Y = testY, param = 3L, output = "detailed"))
  
  set.seed(245L)
  testY <- rnorm(100)
  expect_identical(crossvalidationCP(Y = testY, param = as.list(2:4)), 2L)
  expect_identical(crossvalidationCP(Y = testY, param = as.list(c(2, 3))), 2)
})

test_that("folds is tested and works", {
  testY <- 1:100
  expect_identical(crossvalidationCP(Y = testY, folds = 5L, output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  expect_identical(crossvalidationCP(Y = testY, folds = list(seq(1, 100, 5), seq(2, 100, 5), seq(3, 100, 5),
                                                             seq(4, 100, 5), seq(5, 100, 5)), output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  
  expect_error(crossvalidationCP(Y = testY, folds = 1:2))
  expect_error(crossvalidationCP(Y = testY, folds = matrix(1:4, 2, 2)))
  expect_error(crossvalidationCP(Y = testY, folds = "test"))
  expect_error(crossvalidationCP(Y = testY, folds = NA))
  expect_error(crossvalidationCP(Y = testY, folds = 1))
  expect_error(crossvalidationCP(Y = testY, folds = -1))
  expect_error(crossvalidationCP(Y = testY, folds = 51))
  expect_error(crossvalidationCP(Y = testY, folds = Inf))
  
  expect_error(crossvalidationCP(Y = testY, folds = list("test")))
  expect_error(crossvalidationCP(Y = testY, folds = list(0:20)))
  expect_error(crossvalidationCP(Y = testY, folds = list(c(1:20), 21:101)))
  expect_error(crossvalidationCP(Y = testY, folds = list(c(1:20), 21:90, c(91, NA, 92:100))))
  
  expect_identical(crossvalidationCP(Y = testY, folds = 4L, output = "detailed"),
                   crossvalidationCP(Y = testY, folds = list(seq(1, 100, 4), seq(2, 100, 4), seq(3, 100, 4),
                                                             seq(4, 100, 4)), output = "detailed"))
  expect_identical(crossvalidationCP(Y = testY, folds = 3, output = "detailed"),
                   crossvalidationCP(Y = testY, folds = 3L, output = "detailed"))
  
  expect_identical(crossvalidationCP(Y = c(rep(0, 50), rep(1, 50), rep(-1, 50)) + rnorm(150, 0, 1e-12), folds = list(1:20)), 0L)

  testY <- c(1:50, rep(50, 50))
  ret <- crossvalidationCP(Y = testY, folds = list(c(3, 5, 8, 9, 12, 16, 20, 25, 30, 36, 39, 43, 50)),
                           output = "detailed")
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 5L)[[1]], length(testY)))
  expect_identical(ret$fit, compare)
  # 38 == optimalPartitioning(Y = testY[-c(3, 5, 8, 9, 12, 16, 20, 25, 30, 36, 39, 43, 50)], param = 1L)[[1]] -> 32
  # (1:100)[-c(3, 5, 8, 9, 12, 16, 20, 25, 30, 36, 39, 43, 50)][23]
  compare <- criterionL1loss(testset = testY[c(3, 5, 8, 9, 12, 16, 20, 25, 30)], 
                             estset = (1:32)[-c(3, 5, 8, 9, 12, 16, 20, 25, 30)]) +
    criterionL1loss(testset = testY[c(36, 39, 43, 50)], estset = c((33:50)[-c(36, 39, 43, 50) + 32], rep(50, 50)))
  expect_equal(ret$CV[2], compare)
})

test_that("estimator is tested and works", {
  testY <- 1:100
  expect_identical(crossvalidationCP(Y = testY, estimator = optimalPartitioning, output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  
  expect_error(crossvalidationCP(Y = testY, estimator = 1))
  testEstimator <- function() {NA}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y) {NA}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param) {NA}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator, test = 1))
  
  testEstimator <- function(Y, param, ...) {NA}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list()}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(5, 5))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(5, 7))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(0, 6))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {as.list(rep(81, 6))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  
  testEstimator <- function(Y, param, ...) {list(value = as.list(rep(1, 6)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = 1, value = as.list(rep(1, 6)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 6)), value = 1)}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = list(), value = as.list(rep(1, 6)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 5)), value = as.list(rep(1, 6)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(-1, 6)), value = as.list(rep(1, 6)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 6)), value = as.list(rep(1, 5)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 6)), value = as.list(rep(1, 6)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 3)), value = list(list(1), list(2), list(3)))}
  expect_error(crossvalidationCP(Y = testY, estimator = testEstimator, param = as.list(1:3)))
  
  testEstimator <- function(Y, param, ...) {list(cps = list(10, 25, 40),
                                                 value = list(as.list(0:1), as.list(0:1), as.list(0:1)))}
  expect_identical(crossvalidationCP(Y = c(rep(0, 50), rep(1, 50)), estimator = testEstimator,
                                     param = as.list(1:3), folds = 2L), 2L)
  
  ## existing estimators
  set.seed(8346L)
  testY <- c(rnorm(25), rnorm(25, 10))
  ret <- crossvalidationCP(Y = testY, estimator = pelt, output = "detailed", param = list("SIC", 1e9), folds = 2)
  retCompare <- pelt(Y = testY, param = list("SIC", 1e9))
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_identical(ret$fit, compare)
  
  compareEst1 <- pelt(Y = testY[0:24 * 2 + 1], param = "SIC")
  # 13 == compareEst1$cps[[1]] -> 25
  compareEst2 <- pelt(Y = testY[0:24 * 2 + 2], param = "SIC")
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionL1loss(testset = testY[0:11 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 2], value = compareEst1$value[[1]][[2]]) + 
    criterionL1loss(testset = testY[0:11 * 2 + 1], value = compareEst2$value[[1]][[1]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 1], value = compareEst2$value[[1]][[2]])
  expect_equal(ret$CV[1], compare)
  
  
  ret <- crossvalidationCP(Y = testY, estimator = binseg, output = "detailed", param = list("SIC", 1e9), folds = 2)
  retCompare <- binseg(Y = testY, param = list("SIC", 1e9))
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- binseg(Y = testY[0:24 * 2 + 1], param = "SIC")
  # 13 == compareEst1$cps[[1]] -> 25
  compareEst2 <- binseg(Y = testY[0:24 * 2 + 2], param = "SIC")
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionL1loss(testset = testY[0:11 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 2], value = compareEst1$value[[1]][[2]]) + 
    criterionL1loss(testset = testY[0:11 * 2 + 1], value = compareEst2$value[[1]][[1]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 1], value = compareEst2$value[[1]][[2]])
  expect_equal(ret$CV[1], compare)
  
  ret <- crossvalidationCP(Y = 1:100, estimator = binseg,
                           param = list(list(penalty = "SIC", Q = 5), list(penalty = "SIC", Q = 20),
                                        1e9, list(penalty = 1e9, Q = 20)))
  expect_identical(ret, list(penalty = "SIC", Q = 20))
  
  
  ret <- crossvalidationCP(Y = testY, estimator = smuce, output = "detailed", param = list(0.01, 0.05, 0.5), folds = 2)
  retCompare <- smuce(Y = testY, param = 0.01)
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- smuce(Y = testY[0:24 * 2 + 1], param = 0.01)
  # 10, 13 == compareEst1$cps[[1]] -> 19, 25
  compareEst2 <- smuce(Y = testY[0:24 * 2 + 2], param = list(0.01, 0.05, 0.5))
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionL1loss(testset = testY[0:8 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionL1loss(testset = testY[9:11 * 2 + 2], value = compareEst1$value[[1]][[2]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 2], value = compareEst1$value[[1]][[3]]) + 
    criterionL1loss(testset = testY[0:11 * 2 + 1], value = compareEst2$value[[1]][[1]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 1], value = compareEst2$value[[1]][[2]])
  expect_equal(ret$CV[1], compare)

  
  set.seed(3955L)
  testY <- c(rnorm(25, 0, 0.1), rnorm(25, 10, 0.1))
  ret <- crossvalidationCP(Y = testY, estimator = fdrseg, output = "detailed", param = list(0.01, 0.05), folds = 2)
  retCompare <- fdrseg(Y = testY, param = 0.01)
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- fdrseg(Y = testY[0:24 * 2 + 1], param = 0.01)
  # 13 == compareEst1$cps[[1]] -> 25
  compareEst2 <- fdrseg(Y = testY[0:24 * 2 + 2], param = list(0.05, 0.5, 0.01))
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionL1loss(testset = testY[0:11 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 2], value = compareEst1$value[[1]][[2]]) + 
    criterionL1loss(testset = testY[0:11 * 2 + 1], value = compareEst2$value[[3]][[1]]) +
    criterionL1loss(testset = testY[12:24 * 2 + 1], value = compareEst2$value[[3]][[2]])
  expect_equal(ret$CV[1], compare)
  
  
  ret <- crossvalidationCP(Y = testY, estimator = wbs, output = "detailed", param = list(1.0, 1.3), folds = 2)
  compare <- list(cps = c(0, wbs(Y = testY, param = list(1.3, 1, 0.5))[[2]], length(testY)))
  expect_equal(ret$fit, compare)
  
  ret <- crossvalidationCP(Y = testY, output = "detailed", folds = 2, estimator = wbs, param = list(1.3, 1.5))$CV
  # 13 == wbs(Y = testY[0:24 * 2 + 1], param = 1.3)[[1]] -> 25
  compare <- criterionL1loss(testset = testY[0:11 * 2 + 2], estset = testY[0:12 * 2 + 1]) +
    criterionL1loss(testset = testY[12:24 * 2 + 2], estset = testY[13:24 * 2 + 1])
  # 12 == wbs(Y = testY[0:24 * 2 + 2], param = 1.3)[[1]] -> 24
  compare <- compare +
    criterionL1loss(testset = testY[0:11 * 2 + 1], estset = testY[0:11 * 2 + 2]) +
    criterionL1loss(testset = testY[12:24 * 2 + 1], estset = testY[12:24 * 2 + 2])
  expect_equal(ret[2], compare)
})


test_that("criterion is tested and works", {
  testY <- 1:100
  expect_identical(crossvalidationCP(Y = testY, criterion = criterionL1loss, output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  
  expect_error(crossvalidationCP(Y = testY, criterion = 1))
  testCriterion <- function() {NA}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion))
  testCriterion <- function(testset) {NA}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion))
  testCriterion <- function(testset, estset) {NA}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion))
  
  testCriterion <- function(testset, estset, value, ...) {"s"}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion))
  testCriterion <- function(testset, estset, value, ...) {1:2}
  expect_warning(crossvalidationCP(Y = testY, criterion = testCriterion))
  testCriterion <- function(testset, estset, value, ...) {NULL}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion))
  
  testCriterion <- function(testset, estset, value, ...) {1}
  expect_identical(crossvalidationCP(Y = testY, criterion = testCriterion), 0L)
  expect_identical(crossvalidationCP(Y = testY, criterion = testCriterion, output = "detailed")$CV, 1:6 * 5)
  
  set.seed(969564L)
  testY <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
  ret <- crossvalidationCP(Y = testY, output = "fit", criterion = criterionL2loss)$fit
  compare <- list(cps = c(0, optimalPartitioning(Y = testY, param = 3L)[[1]], length(testY)))
  expect_identical(ret, compare)

  testY <- rnorm(100)
  ret <- crossvalidationCP(Y = testY, output = "detailed", criterion = criterionMod)$CV
  compare <- criterionMod(testset = testY[0:19 * 5 + 1], estset = testY[-(0:19 * 5 + 1)]) +
    criterionMod(testset = testY[0:19 * 5 + 2], estset = testY[-(0:19 * 5 + 2)]) +
    criterionMod(testset = testY[0:19 * 5 + 3], estset = testY[-(0:19 * 5 + 3)]) +
    criterionMod(testset = testY[0:19 * 5 + 4], estset = testY[-(0:19 * 5 + 4)]) +
    criterionMod(testset = testY[0:19 * 5 + 5], estset = testY[-(0:19 * 5 + 5)])
  expect_equal(ret[1], compare)
  
  set.seed(3955L)
  testY <- c(rnorm(25, 0, 0.1), rnorm(25, 10, 0.1))
  ret <- crossvalidationCP(Y = testY, estimator = fdrseg, output = "detailed", param = list(0.01, 0.05), folds = 2,
                           criterion = criterionMod)
  retCompare <- fdrseg(Y = testY, param = 0.01)
  compare <- list(cps = c(0, retCompare[[1]][[1]], length(testY)), value = retCompare$value[[1]])
  expect_equal(ret$fit, compare)
  
  compareEst1 <- fdrseg(Y = testY[0:24 * 2 + 1], param = 0.01)
  # 13 == compareEst1$cps[[1]] -> 25
  compareEst2 <- fdrseg(Y = testY[0:24 * 2 + 2], param = list(0.05, 0.5, 0.01))
  # 12 == compareEst2$cps[[1]] -> 24
  compare <- criterionMod(testset = testY[0:11 * 2 + 2], value = compareEst1$value[[1]][[1]]) +
    criterionMod(testset = testY[12:24 * 2 + 2], value = compareEst1$value[[1]][[2]]) + 
    criterionMod(testset = testY[0:11 * 2 + 1], value = compareEst2$value[[3]][[1]]) +
    criterionMod(testset = testY[12:24 * 2 + 1], value = compareEst2$value[[3]][[2]])
  expect_equal(ret$CV[1], compare)
})

test_that("... is tested and works", {
  testY <- 1:100
  expect_identical(crossvalidationCP(Y = testY, output = "detailed", test = 1),
                   crossvalidationCP(Y = testY, output = "detailed"))
  
  expect_error(crossvalidationCP(Y = testY, output = "detailed", testset = 1))
  
  testEstimator <- function(Y, param, testvalue, ...) {list(cps = list(10, 25, 40),
                                                            value = list(list(testvalue, testvalue),
                                                                         as.list(0:1), as.list(0:1)))}
  expect_identical(crossvalidationCP(Y = testY, output = "detailed", param = as.list(1:3),
                                     estimator = testEstimator, testvalue = 10)$fit$value, list(10, 10))
  testCriterion <- function(value, testcrit, ...) {as.numeric(value != testcrit)}
  testEstimator <- function(Y, param, testvalue, ...) {list(cps = list(10, 25, 40),
                                                            value = list(as.list(0:1), as.list(0:1),
                                                                         list(testvalue, testvalue)))}
  expect_identical(crossvalidationCP(Y = testY, output = "detailed", param = as.list(1:3),
                                     estimator = testEstimator, testvalue = 10,
                                     criterion = testCriterion, testcrit = 10)$CV, c(10, 10, 0))
  
  set.seed(63894L)
  testY <- c(rnorm(50), rnorm(5, 3), rnorm(50))
  ret <- crossvalidationCP(Y = testY, estimator = pelt, param = list("SIC"), minseglen = 30, output = "detailed")
  expect_identical(ret$fit$cps, c(0, 105))
  
  ret <- crossvalidationCP(Y = testY, estimator = pelt, param = list("SIC"), output = "detailed")
  expect_identical(length(ret$fit$cps), 4L)
})

test_that("folds == 'COPPS' is working", {
  set.seed(1)
  testY <- rnorm(100)
  
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed"),
                   CV1(Y = testY, output = "detailed"))

  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", criterion = criterionL2loss),
                   COPPS(Y = testY, output = "detailed"))
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", criterion = criterionL1loss),
                   CV1(Y = testY, output = "detailed"))
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", criterion = criterionMod),
                   CVmod(Y = testY, output = "detailed"))
  
  testY <- 1:100
  expect_error(crossvalidationCP(Y = testY, folds = "COPPS", output = c("param", "fit")))
  expect_identical(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed")$param,
                   crossvalidationCP(Y = testY, folds = "COPPS"))
  

  set.seed(4336L)
  testY <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed"),
                   CV1(Y = testY, output = "detailed"))
  
  expect_error(suppressWarnings(crossvalidationCP(Y = testY, folds = "COPPS", param = "test")))
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", param = 2L),
                   CV1(Y = testY, output = "detailed", param = as.list(0:2)))
  
  testEstimator <- function(Y, param, ...) {NA}
  expect_error(crossvalidationCP(Y = testY, folds = "COPPS", estimator = testEstimator))
  testEstimator <- function(Y, param, ...) {list(cps = as.list(rep(5, 3)), value = list(list(1), list(2), list(3)))}
  expect_error(crossvalidationCP(Y = testY, folds = "COPPS", estimator = testEstimator, param = as.list(1:3)))
  testEstimator <- function(Y, param, ...) {list(cps = list(10, 25, 40),
                                                 value = list(as.list(0:1), as.list(0:1), as.list(0:1)))}
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed",
                                     estimator = testEstimator, param = as.list(1:3)),
                   CV1(Y = testY, output = "detailed", param = as.list(1:3), estimator = testEstimator))
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed",
                                     estimator = pelt, param = list("SIC", 1e9)),
                   CV1(Y = testY, output = "detailed", estimator = pelt, param = list("SIC", 1e9)))
  expect_equal(suppressWarnings(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", criterion = criterionMod,
                                                      estimator = binseg, param = list("SIC", 1e9))),
                   suppressWarnings(CVmod(Y = testY, output = "detailed", estimator = binseg, param = list("SIC", 1e9))))
  set.seed(1987654)
  ret <- crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", criterion = criterionL2loss,
                           estimator = fdrseg, param = list(0.1, 0.2))
  set.seed(1987654)
  expect_equal(ret, COPPS(Y = testY, output = "detailed", estimator = fdrseg, param = list(0.1, 0.2)))

  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", criterion = criterionL2loss,
                                     test = 1),
                   COPPS(Y = testY, output = "detailed"))
  expect_error(crossvalidationCP(Y = testY, folds = "COPPS", testset = 1:10))
  
  testEstimator <- function(Y, param, testvalue, ...) {list(cps = list(10, 25, 40),
                                                            value = list(list(testvalue, testvalue),
                                                                         as.list(0:1), as.list(0:1)))}
  expect_equal(crossvalidationCP(Y = testY, folds = "COPPS", output = "detailed", criterion = criterionL2loss,
                                     estimator = testEstimator, param = as.list(1:3), testvalue = 10),
                   COPPS(Y = testY, output = "detailed", param = as.list(1:3), estimator = testEstimator, testvalue = 10))
})

test_that("criterion is tested and works if folds == 'COPPS'", {
  testY <- 1:100
  expect_identical(crossvalidationCP(Y = testY, criterion = criterionL1loss, output = "detailed", folds = "COPPS"),
                   crossvalidationCP(Y = testY, output = "detailed", folds = "COPPS"))
  
  expect_error(crossvalidationCP(Y = testY, criterion = 1, folds = "COPPS"))
  testCriterion <- function() {NA}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion, folds = "COPPS"))
  testCriterion <- function(testset) {NA}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion, folds = "COPPS"))
  testCriterion <- function(testset, estset) {NA}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion, folds = "COPPS"))
  
  testCriterion <- function(testset, estset, value, ...) {"s"}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion, folds = "COPPS"))
  testCriterion <- function(testset, estset, value, ...) {1:2}
  expect_warning(crossvalidationCP(Y = testY, criterion = testCriterion, folds = "COPPS"))
  testCriterion <- function(testset, estset, value, ...) {NULL}
  expect_error(crossvalidationCP(Y = testY, criterion = testCriterion, folds = "COPPS"))
  
  testCriterion <- function(testset, estset, value, ...) {1}
  expect_identical(crossvalidationCP(Y = testY, criterion = testCriterion, folds = "COPPS"), 0L)
  expect_identical(crossvalidationCP(Y = testY, criterion = testCriterion, output = "detailed", folds = "COPPS")$CV, 1:6 * 2)
  
  set.seed(969564L)
  testY <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
  expect_equal(crossvalidationCP(Y = testY, output = "detailed", criterion = criterionL2loss, folds = "COPPS"),
                   COPPS(Y = testY, output = "detailed"))
  
  testY <- rnorm(100)
  expect_equal(crossvalidationCP(Y = testY, output = "detailed", criterion = criterionMod, folds = "COPPS"),
                  CVmod(Y = testY, output = "detailed"))
  
  set.seed(3955L)
  testY <- c(rnorm(25, 0, 0.1), rnorm(25, 10, 0.1))
  expect_equal(crossvalidationCP(Y = testY, estimator = fdrseg, output = "detailed", param = list(0.01, 0.05),
                                     folds = "COPPS", criterion = criterionMod),
                   CVmod(Y = testY, output = "detailed", estimator = fdrseg, param = list(0.01, 0.05)))
})

test_that("VfoldCV is working", {
  expect_error(VfoldCV())
  expect_error(VfoldCV(Y = NULL))
  expect_error(VfoldCV(Y = "s"))
  expect_error(VfoldCV(Y = rep("s", 20)))
  expect_error(VfoldCV(Y = as.list(rnorm(100))))
  expect_error(VfoldCV(Y = matrix(rnorm(100), 2, 50)))
  
  set.seed(267L)
  testY <- rnorm(100)
  expect_identical(VfoldCV(Y = testY, output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  
  expect_error(VfoldCV(Y = testY, output = c("param", "fit")))
  expect_equal(VfoldCV(Y = testY, output = "detailed")$param,
                   crossvalidationCP(Y = testY))
  
  set.seed(9653L)
  testY <- c(rnorm(50), rnorm(50, 5), rnorm(50), rnorm(50, 5))
  expect_equal(VfoldCV(Y = testY, output = "detailed"),
                   crossvalidationCP(Y = testY, output = "detailed"))
  
  expect_error(suppressWarnings(VfoldCV(Y = testY, Kmax = "test")))
  expect_equal(VfoldCV(Y = testY, output = "detailed", Kmax = 2L),
                   crossvalidationCP(Y = testY, output = "detailed", param = as.list(0:2)))
  
  expect_error(VfoldCV(Y = testY, V = matrix(1:4, 2, 2)))
  expect_equal(VfoldCV(Y = testY, output = "detailed", V = 2L),
                   crossvalidationCP(Y = testY, output = "detailed", folds = 2L))
  
  testEstimator <- function(Y, param, ...) {as.list(rep(5, 7))}
  expect_error(VfoldCV(Y = testY, estimator = testEstimator))
  
  testEstimator <- function(Y, param, ...) {list(cps = list(10, 25, 40),
                                                 value = list(as.list(0:1), as.list(0:1), as.list(0:1)))}
  expect_equal(VfoldCV(Y = testY, output = "detailed", V = 2L, Kmax = 2L, estimator = testEstimator),
                   crossvalidationCP(Y = testY, output = "detailed", folds = 2L, param = 2L, estimator = testEstimator))
  
  testCriterion <- function(testset, estset, value, ...) {1:2}
  expect_warning(VfoldCV(Y = testY, criterion = testCriterion))
  expect_equal(VfoldCV(Y = testY, output = "detailed", criterion = criterionL2loss),
                   crossvalidationCP(Y = testY, output = "detailed", criterion = criterionL2loss))
  expect_equal(VfoldCV(Y = testY, output = "detailed", V = 2L, criterion = criterionMod),
                   crossvalidationCP(Y = testY, output = "detailed", folds = 2L, criterion = criterionMod))
  
  expect_equal(VfoldCV(Y = testY, output = "detailed", test = 1L),
                   crossvalidationCP(Y = testY, output = "detailed"))
  expect_error(VfoldCV(Y = testY, output = "detailed", testset = 1))
  
  testCriterion <- function(value, testcrit, ...) {as.numeric(value != testcrit)}
  testEstimator <- function(Y, param, testvalue, ...) {list(cps = list(10, 25, 40),
                                                            value = list(as.list(0:1), as.list(0:1),
                                                                         list(testvalue, testvalue)))}
  expect_identical(VfoldCV(Y = testY, output = "detailed", Kmax = 2L,
                           estimator = testEstimator, testvalue = 10,
                           criterion = testCriterion, testcrit = 10)$CV, c(10, 10, 0))
})
