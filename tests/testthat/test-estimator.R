context("convertSingleParam")

testRandomEstimator <- function(Y, param, ...) {
  sort(unique(as.integer(runif(3, 1, length(Y)))))
}

testRandomEstimatorValue <- function(Y, param, ...) {
  cps <- sort(unique(as.integer(runif(3, 1, length(Y)))))
  value <- 1:(length(cps) + 1) # TODO does this have to be a list such that it can be passed to crossvalidationCP?
  list(cps = cps, value = value)
}

test_that("testRandomEstimator is working", {
  set.seed(1)
  expect_identical(testRandomEstimator(rnorm(5), 2), c(1L, 3L))
  expect_identical(testRandomEstimator(rnorm(50), "test"), c(1L, 8L, 22L))
  
  set.seed(1)
  expect_identical(testRandomEstimatorValue(rnorm(5), 2), list(cps = c(1L, 3L), value = 1:3))
  expect_identical(testRandomEstimatorValue(rnorm(50), "test"), list(cps = c(1L, 8L, 22L), value = 1:4))
})

test_that("convertSingleParam is working", {
  set.seed(1)
  testEstimator <- convertSingleParam(testRandomEstimator)
  expect_identical(testEstimator(rnorm(100), 1:3), list(c(22L, 27L, 52L), c(18L, 27L, 52L), c(13L, 26L, 56L)))
  
  set.seed(1)
  testEstimator <- convertSingleParam(testRandomEstimatorValue)
  expect_identical(testEstimator(rnorm(100), 1:3), list(cps = list(c(22L, 27L, 52L),
                                                                   c(18L, 27L, 52L),
                                                                   c(13L, 26L, 56L)),
                                                        value = list(1:4, 1:4, 1:4)))
})

test_that("optimalPartitioning is working", {
  set.seed(1)
  expect_identical(optimalPartitioning(rnorm(100), c(3, 0, 1, 5)),
                   list(c(13, 14, 96), numeric(0), 96, c(66, 67, 70, 91, 96)))
})

test_that("pre-implemented change-point methods are working", {
  expect_identical(pelt(c(rep(0, 50), rep(1, 50)), param = list(0.5, "MBIC", 0.9)),
                   list(cps = list(50L, 50L, 50L), value = list(list(0, 1), list(0, 1), list(0, 1))))
  expect_equal(pelt(c(rep(0, 8), rep(1, 58)), param = list(0.5, "MBIC", 0.9), minseglen = 10),
                   list(cps = list(10L, integer(0), 10L), value = list(list(0.2, 1), list(mean(c(rep(0, 8), rep(1, 58)))),
                                                                       list(0.2, 1))))
  
  expect_identical(binseg(c(rep(0, 50), rep(1, 50)), param = list(0.5, "MBIC", 0.9)),
                   list(cps = list(50, 50, 50), value = list(list(0, 1), list(0, 1), list(0, 1))))
  expect_equal(binseg(c(rep(0, 8), rep(1, 58)), param = list(0.5, "MBIC", 0.9), minseglen = 9),
                   list(cps = list(10, numeric(0), 10), value = list(list(0.2, 1), list(mean(c(rep(0, 8), rep(1, 58)))),
                                                                     list(0.2, 1))))
  
  ret <- binseg(1:100, param = list(list(penalty = 0.005, Q = 3), list(penalty = "MBIC", Q = 2)))
  expect_identical(length(ret$cps[[1]]), 3L)
  expect_identical(length(ret$cps[[2]]), 2L)
  
  expect_warning(ret <- binseg(1:100, param = list(0.005, "MBIC"), Q = 8))
  expect_identical(length(ret$cps[[1]]), 8L)
  expect_identical(length(ret$cps[[2]]), 8L)
  
  set.seed(2)
  expect_identical(wbs(rnorm(100), param = list(0.9, 1, 1.3)),
                   list(c(25, 33, 43, 85, 92), c(43, 92), logical(0)))

  expect_identical(smuce(c(rep(0, 50), rep(1, 50)), param = list(0.5, 0.1, 0.9)),
                   list(cps = list(50L, 50L, 50L), value = list(list(0, 1), list(0, 1), list(0, 1))))
  expect_identical(smuce(c(rep(0, 50), rep(1, 50)), param = list(0.5, 0.1, 0.9), sd = 10),
                   list(cps = list(integer(0), integer(0), integer(0)), value = list(list(0.5), list(0.5), list(0.5))))
  
  set.seed(1)
  expect_identical(fdrseg(rnorm(100) + c(rep(0, 50), rep(1, 50)), param = list(0.5, 0.1, 0.9))$cps,
                   list(49L, 49L, 49L))
})
