context("criteria")

testCriterionL2loss <- function(testset, estset) {
  sum((testset - mean(estset))^2)
}

testCriterionL1loss <- function(testset, estset) {
  sum(abs(testset - mean(estset)))
}

testCriterionMod <- function(testset, estset) {
  length(testset) / (length(testset) - 1) * sum((testset[-length(testset)] - mean(estset))^2)
}

test_that("criterionL2loss is working", {
  testtestset <- rnorm(43)
  testestset <- rnorm(34)
  expect_identical(criterionL2loss(testset = testtestset, estset = testestset),
                   testCriterionL2loss(testset = testtestset, estset = testestset))
  
  expect_identical(criterionL2loss(testset = testtestset, estset = testestset, value = NULL),
                   testCriterionL2loss(testset = testtestset, estset = testestset))
  expect_identical(criterionL2loss(testset = testtestset, estset = testestset, test = "s"),
                   testCriterionL2loss(testset = testtestset, estset = testestset))
  
  expect_identical(criterionL2loss(testset = rep(1, 10), estset = 0), 10)
  expect_identical(criterionL2loss(testset = 1:10, estset = 1:10), sum((1:10 - 5.5)^2))
  
  expect_identical(criterionL2loss(testset = rep(1, 10), estset = rnorm(10), value = 0), 10)
  expect_identical(criterionL2loss(testset = 1:10, estset = rnorm(10), value = 12.37), sum((1:10 - 12.37)^2))
})

test_that("criterionL1loss is working", {
  testtestset <- rnorm(43)
  testestset <- rnorm(34)
  expect_identical(criterionL1loss(testset = testtestset, estset = testestset),
                   testCriterionL1loss(testset = testtestset, estset = testestset))
  
  expect_identical(criterionL1loss(testset = testtestset, estset = testestset, value = NULL),
                   testCriterionL1loss(testset = testtestset, estset = testestset))
  expect_identical(criterionL1loss(testset = testtestset, estset = testestset, test = "s"),
                   testCriterionL1loss(testset = testtestset, estset = testestset))
  
  expect_identical(criterionL1loss(testset = rep(1, 10), estset = 0), 10)
  expect_identical(criterionL1loss(testset = 1:10, estset = 1:10), sum(abs(1:10 - 5.5)))
  
  expect_identical(criterionL1loss(testset = rep(1, 10), estset = rnorm(10), value = 0), 10)
  expect_identical(criterionL1loss(testset = 1:10, estset = rnorm(10), value = 12.37), sum(abs(1:10 - 12.37)))
})

test_that("criterionMod is working", {
  testtestset <- rnorm(43)
  testestset <- rnorm(34)
  expect_identical(criterionMod(testset = testtestset, estset = testestset),
                   testCriterionMod(testset = testtestset, estset = testestset))
  
  expect_identical(criterionMod(testset = 1, estset = 10), NaN)
  
  expect_identical(criterionMod(testset = testtestset, estset = testestset, value = NULL),
                   testCriterionMod(testset = testtestset, estset = testestset))
  expect_identical(criterionMod(testset = testtestset, estset = testestset, test = "s"),
                   testCriterionMod(testset = testtestset, estset = testestset))
  
  expect_identical(criterionMod(testset = rep(1, 10), estset = 0), 10)
  expect_identical(criterionMod(testset = 1:10, estset = 1:10), sum(10 / 9 * (1:9 - 5.5)^2))
  
  expect_identical(criterionMod(testset = rep(1, 10), estset = rnorm(10), value = 0), 10)
  expect_identical(criterionMod(testset = 1:10, estset = rnorm(10), value = 12.37), sum(10 / 9 * (1:9 - 12.37)^2))
})
