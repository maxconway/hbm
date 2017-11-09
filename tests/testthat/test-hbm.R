context("hbm")

test_that("erros correctly with 0 matrix", {
  m <- matrix(0,5,5)
  expect_error(hbm(m), 'Failed to find any clusters')
})

test_that("works correctly with 1 matrix", {
  m <- matrix(1,5,5)
  res <- hbm(m)
  expect_equal(res$hm, matrix(1,5,5) - diag(1,5,5))
})

test_that("works correctly with -1 matrix", {
  m <- matrix(-1,5,5)
  res <- hbm(m)
  expect_equal(res$hm, matrix(1,5,5) - diag(1,5,5))
})

test_that("works correctly with one row 0 matrix", {
  m <- matrix(1,5,5)
  m[1,] <- 0
  res <- hbm(m)
  expect_equal(res$hm, matrix(1,5,5) - diag(1,5,5))
})

test_that("works correctly with one row, one column 0 matrix", {
  m <- matrix(1,5,5)
  m[1,] <- 0
  m[,1] <- 0
  res <- hbm(m)
  
  expected <- matrix(1,5,5)
  expected[1,] <- 2
  expected[,1] <- 2
  diag(expected) <- 0
  
  expect_equal(res$hm, expected)
})

test_that('works correctly with fractionals', {
  m <- matrix(0.5,5,5)
  
  res <- hbm(m)
  
  expect_equal(res$hm, matrix(1,5,5) - diag(1,5,5))
})

test_that('works correctly with v.small', {
  m <- matrix(1e-20,5,5)
  
  res <- hbm(m)
  
  expect_equal(res$hm, matrix(1,5,5) - diag(1,5,5))
})

test_that('works correctly with v.mixed', {
  m <- matrix(1,5,5)
  m[upper.tri(m)] <- 1e6
  m[lower.tri(m)] <- 1e-6
  
  res <- hbm(m)
  
  expect_equal(res$hm, matrix(1,5,5) - diag(1,5,5))
})

test_that('works correctly with large, v.sparse', {
  m <- Matrix::Matrix(0, nrow = 1000, ncol = 1000, sparse = TRUE)
  m[250,500] <- 1
  
  expect_error(hbm(m), 'Failed to find any clusters')
  
})

test_that('boundary test for failure to cluster, too sparse', {
  zeroprop <- 0.95
  set.seed(123)
  m <- matrix(sample(x=c(0,1),size=25, replace=TRUE, prob=c(zeroprop, 1-zeroprop)),
              nrow = 5, ncol = 5)
  expect_error(hbm(m), 'Failed to find any clusters')
})

test_that('boundary test for failure to cluster, not too sparse', {
  zeroprop <- 0.9
  set.seed(123)
  m <- matrix(sample(x=c(0,1),size=25, replace=TRUE, prob=c(zeroprop, 1-zeroprop)),
              nrow = 5, ncol = 5)
  expect_silent(hbm(m))
})

test_that('boundary test for failure to cluster, too sparse, v.large', {
  zeroprop <- 0.999
  set.seed(123)
  m <- Matrix::Matrix(sample(x=c(0,1),size=500^2, replace=TRUE, prob=c(zeroprop, 1-zeroprop)),
              nrow = 500, ncol = 500, sparse = TRUE)
  expect_error(hbm(m), 'Failed to find any clusters')
})

test_that('boundary test for failure to cluster, too sparse, v.large', {
  zeroprop <- 0.99
  set.seed(123)
  m <- Matrix::Matrix(sample(x=c(0,1),size=500^2, replace=TRUE, prob=c(zeroprop, 1-zeroprop)),
                      nrow = 500, ncol = 500, sparse = TRUE)
  expect_silent(hbm(m))
})