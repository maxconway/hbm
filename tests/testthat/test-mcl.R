context("mcl")

test_that("works correctly with 0 matrix, infl=2", {
  m <- matrix(0,5,5)
  res <- mcl(m, infl=2)
  expect_equal(res, 1:5)
})

test_that("works correctly with 1 matrix, infl=2", {
  m <- matrix(1,5,5)
  res <- mcl(m, infl=2)
  expect_equal(res, rep.int(1,5))
})

test_that("works correctly with -1 matrix, infl=2", {
  m <- matrix(-1,5,5)
  res <- mcl(m, infl=2)
  expect_equal(res, rep.int(1,5))
})

test_that("works correctly with 0 matrix, infl=3", {
  m <- matrix(0,5,5)
  res <- mcl(m, infl=3)
  expect_equal(res, 1:5)
})

test_that("works correctly with 1 matrix, infl=3", {
  m <- matrix(1,5,5)
  res <- mcl(m, infl=3)
  expect_equal(res, rep.int(1,5))
})

test_that("works correctly with -1 matrix, infl=3", {
  m <- matrix(-1,5,5)
  res <- mcl(m, infl=3)
  expect_equal(res, rep.int(1,5))
})
