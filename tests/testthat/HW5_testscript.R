library(testthat)

if(!file.exists("R/HW5_Class.R")) {
  stop("cannot find file 'HW5_Class.R'")
}

source("R/HW5_Class.R")

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})


test_that("sparse add generic", {
  expect_true(isGeneric("sparse_add"))
})

test_that("sparse mult generic", {
  expect_true(isGeneric("sparse_mult"))
})

test_that("sparse sub generic", {
  expect_true(isGeneric("sparse_sub"))
})

test_that("sparse crossprod generic", {
  expect_true(isGeneric("sparse_crossprod"))
})

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})


# Homework 6

test_that("mean of all-zero sparse vector is zero", {
  x <- as(rep(0, 10), "sparse_numeric")
  expect_equal(mean(x), 0)
})

test_that("mean of mixed sparse vector is correct", {
  x <- as(c(0, 0, 4, 0, 2), "sparse_numeric")
  expect_equal(mean(x), 6/5)
})

test_that("mean of empty sparse vector returns NaN", {
  x <- new("sparse_numeric", value = numeric(), pos = integer(), length = 0L)
  expect_true(is.nan(mean(x)))
})

test_that("norm of all-zero sparse vector is zero", {
  x <- as(rep(0, 8), "sparse_numeric")
  expect_equal(norm(x), 0)
})

test_that("norm matches dense computation", {
  v <- c(0, 3, 0, -4, 0, 12)
  x <- as(v, "sparse_numeric")
  expect_equal(norm(x), sqrt(sum(v^2)))
})

test_that("norm works for one non-zero element", {
  x <- as(c(0, 5, 0, 0), "sparse_numeric")
  expect_equal(norm(x), 5)
})

test_that("standardize throws error for length 0 or 1", {
  x0 <- new("sparse_numeric", value = numeric(), pos = integer(), length = 0L)
  x1 <- new("sparse_numeric", value = 5, pos = 1L, length = 1L)
  
  expect_error(standardize(x0))
  expect_error(standardize(x1))
})

test_that("standardize throws error when sd is zero", {
  x <- as(rep(3, 8), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("standardize output has mean approximately 0", {
  v <- c(0, 5, 0, 10)
  x <- as(v, "sparse_numeric")
  z <- standardize(x)
  z_dense <- as(z, "numeric")
  
  expect_equal(mean(z_dense), 0, tolerance = 1e-10)
})

test_that("standardize output has sd approximately 1", {
  v <- c(0, 5, 0, 10)
  x <- as(v, "sparse_numeric")
  z <- standardize(x)
  z_dense <- as(z, "numeric")
  
  expect_equal(sd(z_dense), 1, tolerance = 1e-10)
})

test_that("standardize matches dense R implementation", {
  v <- c(2, 0, -3, 7, 0)
  x <- as(v, "sparse_numeric")
  
  z_sparse <- standardize(x)
  z_dense <- as(z_sparse, "numeric")
  z_true <- (v - mean(v)) / sd(v)
  
  expect_equal(z_dense, z_true, tolerance = 1e-10)
})

test_that("standardize result is still sparse_numeric", {
  x <- as(c(0, 5, 0, 10), "sparse_numeric")
  z <- standardize(x)
  expect_s4_class(z, "sparse_numeric")
})
