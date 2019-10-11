context("representation")

test_that("init", {
  method <- structure(list(), class = "foo")
  expect_error(init(method))
})

test_that("get_config", {
  method <- structure(list(), class = "foo")
  expect_equal(get_config(method), list())

  method <- structure(list(a = 1), class = "foo")
  expect_equal(get_config(method), list(a = 1))
})

test_that("set_config", {
  method <- structure(list(a = 1), class = "foo")

  expect_error(set_config(method, list(a = 1, b = 1)))

  expect_error(set_config(method, list(a = NULL)))
  expect_error(set_config(method, list(a = NA)))
  expect_error(set_config(method, list(a = NaN)))

  method <- set_config(method, list(a = 2))
  expect_equal(method, structure(list(a = 2), class = "foo"))

  method <- set_config(method, list(a = "foo"))
  expect_equal(method, structure(list(a = "foo"), class = "foo"))
})
