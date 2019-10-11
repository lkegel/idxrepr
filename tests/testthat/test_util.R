context("util")

test_that("write read float8", {
  fp <- tempfile()
  write_float8(c(1, 2, 3), fp, 3, append = F)
  write_float8(c(4, 5, 6), fp, 3, append = F)
  expect_equal(c(5, 6), read_float8(fp, size = 2, pos1 = 1))

  write_float8(c(7, 8, 9), fp, 3, append = T)
  expect_equal(c(6, 7, 8, 9), read_float8(fp, size = 4, pos1 = 2))

  expect_equal(c(8, 9), read_float8(fp, size = 2, pos1 = 2, pos2 = 2))

  unlink(fp)
})


test_that("write read uint2", {
  fp <- tempfile()
  write_uint2(c(1, 2, 10000), fp, 3, append = F)
  write_uint2(c(10001, 10002, 6), fp, 3, append = F)
  expect_equal(c(10002, 6), read_uint2(fp, size = 2, pos1 = 1))

  write_uint2(c(10003, 10004, 10005), fp, 3, append = T)
  expect_equal(c(6, 10003, 10004, 10005), read_uint2(fp, size = 4, pos1 = 2))

  expect_equal(c(10004, 10005), read_uint2(fp, size = 2, pos1 = 2, pos2 = 2))

  unlink(fp)
})

test_that("write read uint1", {
  fp <- tempfile()
  write_uint1(c(1, 2, 253), fp, 3, append = F)
  write_uint1(c(254, 255, 6), fp, 3, append = F)
  expect_equal(c(255, 6), read_uint1(fp, size = 2, pos1 = 1))

  write_uint1(c(7, 8, 9), fp, 3, append = T)
  expect_equal(c(6, 7, 8, 9), read_uint1(fp, size = 4, pos1 = 2))

  expect_equal(c(8, 9), read_uint1(fp, size = 2, pos1 = 2, pos2 = 2))

  unlink(fp)
})
