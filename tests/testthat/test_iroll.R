
test_that("c_roll_min works as expected", {

  ix <- 1:10
  val <- as.numeric(1:10)
  expect_equal(iroll_min(val, ix, -1, -1), c(NA, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L))
  expect_equal(iroll_min(val, ix, -1, 0), c(1L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L))
  expect_equal(iroll_min(val, ix, -5, 0), c(1, 1, 1, 1, 1, 1, 2, 3, 4, 5))
  expect_equal(iroll_min(val, ix, -5, 0, left_open = T), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6))
  expect_equal(iroll_min(val, ix, -5, 0, left_open = T, right_open = T), c(NA, 1, 1, 1, 1, 2, 3, 4, 5, 6))

  expect_equal(iroll_min(val, ix, -3, -1), c(NA, 1, 1, 1, 2, 3, 4, 5, 6, 7))
  expect_equal(iroll_min(val, ix, -3, -1, left_open = T), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8))
  expect_equal(iroll_min(val, ix, -3, -1, left_open = T, right_open = T), c(NA, NA, 1, 2, 3, 4, 5, 6, 7, 8))
  expect_equal(iroll_min(val, ix, -3, -1, right_open = T), c(NA, NA, 1, 1, 2, 3, 4, 5, 6, 7))
  expect_equal(iroll_min(val, ix, -5, -3, left_open = T), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6))

  ix <- seq(as.POSIXct("2016-01-01", tz = "UTC"), as.POSIXct("2016-01-01 00:00:9", tz = "UTC"), by = "1 secs")
  val <- seq_along(ix)
  expect_equal(iroll_min(val, ix, -5, -0, left_open = T), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6))
  expect_equal(iroll_min(val, ix, -2, -1), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8))
  expect_equal(iroll_min(val, ix, -4, -3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6))

  ix2 <- c(ix, as.POSIXct(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20"), tz = "UTC"))
  ## ix2 <- c(0:9, 15, 17, 20)
  val2 <- seq_along(ix2)
  expect_equal(iroll_min(val2, ix2, -4, -0), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 11, 11, 12))
  expect_equal(iroll_min(val2, ix2, -3, -1, left_open = T), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8, NA, 11, NA))
  expect_equal(iroll_min(val2, ix2, -4, -3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6, NA, NA, 12))
  expect_equal(iroll_min(val2, ix2, -4, -4), c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6, NA, NA, NA))
  expect_equal(iroll_min(val2, ix2, -5, -5), c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5, NA, NA, 11))
  expect_equal(iroll_min(val2, ix2, -19, -19), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))
  expect_equal(iroll_min(val2, ix2, -200, -190), c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))

  ix <- c(1, 2, 2, 3, 4:9)
  val <- 1:10
  expect_equal(iroll_min(val, ix, -4, -0),  c(1, 1, 1, 1, 1, 1, 2, 4, 5, 6))
  expect_equal(iroll_min(val, ix, 1, 3, left_open = T), c(4, 5, 5, 6, 7, 8, 9, 10, NA, NA))
  expect_equal(iroll_min(val, ix, 2, 3), c(4, 5, 5, 6, 7, 8, 9, 10, NA, NA))
  expect_equal(iroll_max(val, ix, 2, 3), c(5, 6, 6, 7, 8, 9, 10, 10, NA, NA))
  expect_equal(iroll_min(val, ix, -2, -1), c(NA, 1, 1, 1, 2, 4, 5, 6, 7, 8))
  expect_equal(iroll_min(val, ix, -4, -3), c(NA, NA, NA, NA, 1, 1, 2, 4, 5, 6))

})

test_that("c_roll_max works as expected", {

  ix <- seq(as.POSIXct("2016-01-01", tz = "UTC"), as.POSIXct("2016-01-01 00:00:9", tz = "UTC"), by = "1 secs")
  ix2 <- c(ix, as.POSIXct(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20"), tz = "UTC"))
  ## ix2 <- c(0:9, 15, 17, 20)
  val2 <- seq_along(ix2)
  expect_equal(iroll_max(val2, ix2, -5, -0), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
  expect_equal(iroll_max(val2, ix2, -0, 1, left_open = T), c(2, 3, 4, 5, 6, 7, 8, 9, 10, NA, NA, NA, NA))
  expect_equal(iroll_max(val2, ix2, 1, 2), c(3, 4, 5, 6, 7, 8, 9, 10, 10, NA, 12, NA, NA))
  expect_equal(iroll_max(val2, ix2, -2, -1), c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, NA, 11, NA))
  expect_equal(iroll_max(val2, ix2, -4, -4), c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6, NA, NA, NA))
  expect_equal(iroll_max(val2, ix2, -5, -4), iroll_max(val2, ix2, -5, -3.001))
  expect_equal(iroll_max(val2, ix2, -5.1, -5), iroll_max(val2, ix2, -5, -5))
  expect_equal(iroll_max(val2, ix2, -5.1, -5), c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5, NA, NA, 11))
  expect_equal(iroll_max(val2, ix2, -19, -19), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))
  expect_equal(iroll_max(val2, ix2, -200, -190), c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                   NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))

})

test_that("iroll_prod works as expected", {
  ix <- seq(as.POSIXct("2016-01-01", tz = "UTC"), as.POSIXct("2016-01-01 00:00:9", tz = "UTC"), by = "1 secs")
  ix2 <- c(ix, as.POSIXct(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20"), tz = "UTC"))
  ## ix2 <- c(0:9, 15, 17, 20)
  val2 <- seq_along(ix2)
  expect_equal(iroll_prod(val2, ix2, -2, -0),
               c(1L, 2L, 6L, 24L, 60L, 120L, 210L, 336L, 504L, 720L, 11L, 132L, 13L))
  expect_equal(iroll_prod(val2, ix2, -0, 1, left_open = T), c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, NA, NA, NA, NA))
  expect_equal(iroll_prod(val2, ix2, 1, 2), c(6L, 12L, 20L, 30L, 42L, 56L, 72L, 90L, 10L, NA, 12L, NA, NA))
})

test_that("c_roll_max skips NAs", {
    ix <- seq(as.POSIXct("2016-01-01", tz = "UTC"), as.POSIXct("2016-01-01 00:00:9", tz = "UTC"), by = "1 secs")
    ix <- c(ix, as.POSIXct(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20"), tz = "UTC"))
    val <- seq_along(ix)
    val[c(2, 5, 11)] <- NA
    ## cbind(ix, val)
    expect_equal(iroll_max(val, ix, -4, 0), c(1, 1, 3, 4, 4, 6, 7, 8, 9, 10, NA, 12, 13))
    expect_equal(iroll_max(val, ix, -4, -1), c(NA, 1, 1, 3, 4, 4, 6, 7, 8, 9, NA, NA, 12))
})

test_that("c_roll_mean works as expected", {
    ix <- 1:10
    val <- 1:10
    expect_equal(iroll_mean(val, ix, -4, -0), c(1, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 8))
    expect_equal(iroll_mean(val, ix, -2, -1), c(NA, 1, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5))
    val[c(2, 5, 8)] <- NA
    expect_equal(iroll_mean(val, ix, -2, -1), c(NA, 1, 1, 3, 3.5, 4, 6, 6.5, 7, 9))
})


test_that("rolling output types are correct", {

  expect_type(iroll_min(c(T, T, F), 1:3, -2, -0), "logical")
  expect_type(iroll_max(c(T, T, F), 1:3, -2, -0), "logical")

  expect_type(iroll_min(1:3, 1:3, -2, -0), "integer")
  expect_type(iroll_max(1:3, 1:3, -2, -0), "integer")

  expect_type(iroll_last(1:3, 1:3, -2, -0), "integer")
  expect_type(iroll_first(1:3, 1:3, -2, -0), "integer")

  expect_type(iroll_prod(1:30, 1:30, -2, -0), "double")
  expect_type(iroll_prod(as.numeric(1:30), 1:30, -2, -0), "double")
  expect_type(iroll_prod(c(T, T, F), 1:3, -2, -0), "double")

})
