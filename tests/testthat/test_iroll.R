
test_that("c_roll_min works as expected", {
    ix <- 1:10
    val <- 1:10
    expect_equal(iroll_min(ix, val, -5, 0), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6))
    expect_equal(iroll_min(ix, val, -3, -1), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8))
    expect_equal(iroll_min(ix, val, -5, -3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6))
    dates <- seq(ymd("2016-01-01", tz = "UTC"), ymd_hms("2016-01-01 00:00:9"), by = "1 secs")
    val <- seq_along(dates)
    expect_equal(iroll_min(dates, val, -5, -0), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6))
    expect_equal(iroll_min(dates, val, -3, -1), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8))
    expect_equal(iroll_min(dates, val, -5, -3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6))
    dates2 <- c(dates, ymd_hms(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20")))
    val2 <- seq_along(dates2)
    expect_equal(iroll_min(dates2, val2, -5, -0), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 11, 11, 12))
    expect_equal(iroll_min(dates2, val2, -3, -1), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8, NA, 11, NA))
    expect_equal(iroll_min(dates2, val2, -5, -3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6, NA, NA, 12))
    expect_equal(iroll_min(dates2, val2, -5, -4), c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6, NA, NA, NA))
    expect_equal(iroll_min(dates2, val2, -6, -5), c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5, NA, NA, 11))
    expect_equal(iroll_min(dates2, val2, -20, -19), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))
    expect_equal(iroll_min(dates2, val2, -200, -190), c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                       NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
    ix <- c(1, 2, 2, 3, 4:9)
    val <- 1:10
    expect_equal(iroll_min(ix, val, -5, -0), c(1, 1, 1, 1, 1, 1, 2, 4, 5, 6))
    expect_equal(iroll_min(ix, val, 1, 3), c(4, 5, 5, 6, 7, 8, 9, 10, NA, NA))
    expect_equal(c_roll_max(ix, val, 1, 3), c(5, 6, 6, 7, 8, 9, 10, 10, NA, NA))
    expect_equal(iroll_min(ix, val, -3, -1), c(NA, 1, 1, 1, 2, 4, 5, 6, 7, 8))
    expect_equal(iroll_min(ix, val, -5, -3), c(NA, NA, NA, NA, 1, 1, 2, 4, 5, 6))
})

test_that("c_roll_max works as expected", {
    dates <- seq(ymd("2016-01-01", tz = "UTC"), ymd_hms("2016-01-01 00:00:9"), by = "1 secs")
    dates2 <- c(dates, ymd_hms(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20")))
    val2 <- seq_along(dates2)
    expect_equal(c_roll_max(dates2, val2, -5, -0), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
    expect_equal(c_roll_max(dates2, val2, -0, 1), c(2, 3, 4, 5, 6, 7, 8, 9, 10, NA, NA, NA, NA))
    expect_equal(c_roll_max(dates2, val2, -0, 2), c(3, 4, 5, 6, 7, 8, 9, 10, 10, NA, 12, NA, NA))
    expect_equal(c_roll_max(dates2, val2, -3, -1), c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, NA, 11, NA))
    expect_equal(c_roll_max(dates2, val2, -5, -4), c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6, NA, NA, NA))
    expect_equal(c_roll_max(dates2, val2, -5, -4), c_roll_max(dates2, val2, -5, -3.001))
    expect_equal(c_roll_max(dates2, val2, -5.1, -5), c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5, NA, NA, 11))
    expect_equal(c_roll_max(dates2, val2, -20, -19), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))
    expect_equal(c_roll_max(dates2, val2, -200, -190), c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                       NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
})

test_that("c_roll_max skips NAs", {
    dates <- seq(ymd("2016-01-01", tz = "UTC"), ymd_hms("2016-01-01 00:00:9"), by = "1 secs")
    dates <- c(dates, ymd_hms(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20")))
    val <- seq_along(dates)
    val[c(2, 5, 11)] <- NA
    cbind(dates, val)
    expect_equal(c_roll_max(dates, val, -5, 0), c(1, 1, 3, 4, 4, 6, 7, 8, 9, 10, NA, 12, 13))
    expect_equal(c_roll_max(dates, val, -5, -1), c(NA, 1, 1, 3, 4, 4, 6, 7, 8, 9, NA, NA, 12))
})

test_that("c_roll_mean works as expected", {
    ix <- 1:10
    val <- 1:10
    expect_equal(c_roll_mean(ix, val, -5, -0), c(1, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 8))
    expect_equal(c_roll_mean(ix, val, -3, -1), c(NA, 1, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5))
    val[c(2, 5, 8)] <- NA
    expect_equal(c_roll_mean(ix, val, -3, -1), c(NA, 1, 1, 3, 3.5, 4, 6, 6.5, 7, 9))
})
