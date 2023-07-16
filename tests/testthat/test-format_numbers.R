test_that("format_num() rounds properly", {
  expect_equal(format_num(123.456), "123.5")
  expect_equal(format_num(123.456, digits = 2), "123.46")
})


test_that("format_p() works properly", {
  expect_equal(format_p(0.0012), "_p_ = .001")
  expect_equal(format_p(0.0012, pdigits = 2), "_p_ < .01")
  expect_equal(format_p(0.0012, pzero = TRUE), "_p_ = 0.001")
  expect_equal(format_p(0.0012, italics = FALSE), "p = .001")
  expect_equal(format_p(0.0012, type = "latex"), "$p$ = .001")
})


test_that("format_bf() works properly", {
  expect_equal(format_bf(123.4567), "_BF_~10~ = 123.5")
  expect_equal(format_bf(123.4567, digits1 = 2), "_BF_~10~ = 123.46")
  expect_equal(format_bf(1234.567), "_BF_~10~ = 1.2×10^3^")
  expect_equal(format_bf(1234.567, cutoff = 1000), "_BF_~10~ > 1000")
  expect_equal(format_bf(0.1234), "_BF_~10~ = 0.12")
  expect_equal(format_bf(0.1234, digits2 = 3), "_BF_~10~ = 0.123")
  expect_equal(format_bf(0.1234, italics = FALSE), "BF~10~ = 0.12")
  expect_equal(format_bf(0.1234, subscript = "01"), "_BF_~01~ = 0.12")
  expect_equal(format_bf(0.1234, subscript = ""), "_BF_ = 0.12")
  expect_equal(format_bf(0.1234, type = "latex"), "$BF_{10}$ = 0.12")
  expect_equal(format_bf(0.1234, type = "latex", italics = FALSE), "BF$_{10}$ = 0.12")
})


test_that("format_scientific() works properly", {
  expect_equal(format_scientific(0.00123), "1.2×10^-3^")
  expect_equal(format_scientific(0.00123, digits = 2), "1.23×10^-3^")
  expect_equal(format_scientific(0.00123, type = "latex"), "1.2 \\times 10^{-3}")
  expect_equal(format_scientific(0.00123, digits = 2, type = "latex"), "1.23 \\times 10^{-3}")
})
