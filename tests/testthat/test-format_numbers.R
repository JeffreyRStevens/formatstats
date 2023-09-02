test_that("format_num() rounds properly", {
  suppressMessages(expect_error(format_num("xxx"),
                                "Input must be a numeric vector"))
  suppressMessages(expect_error(format_num(123.456, digits = "xxx"),
                                "Argument `digits` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_num(123.456, digits = -1),
                                "Argument `digits` must be a non-negative numeric vector"))
  expect_equal(format_num(123.456), "123.5")
  expect_equal(format_num(123.456, digits = 2), "123.46")
})


test_that("format_p() works properly", {
  suppressMessages(expect_error(format_p("xxx"),
                                "Input must be a numeric vector"))
  suppressMessages(expect_error(format_p(0.0012, pdigits = "xxx"),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_p(0.0012, pdigits = 0),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_p(0.0012, pdigits = 7),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_p(0.0012, pzero = "xxx"),
                                "Argument `pzero` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_p(0.0012, italics = "xxx"),
                                "Argument `italics` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_p(0.0012, type = "xxx"),
                                "Argument `type` must be 'md' or 'latex'"))
  expect_equal(format_p(0.0012), "_p_ = .001")
  expect_equal(format_p(0.0012, pdigits = 2), "_p_ < .01")
  expect_equal(format_p(0.0012, pzero = TRUE), "_p_ = 0.001")
  expect_equal(format_p(0.0012, italics = FALSE), "p = .001")
  expect_equal(format_p(0.0012, type = "latex"), "$p$ = .001")
})


test_that("format_bf() works properly", {
  suppressMessages(expect_error(format_bf("0.0012"),
                                "Input is not numeric or of class BFBayesFactor"))
  suppressMessages(expect_error(format_bf(123.4567, digits1 = "xxx"),
                                "Argument `digits1` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_bf(123.4567, digits1 = -1),
                                "Argument `digits1` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_bf(123.4567, digits2 = "xxx"),
                                "Argument `digits2` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_bf(123.4567, digits2 = -1),
                                "Argument `digits2` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_bf(123.4567, cutoff = 0.5),
                                "Argument `cutoff` must be a numeric vector greater than 1 or NULL"))
  suppressMessages(expect_error(format_bf(123.4567, cutoff = "xxx"),
                                "Argument `cutoff` must be a numeric vector greater than 1 or NULL"))
  suppressMessages(expect_error(format_bf(123.4567, italics = "xxx"),
                                "Argument `italics` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_bf(123.4567, type = "xxx"),
                                "Argument `type` must be 'md' or 'latex'"))
  expect_equal(format_bf(123.4567), "_BF_~10~ = 123.5")
  expect_equal(format_bf(123.4567, digits1 = 2), "_BF_~10~ = 123.46")
  expect_equal(format_bf(1234.567), "_BF_~10~ = 1.2×10^3^")
  expect_equal(format_bf(1234.567, cutoff = 1000), "_BF_~10~ > 1000")
  expect_equal(format_bf(123.4567, cutoff = 1000), "_BF_~10~ = 123.5")
  expect_equal(format_bf(0.1234), "_BF_~10~ = 0.12")
  expect_equal(format_bf(0.001234, cutoff = 100), "_BF_~10~ < 0.01")
  expect_equal(format_bf(0.1234, cutoff = 1000), "_BF_~10~ = 0.12")
  expect_equal(format_bf(0.1234, digits2 = 3), "_BF_~10~ = 0.123")
  expect_equal(format_bf(0.1234, italics = FALSE), "BF~10~ = 0.12")
  expect_equal(format_bf(0.1234, subscript = "01"), "_BF_~01~ = 0.12")
  expect_equal(format_bf(0.1234, subscript = ""), "_BF_ = 0.12")
  expect_equal(format_bf(0.1234, type = "latex"), "$BF_{10}$ = 0.12")
  expect_equal(format_bf(0.1234, type = "latex", italics = FALSE), "BF$_{10}$ = 0.12")
  skip_on_cran()
  df <- data.frame(a = 1:10, b = c(1,3,2,4,6,5,7,8,10,9))
  test_corrbf <- BayesFactor::correlationBF(df$a, df$b)
  expect_equal(format_bf(test_corrbf), "_BF_~10~ = 123.3")
})

test_that("format_meanerror() works properly", {
  suppressMessages(expect_error(format_meanerror(x = "xxx"),
                                "Argument `x` must be a numeric vector"))
  suppressMessages(expect_error(format_meanerror(error = "xxx"),
                                'You must include either the `x` or `values` argument'))
  suppressMessages(expect_error(format_meanerror(x = 1:3, error = "xxx"),
                                'Specify `error` as "ci", "sd", or "se"'))
  suppressMessages(expect_error(format_meanerror(values = "xxx"),
                                "Argument `values` must be a numeric vector"))
  suppressMessages(expect_error(format_meanerror(values = 1:4),
                                "Argument `values` must be a vector with two or three elements"))
  suppressMessages(expect_error(format_meanerror(values = c(2, 4, 1)),
                                "Argument `values` must include the mean followed by the lower CI limit then the upper CI limit"))
  suppressMessages(expect_error(format_meanerror(x = 1:3, meanlabel = "xxx"),
                                'Specify `meanlabel` as "abbr", "word", or "none"'))
  suppressMessages(expect_error(format_meanerror(x = 1:3, units = 2),
                                "The `units` argument must be a character vector or NULL"))
  suppressMessages(expect_error(format_meanerror(x = 1:3, display = "xxx"),
                                'Specify `display` as "limits", "pm", "par", or "none"'))
  expect_equal(format_meanerror(x = 1:10), "_M_ = 5.5, 95% CI [3.3, 7.7]")
  expect_equal(format_meanerror(values = c(5.5, 1.2)), "_M_ = 5.5, 95% CI [4.3, 6.7]")
  expect_equal(format_meanerror(values = c(5.5, 1.2, 7.4)), "_M_ = 5.5, 95% CI [1.2, 7.4]")
  expect_equal(format_meanerror(x = 1:10, error = "sd"), "_M_ = 5.5, SD [2.5, 8.5]")
  expect_equal(format_meanerror(x = 1:10, error = "se"), "_M_ = 5.5, SE [4.5, 6.5]")
  expect_equal(format_meanerror(x = 1:10, digits = 2), "_M_ = 5.50, 95% CI [3.33, 7.67]")
  expect_equal(format_meanerror(x = 1:10, meanlabel = "word"), "_Mean_ = 5.5, 95% CI [3.3, 7.7]")
  expect_equal(format_meanerror(x = 1:10, meanlabel = "none"), "5.5, 95% CI [3.3, 7.7]")
  expect_equal(format_meanerror(x = 1:10, italics = FALSE), "M = 5.5, 95% CI [3.3, 7.7]")
  expect_equal(format_meanerror(x = 1:10, subscript = "test"), "_M_~test~ = 5.5, 95% CI [3.3, 7.7]")
  expect_equal(format_meanerror(x = 1:10, units = "cm"), "_M_ = 5.5 cm, 95% CI [3.3, 7.7]")
  expect_equal(format_meanerror(x = 1:10, display = "pm"), "_M_ = 5.5 ± 2.2")
  expect_equal(format_meanerror(x = 1:10, display = "par"), "_M_ = 5.5 (2.2)")
  expect_equal(format_meanerror(x = 1:10, cilevel = 0.9), "_M_ = 5.5, 90% CI [3.7, 7.3]")
  expect_equal(format_meanerror(x = 1:10, errorlabel = FALSE), "_M_ = 5.5, [3.3, 7.7]")
  expect_equal(format_meanerror(x = 1:10, type = "latex"), "$M$ = 5.5, 95% CI [3.3, 7.7]")
  expect_equal(format_mean(x = 1:10), "_M_ = 5.50")
  expect_equal(format_meanci(x = 1:10), "_M_ = 5.50, 95% CI [3.33, 7.67]")
  expect_equal(format_meansd(x = 1:10), "_M_ = 5.50 (3.03)")
  expect_equal(format_meanse(x = 1:10), "_M_ = 5.50 (0.96)")
})

test_that("format_scientific() works properly", {
  suppressMessages(expect_error(format_scientific("xxx"),
                                "Input must be a numeric vector"))
  suppressMessages(expect_error(format_scientific(123.456, digits = "xxx"),
                                "Argument `digits` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_scientific(123.456, digits = -1),
                                "Argument `digits` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_bf(123.4567, type = "xxx"),
                                "Argument `type` must be 'md' or 'latex'"))
  expect_equal(format_scientific(1000), "1.0×10^3^")
  expect_equal(format_scientific(0.00123), "1.2×10^-3^")
  expect_equal(format_scientific(0.00123, digits = 2), "1.23×10^-3^")
  expect_equal(format_scientific(0.00123, type = "latex"), "1.2 \\times 10^{-3}")
  expect_equal(format_scientific(0.00123, digits = 2, type = "latex"), "1.23 \\times 10^{-3}")
})
