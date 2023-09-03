df <- data.frame(a = 1:10, b = 2:11)
test_corr <- cor.test(df$a, df$b)
test_ttest1 <- t.test(df$a, mu = 5)
test_ttest <- t.test(df$a, df$b)
test_ttest2 <- t.test(df$a, c(df$b, 120))

test_that("format_corr works properly", {
  suppressMessages(expect_error(format_corr("xxx"),
                                "Input must be a correlation object"))
  suppressMessages(expect_error(format_corr(test_corr, digits = "xxx"),
                                "Argument `digits` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_corr(test_corr, digits = -1),
                                "Argument `digits` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_corr(test_corr, pdigits = "xxx"),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_corr(test_corr, pdigits = 0),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_corr(test_corr, pdigits = 7),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_corr(test_corr, pzero = "xxx"),
                                "Argument `pzero` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_corr(test_corr, ci = "xxx"),
                                "Argument `ci` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_corr(test_corr, italics = "xxx"),
                                "Argument `italics` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_corr(test_corr, type = "xxx"),
                                "Argument `type` must be 'md' or 'latex'"))
  expect_equal(format_corr(test_corr), "_r_ = 1.00, 95% CI [1.00, 1.00], _p_ < .001")
  expect_equal(format_corr(test_corr, digits = 3), "_r_ = 1.000, 95% CI [1.000, 1.000], _p_ < .001")
  expect_equal(format_corr(test_corr, pdigits = 2), "_r_ = 1.00, 95% CI [1.00, 1.00], _p_ < .01")
  expect_equal(format_corr(test_corr, pzero = TRUE), "_r_ = 1.00, 95% CI [1.00, 1.00], _p_ < 0.001")
  expect_equal(format_corr(test_corr, ci = FALSE), "_r_ = 1.00, _p_ < .001")
  expect_equal(format_corr(test_corr, italics = FALSE), "r = 1.00, 95% CI [1.00, 1.00], p < .001")
  expect_equal(format_corr(test_corr, type = "latex"), "$r$ = 1.00, 95% CI [1.00, 1.00], $p$ < .001")
  expect_equal(format_corr(cor.test(df$a, df$b, method = "kendall")), "_τ_ = 1.00, _p_ < .001")
  expect_equal(format_corr(cor.test(df$a, df$b, method = "spearman")), "_ρ_ = 1.00, _p_ < .001")
})


test_that("format_ttest works properly", {
  suppressMessages(expect_error(format_ttest("xxx"),
                                "Input must be a correlation object"))
  suppressMessages(expect_error(format_ttest(test_ttest, digits = "xxx"),
                                "Argument `digits` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_ttest(test_ttest, digits = -1),
                                "Argument `digits` must be a non-negative numeric vector"))
  suppressMessages(expect_error(format_ttest(test_ttest, pdigits = "xxx"),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_ttest(test_ttest, pdigits = 0),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_ttest(test_ttest, pdigits = 7),
                                "Argument `pdigits` must be a numeric between 1 and 5"))
  suppressMessages(expect_error(format_ttest(test_ttest, pzero = "xxx"),
                                "Argument `pzero` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_ttest(test_ttest, full = "xxx"),
                                "Argument `full` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_ttest(test_ttest, italics = "xxx"),
                                "Argument `italics` must be TRUE or FALSE"))
  suppressMessages(expect_error(format_ttest(test_ttest, dfs = "xxx"),
                                "Argument `dfs` must be 'par', 'sub', or 'none'"))
  suppressMessages(expect_error(format_ttest(test_ttest, mean = "xxx"),
                                "Argument `mean` must be 'abbr' or 'word'"))
  suppressMessages(expect_error(format_ttest(test_ttest, type = "xxx"),
                                "Argument `type` must be 'md' or 'latex'"))
  expect_equal(format_ttest(test_ttest1), "_M_ = 5.5, 95% CI [3.3, 7.7], _t_(9) = 0.5, _p_ = .614")
  expect_equal(format_ttest(test_ttest), "_M_ = -1.0, 95% CI [-3.8, 1.8], _t_(18) = -0.7, _p_ = .470")
  expect_equal(format_ttest(test_ttest2), "_M_ = -11.3, 95% CI [-34.4, 11.8], _t_(10.2) = -1.1, _p_ = .302")
  expect_equal(format_ttest(test_ttest, digits = 2), "_M_ = -1.00, 95% CI [-3.84, 1.84], _t_(18) = -0.74, _p_ = .470")
  expect_equal(format_ttest(test_ttest, pdigits = 2), "_M_ = -1.0, 95% CI [-3.8, 1.8], _t_(18) = -0.7, _p_ = .47")
  expect_equal(format_ttest(test_ttest, pzero = TRUE), "_M_ = -1.0, 95% CI [-3.8, 1.8], _t_(18) = -0.7, _p_ = 0.470")
  expect_equal(format_ttest(test_ttest, full = FALSE), "_t_(18) = -0.7, _p_ = .470")
  expect_equal(format_ttest(test_ttest, italics = FALSE), "M = -1.0, 95% CI [-3.8, 1.8], t(18) = -0.7, p = .470")
  expect_equal(format_ttest(test_ttest, dfs = "sub"), "_M_ = -1.0, 95% CI [-3.8, 1.8], _t_~18~ = -0.7, _p_ = .470")
  expect_equal(format_ttest(test_ttest, dfs = "none"), "_M_ = -1.0, 95% CI [-3.8, 1.8], _t_ = -0.7, _p_ = .470")
  expect_equal(format_ttest(test_ttest, type = "latex"), "$M$ = -1.0, 95% CI [-3.8, 1.8], $t$(18) = -0.7, $p$ = .470")
  expect_equal(format_ttest(test_ttest, type = "latex", dfs = "sub"), "$M$ = -1.0, 95% CI [-3.8, 1.8], $t$$_{18}$ = -0.7, $p$ = .470")
  expect_equal(format_ttest(test_ttest, mean = "word"), "_Mean_ = -1.0, 95% CI [-3.8, 1.8], _t_(18) = -0.7, _p_ = .470")
})
