#' Format correlation statistics
#'
#' @param x Correlation object
#' @param digits Number of digits after the decimal for means, confidence
#' intervals, and correlation coefficients
#' @param pdigits Number of digits after the decimal for p-values, ranging
#' between 1-5 (also controls cutoff for small p-values)
#' @param pzero Logical indicator of whether to include leading zero for
#' p-values
#' @param ci Logical indicator of whether to print 95% confidence intervals
#' @param italics Logical for whether _p_ label should be italicized
#' @param dfs Formatting for degrees of freedom ("par" = parenthetical,
#' "sub" = subscript, "none" = do not print degrees of freedom)
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string of statistical information formatted in Markdown or LaTeX.
#' @export
#'
#' @family functions for printing statistics
#'
#' @examples
#' # Prepare data
#' mtcars_corr <- cor.test(mtcars$mpg, mtcars$disp)
#' # Print statistics
#' format_corr(mtcars_corr)
#' # Change digits
#' format_corr(mtcars_corr, digits = 3)
#' # Change cutoff digits for p-value
#' format_corr(mtcars_corr, pdigits = 2)
#' # Add leading zero to p-value and don't print confidence intervals
#' format_corr(mtcars_corr, pzero = TRUE, ci = FALSE)

format_corr <- function(x,
                        digits = 2,
                        pdigits = 3,
                        pzero = FALSE,
                        ci = TRUE,
                        italics = TRUE,
                        type = "md") {
  # Check arguments
  stopifnot("Input must be a correlation object." = inherits(x, what = "htest") && grepl("correlation", x$method))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = is.numeric(digits))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = digits >= 0)
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = is.numeric(pdigits))
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = pdigits > 0)
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = pdigits < 6)
  stopifnot("Argument `pzero` must be TRUE or FALSE." = is.logical(pzero))
  stopifnot("Argument `ci` must be TRUE or FALSE." = is.logical(ci))
  stopifnot("Argument `italics` must be TRUE or FALSE." = is.logical(italics))
  stopifnot("Argument `type` must be 'md' or 'latex'." = type %in% c("md", "latex"))

  # Format numbers
  corr_method <- dplyr::case_when(grepl("Pearson", x$method) ~ "pearson",
                                  grepl("Kendall", x$method) ~ "kendall",
                                  grepl("Spearman", x$method) ~ "spearman")
  corr <- format_num(x$estimate, digits = digits)
  if (corr_method == "pearson") {
    cis <- format_num(x$conf.int, digits = digits)
  } else {
    cis <- NULL
    ci <- FALSE
  }
  pvalue <- format_p(x$p.value, pdigits = pdigits, pzero = pzero,
                     italics = italics, type = type)

  # Build label
  r_lab <- dplyr::case_when(!italics & identical(corr_method, "pearson") ~ paste0("r"),
                            !italics & identical(corr_method, "kendall") & identical(type, "md")~ paste0("\u03C4"),
                            !italics & identical(corr_method, "kendall") & identical(type, "latex")~ paste0("\\tau"),
                            !italics & identical(corr_method, "spearman") & identical(type, "md")~ paste0("\u03C1"),
                            !italics & identical(corr_method, "spearman") & identical(type, "latex")~ paste0("\\rho"),
                            identical(corr_method, "pearson") & identical(type, "md") ~ paste0("_r_"),
                            identical(corr_method, "pearson") & identical(type, "latex") ~ paste0("$r$"),
                            identical(corr_method, "kendall") & identical(type, "md") ~ paste0("_\u03C4_"),
                            identical(corr_method, "kendall") & identical(type, "latex") ~ paste0("$\\tau$"),
                            identical(corr_method, "spearman") & identical(type, "md") ~ paste0("_\u03C1_"),
                            identical(corr_method, "spearman") & identical(type, "latex") ~ paste0("$\\rho$"),
  )

  # Create statistics string
  full_lab <- dplyr::case_when(ci & corr_method == "pearson" ~ paste0(r_lab, " = ", corr, ", 95% CI [", cis[1], ", ", cis[2], "], ", pvalue),
                               !ci ~ paste0(r_lab, " = ", corr, ", ", pvalue))
  return(full_lab)
}


#' Format t-test statistics
#'
#' @param x t-test object
#' @param digits Number of digits after the decimal for means, confidence
#' intervals, and t-statistics
#' @param pdigits Number of digits after the decimal for p-values, ranging
#' between 1-5 (also controls cutoff for small p-values)
#' @param pzero Logical indicator of whether to include leading zero for
#' p-values
#' @param full Logical indicator of whether to include means and confidence
#' intervals or just t-statistic and p-value
#' @param italics Logical for whether _p_ label should be italicized
#' @param ns Formatting for degrees of freedom ("par" = parenthetical,
#' "sub" = subscript, "none" = do not print degrees of freedom)
#' @param mean Formatting for mean label ("abbr" = M, "word" = Mean)
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string of statistical information formatted in Markdown or LaTeX.
#' @export
#'
#' @family functions for printing statistics
#'
#' @examples
#' # Prepare data
#' mtcars_tt <- t.test(formula = mtcars$mpg ~ mtcars$vs)
#' # Print statistics
#' format_ttest(mtcars_tt)
#' # Change digits
#' format_ttest(mtcars_tt, digits = 2)
#' # Change cutoff digits for p-value
#' format_ttest(mtcars_tt, pdigits = 2)
#' # Add leading zero to p-value and don't print confidence intervals
#' format_ttest(mtcars_tt, pzero = TRUE, full = FALSE)
format_ttest <- function(x,
                         digits = 1,
                         pdigits = 3,
                         pzero = FALSE,
                         full = TRUE,
                         italics = TRUE,
                         dfs = "par",
                         mean = "abbr",
                         type = "md") {
  # Check arguments
  stopifnot("Input must be a correlation object." = inherits(x, what = "htest") && grepl("t-test", x$method))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = is.numeric(digits))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = digits >= 0)
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = is.numeric(pdigits))
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = pdigits > 0)
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = pdigits < 6)
  stopifnot("Argument `pzero` must be TRUE or FALSE." = is.logical(pzero))
  stopifnot("Argument `full` must be TRUE or FALSE." = is.logical(full))
  stopifnot("Argument `italics` must be TRUE or FALSE." = is.logical(italics))
  stopifnot("Argument `dfs` must be 'par', 'sub', or 'none'." = dfs %in% c("par", "sub", "none"))
  stopifnot("Argument `mean` must be 'abbr' or 'word'." = mean %in% c("abbr", "word"))
  stopifnot("Argument `type` must be 'md' or 'latex'." = type %in% c("md", "latex"))

  # Format numbers
  if (length(x$estimate) == 2) {
    mean_val <- format_num(x$estimate[1] - x$estimate[2], digits = digits)
  } else if (length(x$estimate) == 1) {
    mean_val <- format_num(x$estimate, digits = digits)
  }
  cis <- format_num(x$conf.int, digits = digits)
  df <- dplyr::case_when(round(x$parameter, 1) == round(x$parameter) ~ format_num(x$parameter, digits = 0),
                         .default = format_num(x$parameter, digits = digits))
  tstat <- format_num(x$statistic, digits = digits)
  pvalue <- format_p(x$p.value, pdigits = pdigits, pzero = pzero,
                     italics = italics, type = type)

  # Build label
  t_lab <- dplyr::case_when(!italics ~ paste0("t"),
                            identical(type, "md") ~ paste0("_t_"),
                            identical(type, "latex") ~ paste0("$t$"))
  tlab <- dplyr::case_when(identical(dfs, "par") ~ paste0(t_lab, "(", df, ")"),
                           identical(dfs, "sub") & identical(type, "md") ~ paste0(t_lab, "~", df, "~"),
                           identical(dfs, "sub") & identical(type, "latex") ~ paste0(t_lab, "$_{", df, "}$"),
                           identical(dfs, "none") ~ t_lab)

  # Create statistics string
  if (full) {
    mean_lab <- dplyr::case_when(identical(mean, "abbr") & italics & identical(type, "md") ~ "_M_ = ",
                                 identical(mean, "abbr") & italics & identical(type, "latex") ~ "$M$ = ",
                                 identical(mean, "abbr") & !italics ~ "M = ",
                                 identical(mean, "word") & italics & identical(type, "md") ~ "_Mean_ = ",
                                 identical(mean, "word") & italics & identical(type, "latex") ~ "$Mean$ = ",
                                 identical(mean, "word") & !italics ~ "Mean = ")
    paste0(mean_lab, mean_val, ", 95% CI [", cis[1], ", ", cis[2], "], ", tlab, " = ", tstat, ", ", pvalue)
  } else {
    paste0(tlab, " = ", tstat, ", ", pvalue)
  }
}

