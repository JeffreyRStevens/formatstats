#' Print correlation statistics
#'
#' @param x Correlation object
#' @param digits Number of digits after the decimal for means, correlation
#' coefficient, confidence intervals, and degrees of freedom
#' @param pdigits Number of digits after the decimal for p-values (also controls
#' cutoff for small p-values)
#' @param pzero Logical indicator of whether to include leading zero for
#' p-values
#' @param ci Logical indicator of whether to print 95% confidence intervals
#' @param italics Logical for whether _p_ label should be italicized or not
#' @param dfs Formatting for degrees of freedom ("par" = parenthetical,
#' "sub" = subscript, "none" = do not print degrees of freedom)
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string of statistical information formatted in Markdown.
#' @export
#'
#' @family functions for printing statistics
#'
#' @examples
#' # Prepare data
#' beavers <- merge(beaver1, beaver2, by = "time")
#' beavers <- beavers[, c("time", "temp.x", "temp.y")]
#' beavers_corr <- cor.test(beavers$temp.x, beavers$temp.y)
#' # Print statistics
#' apa_corr(beavers_corr)
#' # Change digits
#' apa_corr(beavers_corr, digits = 3)
#' # Change cutoff digits for p-value
#' apa_corr(beavers_corr, pdigits = 2)
#' # Add leading zero to p-value and don't print confidence intervals
#' apa_corr(beavers_corr, pzero = TRUE, ci = FALSE)

apa_corr <- function(x,
                     digits = 2,
                     pdigits = 3,
                     pzero = FALSE,
                     ci = TRUE,
                     italics = TRUE,
                     dfs = "par",
                     type = "md") {
  # Format numbers
  corr <- format_num(x$estimate, digits = digits)
  cis <- format_num(x$conf.int, digits = digits)
  df <- format_num(x$parameter, digits = digits)
  pvalue <- format_p(x$p.value, pdigits = pdigits, pzero = pzero,
                     italics = italics, type = type)

  # Build label
  if (identical(type, "md") & italics) {
    r_lab <- paste0("_r_")
  } else if (identical(type, "latex") & italics) {
    r_lab <- paste0("$r$")
  } else if (!italics) {
    r_lab <- paste0("r")
  } else {
    stop("Wrong type specified.")
  }
  if (identical(dfs, "par")) {
    rlab <- paste0(r_lab, "(", df, ")")
  } else if (identical(dfs, "sub") & identical(type, "md")) {
    rlab <- paste0(r_lab, "~", df, "~")
  } else if (identical(dfs, "sub") & identical(type, "latex")) {
    rlab <- paste0(r_lab, "$_{", df, "}$")
  } else if (identical(dfs, "none")) {
    rlab <- r_lab
  } else {
    stop("Wrong dfs type specified.")
  }

  # Create statistics string
  if (ci) {
    paste0(rlab, " = ", corr, ", 95% CI [", cis[1], ", ", cis[2], "], ", pvalue)
  } else {
    paste0(rlab, " = ", corr, ", ", pvalue)
  }
}


#' Print t-test statistics
#'
#' @param x t-test object
#' @param digits Number of digits after the decimal for means, confidence
#' intervals, t-statistic, and degrees of freedom
#' @param pdigits Number of digits after the decimal for p-values (also controls
#' cutoff for small p-values)
#' @param pzero Logical indicator of whether to include leading zero for
#' p-values
#' @param full Logical indicator of whether to include means and confidence
#' intervals or just t-statistic and p-value
#' @param italics Logical for whether _p_ label should be italicized or not
#' @param dfs Formatting for degrees of freedom ("par" = parenthetical,
#' "sub" = subscript, "none" = do not print degrees of freedom)
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string of statistical information formatted in Markdown.
#' @export
#'
#' @family functions for printing statistics
#'
#' @examples
#' # Prepare data
#' beavers <- merge(beaver1, beaver2, by = "time")
#' beavers <- beavers[, c("time", "temp.x", "temp.y")]
#' beavers_tt <- t.test(beavers$temp.x, beavers$temp.y)
#' # Print statistics
#' apa_ttest(beavers_tt)
#' # Change digits
#' apa_ttest(beavers_tt, digits = 2)
#' # Change cutoff digits for p-value
#' apa_ttest(beavers_tt, pdigits = 2)
#' # Add leading zero to p-value and don't print confidence intervals
#' apa_ttest(beavers_tt, pzero = TRUE, full = FALSE)
apa_ttest <- function(x,
                      digits = 1,
                      pdigits = 3,
                      pzero = FALSE,
                      full = TRUE,
                      italics = TRUE,
                      dfs = "par",
                      type = "md") {
  # Format numbers
  if (length(x$estimate) == 2) {
    mean_val <- format_num(x$estimate[1] - x$estimate[2], digits = digits)
  } else if (length(x$estimate) == 1) {
    mean_val <- format_num(x$estimate, digits = digits)
  } else {
    stop("Too many values in the t-test estimate.")
  }
  cis <- format_num(x$conf.int, digits = digits)
  if (round(x$parameter, 1) == round(x$parameter)) {
    df <- format_num(x$parameter, digits = 0)
  } else {
    df <- format_num(x$parameter, digits = digits)
  }
  tstat <- format_num(x$statistic, digits = digits)
  pvalue <- format_p(x$p.value, pdigits = pdigits, pzero = pzero,
                     italics = italics, type = type)

  # Build label
  if (identical(type, "md") & italics) {
    t_lab <- paste0("_t_")
  } else if (identical(type, "latex") & italics) {
    t_lab <- paste0("$t$")
  } else if (!italics) {
    t_lab <- paste0("t")
  } else {
    stop("Wrong type specified.")
  }
  if (identical(dfs, "par")) {
    tlab <- paste0(t_lab, "(", df, ")")
  } else if (identical(dfs, "sub") & identical(type, "md")) {
    tlab <- paste0(t_lab, "~", df, "~")
  } else if (identical(dfs, "sub") & identical(type, "latex")) {
    tlab <- paste0(t_lab, "$_{", df, "}$")
  } else if (identical(dfs, "none")) {
    tlab <- t_lab
  } else {
    stop("Wrong dfs type specified.")
  }

  # Create statistics string
  if (full) {
    paste0("Mean = ", mean_val, ", 95% CI [", cis[1], ", ", cis[2], "], ", tlab, " = ", tstat, ", ", pvalue)
  } else {
    paste0(tlab, " = ", tstat, ", ", pvalue)
  }
}

