#' Format numbers
#'
#' @param x Number
#' @param digits Number of digits after the decimal
#'
#' @return
#' A character string formatting the number with specified number of digits
#' after the decimal.
#' @export
#'
#' @examples
#' format_num(pi, digits = 2)
#' format_num(pi, digits = 4)
format_num <- function(x, digits = 1) {
  # Check arguments
  stopifnot("Input must be a numeric vector." = is.numeric(x))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = is.numeric(digits))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = digits >= 0)

  # Format number
  formatC(x, digits = digits, format = "f")
}


#' Format p-values
#'
#' @param x Number representing p-value
#' @param pdigits Number of digits after the decimal for p-values, ranging
#' between 1-5 (also controls cutoff for small p-values)
#' @param pzero Logical indicator of whether to include leading zero for
#' p-values
#' @param italics Logical for whether _p_ label should be italicized
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string that includes _p_ and then the p-value formatted in
#' Markdown or LaTeX. If p-value is below `pdigits` cutoff, `_p_ < <cutoff>` is
#' used.
#' @export
#'
#' @examples
#' format_p(0.001)
#' # Round digits for p-values greater than cutoff
#' format_p(0.111, pdigits = 2)
#' # Default cutoff is p < 0.001
#' format_p(0.0001)
#' # Set cutoff with pdigits
#' format_p(0.0001, pdigits = 2)
#' # Include leading zero
#' format_p(0.001, pzero = TRUE)
format_p <- function(x,
                     pdigits = 3,
                     pzero = FALSE,
                     italics = TRUE,
                     type = "md") {
  # Check arguments
  stopifnot("Input must be a numeric vector." = is.numeric(x))
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = is.numeric(pdigits))
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = pdigits > 0)
  stopifnot("Argument `pdigits` must be a numeric between 1 and 5." = pdigits < 6)
  stopifnot("Argument `pzero` must be TRUE or FALSE." = is.logical(pzero))
  stopifnot("Argument `italics` must be TRUE or FALSE." = is.logical(italics))
  stopifnot("Argument `type` must be 'md' or 'latex'." = type %in% c("md", "latex"))

  # Build label
  ## Format p
  p_lab <- dplyr::case_when(identical(type, "md") & italics ~ paste0("_p_"),
                            identical(type, "latex") & italics ~ paste0("$p$"),
                            identical(type, "md") & !italics ~ paste0("p"),
                            identical(type, "latex") & !italics ~ paste0("p"))
  ## Determine if using = or <
  cutoff <- as.numeric(paste0("1e-", pdigits))
  if (x < cutoff) {
    minp <- dplyr::case_when(pzero ~ as.character(as.numeric(paste0("1e-", pdigits))),
                             !pzero ~ sub("0.", ".", as.character(as.numeric(paste0("1e-", pdigits)))))
    paste0(p_lab, " < ", minp)
  } else {
    pvalue <- dplyr::case_when(pzero ~ format_num(x, digits = pdigits),
                               !pzero ~ sub("0.", ".", format_num(x, digits = pdigits)))
    paste0(p_lab, " = ", pvalue)
  }
}


#' Format Bayes factors
#'
#' @param x Number for Bayes factor or BayesFactor object
#' @param digits1 Number of digits after the decimal for Bayes factors > 1
#' @param digits2 Number of digits after the decimal for Bayes factors < 1
#' @param cutoff Cutoff for using `_BF_~10~ > <cutoff>` or
#' `_BF_~10~ < 1 / <cutoff>` (value must be > 1)
#' @param italics Logical for whether _BF_ label should be italicized
#' @param subscript Subscript to include with _BF_ label ("10", "01", or "" for
#' no subscript)
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string that includes _BF_~10~ and then the Bayes factor formatted
#' in Markdown or LaTeX. If Bayes factor is above or below `cutoff`,
#' `_BF_~10~ > <cutoff>` or `_BF_~10~ < 1 / <cutoff>` is used.
#' @export
#'
#' @examples
#' # Format Bayes factors > 1
#' format_bf(12.4444)
#' # By default, Bayes factors > 1000 will use scientific notation
#' format_bf(1244.44)
#' # Control digits for Bayes factors > 1 with digits1
#' format_bf(1244.44, digits1 = 3)
#' # Control cutoff for scientific notation with cutoff
#' format_bf(1244.44, cutoff = 10000)
#' # Format Bayes factors < 1
#' format_bf(0.111)
#' # By default, Bayes factors <_0.001 will use scientific notation
#' format_bf(0.0001)
#' # Control digits for Bayes factors < 1 with digits2
#' format_bf(0.111, digits2 = 3)
#' # Control cutoff for scientific notation with cutoff
#' format_bf(0.001, cutoff = 100)
format_bf <- function(x,
                      digits1 = 1,
                      digits2 = 2,
                      cutoff = NULL,
                      italics = TRUE,
                      subscript = "10",
                      type = "md") {
  # Check arguments
  if (is.numeric(x)) {
    bf <- x
  } else if (inherits(x, what = "BFBayesFactor")) {
    bf <- BayesFactor::extractBF(x)$bf
  } else {
    stop("Input is not numeric or of class BFBayesFactor.")
  }
  stopifnot("Argument `digits1` must be a non-negative numeric vector." = is.numeric(digits1))
  stopifnot("Argument `digits1` must be a non-negative numeric vector." = digits1 >= 0)
  stopifnot("Argument `digits2` must be a non-negative numeric vector." = is.numeric(digits2))
  stopifnot("Argument `digits2` must be a non-negative numeric vector." = digits2 >= 0)
  stopifnot("Argument `cutoff` must be a numeric vector greater than 1 or NULL." = (is.numeric(cutoff) & cutoff > 1) | is.null(cutoff))
  stopifnot("Argument `italics` must be TRUE or FALSE." = is.logical(italics))
  stopifnot("Argument `subscript` must be a character string (usually '10', '01', or '')." = is.character(subscript))
  stopifnot("Argument `type` must be 'md' or 'latex'." = type %in% c("md", "latex"))

  # Build label
  bf_lab <- dplyr::case_when(identical(type, "md") & italics ~ paste0("_BF_~", subscript, "~"),
                             identical(type, "md") & !italics ~ paste0("BF~", subscript, "~"),
                             identical(type, "latex") & italics ~ paste0("$BF_{", subscript, "}$"),
                             identical(type, "latex") & !italics ~ paste0("BF$_{", subscript, "}$"))
  bf_lab <- sub("~~", "", bf_lab)

  # Format Bayes factor
  if (is.null(cutoff)) {
    bf <- dplyr::case_when(bf >= 1000 ~ format_scientific(bf, digits = digits1, type = type),
                           bf <= 0.001 ~ format_scientific(bf, digits = digits1, type = type),
                           bf > 1 ~ format_num(bf, digits = digits1),
                           bf < 1 ~ format_num(bf, digits = digits2))
    paste0(bf_lab, " = ", bf)
  } else {
    if (bf > cutoff) {
      paste0(bf_lab, " > ", cutoff)
    } else if (bf <  1 / cutoff) {
      paste0(bf_lab, " < ", (1 / cutoff))
    } else {
      bf <- dplyr::case_when(bf > 1 ~ format_num(bf, digits = digits1),
                             .default = format_num(bf, digits = digits2))
      paste0(bf_lab, " = ", bf)
    }
  }
}

#' Calculate and format mean and confidence interval
#'
#' @param x Numeric vector of data to calculate mean and confidence interval
#' @param values Numeric vector of mean, lower CI, and upper CI to format
#' @param level Numeric scalar from 0-1 defining width of confidence interval
#' (defaults to 0.95)
#' @param digits Number of digits after the decimal for means and confidence
#' intervals
#' @param italics Logical for whether _p_ label should be italicized
#' @param mean Formatting for mean label ("abbr" = M, "word" = Mean)
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' @export
#'
#' @examples
format_meanci <- function(x = NULL,
                          values = NULL,
                          level = 0.95,
                          digits = 1,
                          italics = TRUE,
                          mean = "abbr",
                          type = "md") {
  if (!is.null(x)) {
    stopifnot("Argument `x` must be a numeric vector." = is.numeric(x))
    xmean <- mean(x, na.rm = TRUE)
    xn <- sum(!is.na(x))
    stopifnot("Less than two non-missing values in vector, so no confidence interval can be computed." = xn > 1)
    xlimit <- 1- (1 - level) / 2
    xsd <- stats::sd(x, na.rm = TRUE)
    xci <- stats::qt(xlimit, df = (xn - 1)) * xsd/ sqrt(xn)
    # xci <- ci(x, na.rm = TRUE)
    xlower <- xmean - xci
    xupper <-  xmean + xci
  } else if (!is.null(values)) {
    stopifnot("Argument `values` must be a numeric vector." = is.numeric(values))
    stopifnot("Argument `values` must be a vector with three elements." = length(values) == 3)
    stopifnot("Argument `values` must include the mean followed by the lower CI limit then the upper CI limit." = values[1] >= values[2] & values[1] <= values[3])
    xmean <- values[1]
    xlower <- values[2]
    xupper <- values[3]
  } else {
    stop("You must include either the `x` or `values` argument.")
  }
  mean_lab <- dplyr::case_when(identical(mean, "abbr") & italics & identical(type, "md") ~ "_M_ = ",
                               identical(mean, "abbr") & italics & identical(type, "latex") ~ "$M$ = ",
                               identical(mean, "abbr") & !italics ~ "M = ",
                               identical(mean, "word") & italics & identical(type, "md") ~ "_Mean_ = ",
                               identical(mean, "word") & italics & identical(type, "latex") ~ "$Mean$ = ",
                               identical(mean, "word") & !italics ~ "Mean = ")
  paste0(mean_lab, format_num(xmean, digits = digits), ", 95% CI [", format_num(xlower, digits = digits), ", ", format_num(xupper, digits = digits), "]")

}


#' Format numbers in scientific notation
#'
#' @param x Number
#' @param digits Number of digits after the decimal
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string of a number in scientific notation formatted in Markdown
#' or LaTeX.
#' @export
#'
#' @examples
#' format_scientific(1111)
#' # Control number of digits after decimal with digits
#' format_scientific(1111, digits = 3)
format_scientific <- function(x,
                              digits = 1,
                              type = "md") {
  # Check arguments
  stopifnot("Input must be a numeric vector." = is.numeric(x))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = is.numeric(digits))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = digits >= 0)
  stopifnot("Argument `type` must be 'md' or 'latex'." = type %in% c("md", "latex"))

  # Format number
  x <- formatC(x, digits = digits, format = "e")
  x <- gsub("e\\+00$", "", x)
  if (identical(type, "md")) {
    x <- gsub("e\\+0?(\\d+)", "\u00D710^\\1^", x)
    gsub("e\\-0?(\\d+)", "\u00D710^-\\1^", x)
  } else if (identical(type, "latex")) {
    x <- gsub("e\\+0?(\\d+)$", " \\\\times 10\\^\\{\\1\\}", x)
    gsub("e\\-0?(\\d+)$", " \\\\times 10\\^\\{-\\1\\}", x)
  }
}
