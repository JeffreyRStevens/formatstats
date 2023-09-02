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

#' Calculate and format mean and error
#'
#' `format_meanerror()` is a general function that allows you to either
#' automatically calculate mean and a measure of error from a data vector or
#' specify already calculated means and either an error interval or error
#' limits. Error measures include confidence intervals, standard deviation,
#' and standard error of the mean. Each of those has a specific function that
#' formats means and those error measures using APA (7th edition) style. So
#' `format_meanci()`, `format_meansd()`, and `format_meanse()` are wrappers
#' around `format_meanerror()` for specific error measures with a default
#' style. To just format the mean with no error, use `format_mean()`, another
#' wrapper around `format_meanerror()` that drops the error measure.
#'
#' @param x Numeric vector of data to calculate mean and error
#' @param error Character vector specifying error type ("ci" = confidence
#' interval, "se" = standard error of the mean, "sd" = standard deviation)
#' @param values Numeric vector of mean and interval or mean and lower and upper
#' limits
#' @param digits Number of digits after the decimal for means and error
#' @param meanlabel Formatting for mean label ("abbr" = M, "word" = Mean,
#' "none" = no label)
#' @param italics Logical for whether mean label should be italicized
#' @param subscript Character string to include as subscript with mean label
#' @param units Character string that gives units to include after mean value
#' @param display Character vector specifying how to display error ("limits" =
#' \[lower limit, upper limit\], "pm" = ±interval, "par" = (interval), "none" =
#' do not display error)
#' @param cilevel Numeric scalar from 0-1 defining confidence level
#' (defaults to 0.95)
#' @param errorlabel Logical for whether error label (e.g., 95% CI) should be
#' included
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return A character string of mean and error formatted in Markdown or LaTeX.
#' To return only the mean (no error), set `display = "none"`.
#' @export
#'
#' @examples
#' # Print mean and 95% confidence limits for fuel efficiency
#' format_meanci(mtcars$mpg)
#' # Print mean and standard deviation
#' format_meansd(mtcars$mpg)
#' # Print mean and standard error of the mean
#' format_meanse(mtcars$mpg)
#' # Print mean
#' format_mean(mtcars$mpg)
#' # Print mean and 95% confidence limits with no label for "95% CI"
#' format_meanci(mtcars$mpg, errorlabel = FALSE)
#' # Print mean and standard error of the mean as plus/minus interval
#' format_meanse(mtcars$mpg, error = "se", display = "pm")
#' # Print mean and 90% confidence limits with units
#' format_meanci(mtcars$mpg, units = "cm", cilevel = 0.9)
#' # Print three-digit mean with subscript in LaTeX
#' format_meanerror(mtcars$mpg, digits = 3, subscript = "control", display = "none", type = "latex")
format_meanerror <- function(x = NULL,
                              error = "ci",
                              values = NULL,
                              digits = 1,
                              meanlabel = "abbr",
                              italics = TRUE,
                              subscript = NULL,
                              units = NULL,
                              display = "limits",
                              cilevel = 0.95,
                              errorlabel = TRUE,
                              type = "md") {
  # Check arguments
  if (!is.null(x)) {
    stopifnot("Argument `x` must be a numeric vector." = is.numeric(x))
    stopifnot('Specify `error` as "ci", "sd", or "se".' = error %in% c("ci", "sd", "se"))
    xmean <- mean(x, na.rm = TRUE)
    xn <- sum(!is.na(x))
    stopifnot("Less than two non-missing values in vector, so no confidence interval can be computed." = xn > 1)
    xlimit <- 1- (1 - cilevel) / 2
    xsd <- stats::sd(x, na.rm = TRUE)
    xse <- xsd / sqrt(xn)
    xci <- stats::qt(xlimit, df = (xn - 1)) * xse
    xlower <- dplyr::case_when(identical(error, "ci") ~ xmean - xci,
                               identical(error, "sd") ~ xmean - xsd,
                               identical(error, "se") ~ xmean - xse)
    xupper <-  dplyr::case_when(identical(error, "ci") ~ xmean + xci,
                                identical(error, "sd") ~ xmean + xsd,
                                identical(error, "se") ~ xmean + xse)
    xinterval <- xmean - xlower
  } else if (!is.null(values)) {
    stopifnot("Argument `values` must be a numeric vector." = is.numeric(values))
    stopifnot("Argument `values` must be a vector with two or three elements." = length(values) %in% c(2, 3))
    if (length(values) == 2) {
      xmean <- values[1]
      xinterval <- values[2]
      xlower <- xmean - xinterval
      xupper <- xmean + xinterval
    } else {
      stopifnot("Argument `values` must include the mean followed by the lower CI limit then the upper CI limit." = values[1] >= values[2] & values[1] <= values[3])
      xmean <- values[1]
      xlower <- values[2]
      xupper <- values[3]
      xinterval <- xmean - xlower
    }
  } else {
    stop("You must include either the `x` or `values` argument.")
  }
  stopifnot('Specify `meanlabel` as "abbr", "word", or "none".' = meanlabel %in% c("abbr", "word", "none"))
  stopifnot("The `units` argument must be a character vector or NULL" = is.character(units) | is.null(units))
  stopifnot('Specify `display` as "limits", "pm", "par", or "none".' = display %in% c("limits", "pm", "par", "none"))

  # Build mean
  subname <- dplyr::case_when(!is.null(subscript) & identical(type, "md") ~ paste0("~", subscript, "~"),
                              !is.null(subscript) & identical(type, "latex") ~ paste0("_{", subscript, "}"),
                              .default = "")
  unit <- dplyr::case_when(!is.null(units) ~ paste0(" ", units),
                           .default = "")
  mean_lab <- dplyr::case_when(identical(meanlabel, "none") ~ "",
                               identical(meanlabel, "abbr") & italics & identical(type, "md") ~
                                 paste0("_M_", subname, " = "),
                               identical(meanlabel, "abbr") & italics & identical(type, "latex") ~
                                 paste0("$M", subname, "$ = "),
                               identical(meanlabel, "abbr") & !italics ~
                                 paste0("M", subname, " = "),
                               identical(meanlabel, "word") & italics & identical(type, "md") ~
                                 paste0("_Mean_", subname, " = "),
                               identical(meanlabel, "word") & italics & identical(type, "latex") ~
                                 paste0("$Mean", subname, "$ = "),
                               identical(meanlabel, "word") & !italics ~
                                 paste0("Mean", subname, " = "))
  full_mean <- paste0(mean_lab, format_num(xmean, digits = digits), unit)

  # Add error
  error_lab <- dplyr::case_when(!errorlabel ~ "",
                                identical(error, "ci") ~ paste0(cilevel * 100, "% CI "),
                                identical(error, "sd") ~ paste0("SD "),
                                identical(error, "se") ~ paste0("SE "))
  full_error <- dplyr::case_when(identical(display, "limits") ~ paste0(", ", error_lab, "[", format_num(xlower, digits = digits), ", ", format_num(xupper, digits = digits), "]"),
                                identical(display, "pm") ~ paste0(" \u00b1 ", format_num(xinterval, digits = digits)),
                                identical(display, "par") ~ paste0(" ", "(", format_num(xinterval, digits = digits), ")"),
                                .default = "")

  paste0(full_mean, full_error)

}

#' @rdname format_meanerror
#' @export
format_mean <- function(x = NULL,
                          values = NULL,
                          digits = 2,
                          meanlabel = "abbr",
                          italics = TRUE,
                          subscript = NULL,
                          units = NULL,
                          display = "none",
                          type = "md") {
  format_meanerror(x = x, values = values, digits = digits, meanlabel = meanlabel, italics = italics, subscript = subscript, units = units, display = display, type = type)
}

#' @rdname format_meanerror
#' @export
format_meanci <- function(x = NULL,
                          error = "ci",
                          values = NULL,
                          digits = 2,
                          meanlabel = "abbr",
                          italics = TRUE,
                          subscript = NULL,
                          units = NULL,
                          display = "limits",
                          cilevel = 0.95,
                          errorlabel = TRUE,
                          type = "md") {
  format_meanerror(x = x, error = error, values = values, digits = digits, meanlabel = meanlabel, italics = italics, subscript = subscript, units = units, display = display, cilevel = cilevel, errorlabel = errorlabel, type = type)
}

#' @rdname format_meanerror
#' @export
format_meanse <- function(x = NULL,
                          error = "se",
                          values = NULL,
                          digits = 2,
                          meanlabel = "abbr",
                          italics = TRUE,
                          subscript = NULL,
                          units = NULL,
                          display = "par",
                          errorlabel = TRUE,
                          type = "md") {
  format_meanerror(x = x, error = error, values = values, digits = digits, meanlabel = meanlabel, italics = italics, subscript = subscript, units = units, display = display, errorlabel = errorlabel, type = type)
}

#' @rdname format_meanerror
#' @export
format_meansd <- function(x = NULL,
                          error = "sd",
                          values = NULL,
                          digits = 2,
                          meanlabel = "abbr",
                          italics = TRUE,
                          subscript = NULL,
                          units = NULL,
                          display = "par",
                          errorlabel = TRUE,
                          type = "md") {
  format_meanerror(x = x, error = error, values = values, digits = digits, meanlabel = meanlabel, italics = italics, subscript = subscript, units = units, display = display, errorlabel = errorlabel, type = type)
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
