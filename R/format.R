#' Format numbers
#'
#' @param x Number
#' @param digits Number of digits after the decimal
#'
#' @return
#' A character string formatting the number
#' @export
#'
#' @examples
#' format_num(pi, digits = 2)
#' format_num(pi, digits = 4)
format_num <- function(x, digits) {
  format(x, digits = 1, nsmall = digits)
}


#' Format p-values
#'
#' @param x Number representing p-value
#' @param pdigits Number of digits after the decimal for p-values (also controls
#' cutoff for small p-values)
#' @param pzero Logical indicator of whether to include leading zero for
#' p-values
#' @param italics Logical for whether _p_ label should be italicized or not
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string that includes _p_ and then the p-value formatted in
#' Markdown. If p-value is below `pdigits` cutoff, `_p_ < <cutoff>` is used.
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
  cutoff <- as.numeric(paste0("1e-", pdigits))
  # Build label
  if (identical(type, "md") & italics) {
    p_lab <- paste0("_p_")
  } else if (identical(type, "latex") & italics) {
    p_lab <- paste0("$p$")
  } else if (!italics) {
    p_lab <- paste0("p")
  } else {
    stop("Wrong type specified.")
  }
  if (x < cutoff) {
    if (pzero) {
      minp <- as.numeric(paste0("1e-", pdigits))
    } else {
      minp <- sub("0.", ".", as.character(as.numeric(paste0("1e-", pdigits))))
    }
    paste0(p_lab, " < ", minp)
  } else {
    if (pzero) {
      pvalue <- format_num(x, digits = pdigits)
    } else {
      pvalue <- sub("0.", ".", format_num(x, digits = pdigits))
    }
    paste0(p_lab, " = ", pvalue)
  }
}


#' Format Bayes factors
#'
#' @param x Number for Bayes factor or BayesFactor object
#' @param digits1 Number of digits after the decimal for Bayes factors > 1
#' @param digits2 Number of digits after the decimal for Bayes factors < 1
#' @param cutoff Cutoff for using `_BF_~10~ > <cutoff>` or
#' `_BF_~10~ < 1 / <cutoff>` (value must be > 0)
#' @param italics Logical for whether _BF_ label should be italicized or not
#' @param subscript Subscript to include with _BF_ label ("10", "01", or "" for
#' no subscript)
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string that includes _BF_~10~ and then the Bayes factor formatted
#' in Markdown. If Bayes factor is above or below `cutoff`,
#' `_BF_~10~ > <cutoff>` or `_BF_~10~ < <cutoff>` is used.
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
  # Check if object is numeric, BFBayesFactor, or other
  if (is.numeric(x)) {
    bf <- x
  } else if (inherits(x, what = "BFBayesFactor")) {
    bf <- BayesFactor::extractBF(x)$bf
  } else {
    stop("Object is not numeric or of class BFBayesFactor.")
  }
  # Build label
  if (identical(type, "md") & italics) {
    bf_lab <- paste0("_BF_~", subscript, "~")
  } else if (identical(type, "md") & !italics) {
    bf_lab <- paste0("BF~", subscript, "~")
  } else if (identical(type, "latex") & italics) {
    bf_lab <- paste0("$BF_{", subscript, "}$")
  } else if (identical(type, "latex") & !italics) {
    bf_lab <- paste0("BF$_{", subscript, "}$")
  } else {
    stop("Wrong type specified.")
  }
  bf_lab <- sub("~~", "", bf_lab)
  # Format Bayes factor
  if (is.null(cutoff)) {
    if (bf > 1000 | bf < 0.001) {
      bf <- format_scientific(bf, digits = digits1, type = type)
    } else {
      if (bf > 1) {
        bf <- format_num(bf, digits = digits1)
      } else {
        bf <- format_num(bf, digits = digits2)
      }
    }
    paste0(bf_lab, " = ", bf)
  } else {
    if (bf > cutoff) {
      paste0(bf_lab, " > ", cutoff)
    } else if (bf <  1 / cutoff) {
      paste0(bf_lab, " < ", (1 / cutoff))
    } else {
      if (bf > 1) {
        bf <- format_num(bf, digits = digits1)
      } else {
        bf <- format_num(bf, digits = digits2)
      }
      paste0(bf_lab, " = ", bf)
    }
  }
}

#' Format numbers in scientific notation
#'
#' @param x Number
#' @param digits Number of digits after the decimal
#' @param type Type of formatting ("md" = markdown, "latex" = LaTeX)
#'
#' @return
#' A character string of a number in scientific notation formatted in Markdown
#' @export
#'
#' @examples
#' format_scientific(1111)
#' # Control number of digits after decimal with digits
#' format_scientific(1111, digits = 3)
format_scientific <- function(x, digits = 1, type = "md") {
  x <- format(x, digits = digits + 1, nsmall = digits, scientific = TRUE)
  x <- gsub("e\\+00$", "", x)
  if (identical(type, "md")) {
    x <- gsub("e\\+0?(\\d+)", "\u00D710^\\1^", x)
    x <- gsub("e\\-0?(\\d+)", "\u00D710^-\\1^", x)
  } else if (identical(type, "latex")) {
    x <- gsub("e\\+0?(\\d+)$", " \\\\times 10\\^\\{\\1\\}", x)
    x <- gsub("e\\-0?(\\d+)$", " \\\\times 10\\^\\{-\\1\\}", x)
  } else {
    stop("Wrong type specified.")
  }
  x
}
