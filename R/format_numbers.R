#' Format numbers
#'
#' @param x Number
#' @param digits Number of digits after the decimal
#' @param pzero Logical indicator of whether to include leading zero numbers
#' less than 1
#'
#' @return
#' A character string formatting the number with specified number of digits
#' after the decimal.
#' @export
#'
#' @examples
#' format_num(pi, digits = 2)
#' format_num(pi, digits = 4)
format_num <- function(x, digits = 1, pzero = TRUE) {
  # Check arguments
  stopifnot("Input must be a numeric vector." = is.numeric(x))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = is.numeric(digits))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = digits >= 0)

  # Format number
  # char_num <- formatC(x, digits = digits, format = "f")
  if (!pzero) {
    sub("0\\.", "\\.", formatC(x, digits = digits, format = "f"))
  } else {
    formatC(x, digits = digits, format = "f")
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
