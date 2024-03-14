#' Format numbers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @keywords internal
#' @examples
#' format_num(pi, digits = 2)
#' format_num(pi, digits = 4)
format_num <- function(x,
                       digits = 1,
                       pzero = TRUE) {
  lifecycle::deprecate_warn("0.0.0.9000", "format_num()", "cocoon::format_num()")
  # Check arguments
  stopifnot("Input must be a numeric vector." = is.numeric(x))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = is.numeric(digits))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = digits >= 0)

  # Format number
  dplyr::case_when(
    !pzero ~ sub("0\\.", "\\.", formatC(x, digits = digits, format = "f")),
    pzero ~ formatC(x, digits = digits, format = "f")
  )
}


#' Format numbers in scientific notation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @keywords internal
#' @examples
#' format_scientific(1111)
#' # Control number of digits after decimal with digits
#' format_scientific(1111, digits = 3)
format_scientific <- function(x,
                              digits = 1,
                              type = "md") {
  lifecycle::deprecate_warn("0.0.0.9000", "format_scientific()", "cocoon::format_scientific()")
  # Check arguments
  stopifnot("Input must be a numeric vector." = is.numeric(x))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = is.numeric(digits))
  stopifnot("Argument `digits` must be a non-negative numeric vector." = digits >= 0)
  stopifnot("Argument `type` must be 'md' or 'latex'." = type %in% c("md", "latex"))

  # Format number
  num <- formatC(x, digits = digits, format = "e")
  num <- gsub("e\\+00$", "", num)
  num <- dplyr::case_when(
    identical(type, "md") ~ gsub("e\\+0?(\\d+)", "\u00D710^\\1^", num),
    identical(type, "latex") ~ gsub("e\\+0?(\\d+)$", " \\\\times 10\\^\\{\\1\\}", num),
    .default = num
  )
  num <- dplyr::case_when(
    identical(type, "md") ~ gsub("e\\-0?(\\d+)", "\u00D710^-\\1^", num),
    identical(type, "latex") ~ gsub("e\\-0?(\\d+)$", " \\\\times 10\\^\\{-\\1\\}", num),
    .default = num
  )
  num
}

format_chr <- function(x,
                       italics = TRUE,
                       type = "md") {
  dplyr::case_when(
    italics & type == "md" ~ paste0("_", x, "_"),
    italics & type == "latex" ~ paste0("$", x, "$"),
    !italics ~ x
  )
}



format_sub <- function(subscript = NULL,
                       type = "md") {
  dplyr::case_when(
    subscript == "" ~ "",
    !is.null(subscript) & type == "md" ~ paste0("~", subscript, "~"),
    !is.null(subscript) & type == "latex" ~ paste0("$_{", subscript, "}$"),
    .default = ""
  )
}
