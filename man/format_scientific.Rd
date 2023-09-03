% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_numbers.R
\name{format_scientific}
\alias{format_scientific}
\title{Format numbers in scientific notation}
\usage{
format_scientific(x, digits = 1, type = "md")
}
\arguments{
\item{x}{Number}

\item{digits}{Number of digits after the decimal}

\item{type}{Type of formatting ("md" = markdown, "latex" = LaTeX)}
}
\value{
A character string of a number in scientific notation formatted in Markdown
or LaTeX.
}
\description{
Format numbers in scientific notation
}
\examples{
format_scientific(1111)
# Control number of digits after decimal with digits
format_scientific(1111, digits = 3)
}