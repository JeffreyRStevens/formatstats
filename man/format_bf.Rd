% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_stats.R
\name{format_bf}
\alias{format_bf}
\title{Format Bayes factors}
\usage{
format_bf(
  x,
  digits1 = 1,
  digits2 = 2,
  cutoff = NULL,
  italics = TRUE,
  subscript = "10",
  type = "md"
)
}
\arguments{
\item{x}{BayesFactor object or vector of numeric Bayes factor values}

\item{digits1}{Number of digits after the decimal for Bayes factors > 1}

\item{digits2}{Number of digits after the decimal for Bayes factors < 1}

\item{cutoff}{Cutoff for using \verb{_BF_~10~ > <cutoff>} or
\verb{_BF_~10~ < 1 / <cutoff>} (value must be > 1)}

\item{italics}{Logical for whether \emph{BF} label should be italicized}

\item{subscript}{Subscript to include with \emph{BF} label ("10", "01", or "" for
no subscript)}

\item{type}{Type of formatting ("md" = markdown, "latex" = LaTeX)}
}
\value{
A character string that includes \emph{BF}~10~ and then the Bayes factor formatted
in Markdown or LaTeX. If Bayes factor is above or below \code{cutoff},
\verb{_BF_~10~ > <cutoff>} or \verb{_BF_~10~ < 1 / <cutoff>} is used.
}
\description{
\code{format_bf()} can input either a \link{BayesFactor} object or a vectors of Bayes
factor values. By default, this function rounds Bayes factors greater than 1
to one decimal place and Bayes factors less than 1 to two decimal places.
Values greater than 1000 or less than 1/1000 are formatted using scientific
notation. Cutoffs can be set that format the values as greater than or less
than the cutoffs (e.g., BF > 1000 or BF < 0.001). Numbers of digits, cutoffs,
italics, and label subscripts are all customizable.
}
\examples{
# Format Bayes factors > 1
format_bf(12.4444)
# Bayes factors > 1000 will use scientific notation
format_bf(1244.44)
# Control digits for Bayes factors > 1 with digits1
format_bf(1244.44, digits1 = 3)
# Control cutoff for output
format_bf(1244.44, cutoff = 10000)
# Format Bayes factors < 1
format_bf(0.111)
# Bayes factors < 0.001 will use scientific notation
format_bf(0.0001)
# Control digits for Bayes factors < 1 with digits2
format_bf(0.111, digits2 = 3)
# Control cutoff for output
format_bf(0.001, cutoff = 100)
}
\seealso{
Other functions for printing statistical objects: 
\code{\link{format_corr}()},
\code{\link{format_ttest}()}
}
\concept{functions for printing statistical objects}
