
<!-- README.md is generated from README.Rmd. Please edit that file -->

# formatstats

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/JeffreyRStevens/formatstats/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JeffreyRStevens/formatstats?branch=main)
<!-- badges: end -->

The goal of formatstats is to provide functions that flexibly format
statistical output in a way that can be inserted into R Markdown
documents. This is analogous to the
[`apa_print()`](https://frederikaust.com/papaja_man/reporting.html#statistical-models-and-tests)
functions in the [papaja](https://github.com/crsh/papaja) package, but
functions in this package can print Markdown or LaTeX syntax. If your
output document is a PDF, this doesn’t matter. But if your output
document is a Word document (as required by many journal publishers),
Markdown syntax generates editable output instead of an image of output.
The default style for statistical output follows [American Psychological
Association style](https://apastyle.apa.org/), but some defaults can be
over-ridden to flexibly format output.

## Installation

You can install the development version of formatstats from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JeffreyRStevens/formatstats")
```

## Example

For an example, we’ll create a correlation from the `mtcars` data set.

``` r
library(formatstats)
(cars_corr <- cor.test(mtcars$mpg, mtcars$disp))
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  mtcars$mpg and mtcars$disp
#> t = -8.7472, df = 30, p-value = 9.38e-10
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.9233594 -0.7081376
#> sample estimates:
#>        cor 
#> -0.8475514
```

Now we can apply the `format_corr()` function to `cars_corr` to create a
Markdown-formatted character string for the statistical results. We can
embed this as inline R Markdown code to generate the results.

#### Code

`` Fuel efficiency and engine displacement were highly correlated (`r format_corr(cars_corr)`). ``

#### Output

Fuel efficiency and engine displacement were highly correlated (*r*(30)
= -0.85, 95% CI \[-0.92, -0.71\], *p* \< .001).

#### Other formatting

We can also format things like Bayes factors flexibly:

| Code                                               | Output                                  |
|----------------------------------------------------|-----------------------------------------|
| `format_bf(4321)`                                  | *BF*<sub>10</sub> = 4.3×10<sup>3</sup>  |
| `format_bf(4321, digits1 = 2)`                     | *BF*<sub>10</sub> = 4.32×10<sup>3</sup> |
| `format_bf(4321, italics = FALSE, subscript = "")` | BF = 4.3×10<sup>3</sup>                 |
| `format_bf(4321, cutoff = 1000)`                   | *BF*<sub>10</sub> \> 1000               |
|                                                    |                                         |
| `format_bf(0.04321)`                               | *BF*<sub>10</sub> = 0.04                |
| `format_bf(0.04321, digits2 = 3)`                  | *BF*<sub>10</sub> = 0.043               |
| `format_bf(0.04321, cutoff = 10)`                  | *BF*<sub>10</sub> \< 0.1                |

## Citation

To cite formatstats, use:

> Stevens, Jeffrey R. (2023). formatstats: Format and print statistical
> output. (version 0.0.0.9000)
> <https://github.com/JeffreyRStevens/formatstats>
