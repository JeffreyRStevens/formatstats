
<!-- README.md is generated from README.Rmd. Please edit that file -->

# formatstat

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of formatstat is to provide functions that format statistical
output in a way that can be inserted into R Markdown documents. This is
analogous to the
[`apa_print()`](https://frederikaust.com/papaja_man/reporting.html#statistical-models-and-tests)
functions in the [papaja](https://github.com/crsh/papaja) package but
can print Markdown or LaTeX syntax. The defaults follow [American
Psychological Association style](https://apastyle.apa.org/), but some
defaults can be over-ridden.

## Installation

You can install the development version of formatstat from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JeffreyRStevens/formatstat")
```

## Example

For an example, we’ll create a data set from the `beavers1` and
`beavers2` data sets.

``` r
library(formatstat)
beavers <- merge(beaver1, beaver2, by = "time")
beavers <- beavers[, c("time", "temp.x", "temp.y")]
beavers_corr <- cor.test(beavers$temp.x, beavers$temp.y)
```

Now we can apply the `apa_ttest()` to `beavers_corr` to create a
Markdown- formatted character string for the statistical results. We can
embed this as inline R Markdown code to generate the results.

#### Code

`` `The temperature for the two beavers was highly correlated (_r_(97) = 0.42, 95% CI [0.24, 0.57], _p_ < .001). ``

#### Output

The temperature for the two beavers was highly correlated (*r*(97) =
0.42, 95% CI \[0.24, 0.57\], *p* \< .001).

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

To cite formatstat, use:

> Stevens, Jeffrey R. (2023). formatstat: Format and print statistical
> output. (version 0.0.0.9000)
> <https://github.com/JeffreyRStevens/formatstat>
