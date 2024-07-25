
<!-- README.md is generated from README.Rmd. Please edit that file -->

# choiceTools

<!-- badges: start -->
<!-- badges: end -->

The package contains some files that may make your live as a choice
modeller easier, especially if you use the r package `apollo`.

It contains the following functions:

- createSets: If you have a new dataset and want to understand how many
  people chose which alternative or want to get an overview of your
  choice sets, this function helps you. It creates a list in which each
  element is a choice set showing its alternatives with attributes and
  the frequencies and percentage values of chosen alternatives for each
  alternative.

- poetest: Conduct the test by Poe et al. (2005) with ease when you want
  to compare willingness to pay values from two models estimated with
  apollo

- apollo_ztest: Conduct a z test for all parameters between two models
  estimated with apollo in one line

- quicktexregapollo: If you want to create publication-ready tables with
  texreg, this function transform an apollo object into a texreg object

- subcoef and remGOF: make it easy to split your model parameters so
  that you can create publication-ready tables where columns represent
  different sets of estimated parameters from one model rather than all
  parameters in one row.

## Installation

You can install the stable version of choiceTools like so:

``` r
devtools::install_git('https://git.idiv.de/dj44vuri/choicetools')
```

You can install the development version of choiceTools like so:

``` r
devtools::install_git('https://git.idiv.de/dj44vuri/choicetools', ref = "devel" )
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(choiceTools)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
