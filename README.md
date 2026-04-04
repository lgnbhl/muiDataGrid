
<!-- README.md is generated from README.Rmd. Please edit that file -->

# muiDataGrid

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/muiDataGrid)](https://CRAN.R-project.org/package=muiDataGrid)
[![R-CMD-check](https://github.com/lgnbhl/muiDataGrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lgnbhl/muiDataGrid/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/@mui/x--data--grid-%5E8.28.1-blue.svg)](https://mui.com/x/react-data-grid/)
[![](https://img.shields.io/badge/react-18.3.1-blue.svg)](https://mui.com/x/react-data-grid/)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Follow-E4405F?style=social&logo=linkedin)](https://www.linkedin.com/in/FelixLuginbuhl)
<!-- badges: end -->

**muiDataGrid** gives access to [MUI X Data
Grid](https://mui.com/x/react-data-grid/), a fast and extensible React
data table and React data grid, with filtering, sorting, aggregation,
and more.

## Install

``` r
install.packages("muiDataGrid")
```

You can install the development version of muiDataGrid like so:

``` r
remotes::install_github("lgnbhl/muiDataGrid")
```

## Basic examples

A minimal example:

``` r
library(muiDataGrid)

DataGrid(
  rows = head(iris)
)
```

Customization can be done very easily:

``` r
library(muiDataGrid)
library(dplyr)

DataGrid(
  rows = starwars,
  columns = data.frame(
    field = c("name", "height", "mass"), # column names
    headerName = c("Names", "Height (cm)", "Mass (kg)"),
    description = c("Names of Starwars character", "Height in centimeter", "Mass in kilogram")
  ),
  initialState = list(
    pagination = list(
      paginationModel = list(pageSize = 5)
    )
  ),
  showToolbar = TRUE
)
```

**Read the full documentation with examples
[here](https://felixluginbuhl.com/muiDataGrid/).**

### Contribute

If you have any issue, question or want to contribute with a pull
request, don’t hesitate to write me on
<https://felixluginbuhl.com/muiDataGrid/>

For updates follow [Felix
Luginbuhl](https://linkedin.com/in/FelixLuginbuhl) on LinkedIn.
