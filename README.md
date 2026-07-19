
<!-- README.md is generated from README.Rmd. Please edit that file -->

# muiDataGrid

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/muiDataGrid)](https://CRAN.R-project.org/package=muiDataGrid)
[![R-CMD-check](https://github.com/lgnbhl/muiDataGrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lgnbhl/muiDataGrid/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/@mui/x--data--grid-9.5.0-blue.svg)](https://mui.com/x/react-data-grid/)
[![](https://img.shields.io/badge/react-18.3.1-blue.svg)](https://mui.com/x/react-data-grid/)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Follow-E4405F?style=social&logo=linkedin)](https://www.linkedin.com/in/FelixLuginbuhl)
<!-- badges: end -->

**muiDataGrid** gives access to [MUI X Data
Grid](https://mui.com/x/react-data-grid/), a fast and extensible React
data table and React data grid, with filtering, sorting, pagination, and
more.

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
  columns = list(
    list(field = "name", headerName = "Names", description = "Names of Starwars character"),
    list(field = "height", headerName = "Height (cm)", description = "Height in centimeter"),
    list(field = "mass", headerName = "Mass (kg)", description = "Mass in kilogram")
  ),
  initialState = list(
    pagination = list(
      paginationModel = list(pageSize = 5)
    )
  ),
  showToolbar = TRUE
)
```

### Resources

- [Package documentation](https://felixluginbuhl.com/muiDataGrid/)
- \[All R
  examples\](<https://github.com/lgnbhl/muiDataGrid/tree/main/inst/examples>
- [Official MUI X Data Grid docs](https://mui.com/x/react-data-grid/)

### Acknowledgements

`muiDataGrid` is built on top of
[shiny.react](https://github.com/Appsilon/shiny.react), the R package by
[Appsilon](https://www.appsilon.com/) that makes it possible to use
React components in Shiny and Quarto.

### Contributing

If you have any issue, question or want to contribute with a pull
request, don’t hesitate to write me on
<https://github.com/lgnbhl/muiDataGrid/>

Follow [Felix Luginbuhl](https://linkedin.com/in/FelixLuginbuhl) on
LinkedIn for updates.

### Trademark notice

“MUI” and “MUI X Data Grid” are trademarks of MUI Inc. `muiDataGrid` is
an independent, community-maintained R package and is **not affiliated
with, sponsored by, or endorsed by MUI Inc.** The names are used solely
to identify the underlying JavaScript library that this package wraps.
See MUI’s [legal information](https://mui.com/legal/) for the canonical
list of MUI’s published policies.
