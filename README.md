
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAMATO

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/bishun945/DAMATO.svg?branch=master)](https://travis-ci.com/bishun945/DAMATO)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/bishun945/DAMATO?branch=master&svg=true)](https://ci.appveyor.com/project/bishun945/DAMATO)
<!-- badges: end -->

The goal of `DAMATO` is to automatically assess the MS spreadsheets
recording in situ data by:

  - **Format check** includes sheetname check, sampleID check, base info
    check, etc.;
  - **Generate a skeleton** by the pre-defined spreadsheet format (easy
    to use when start to data collection);
  - **Produce assessment figures** includes parameters comparison,
    relationships between AOPs (or IOPs) and OACs, geolocation map and
    so on.

The present built-in spreadsheet format only supports the lab of
Prof. Yunmei Li in Nanjing Normal University. If you want to customize
the spreadsheet format, you can consult Prof. Li for more details via
<liyunmei@njnu.edu.cn> and cc to <bishun1994@foxmail>.

## Installation

You can install the released version of DAMATO from [My
Github](https://github.com/bishun945/DAMATO) with:

``` r
install.packages("devtools") # install devtools require
devtools::install_github('DAMATO', build_vignettes=TRUE)
```

## Example

This is a basic example which shows you how to use `DAMATO`:

``` r
library(DAMATO)
## basic example code
```
