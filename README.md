
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAMATO

Current version: 0.0.4

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/bishun945/DAMATO.svg?token=ekKczQU5ZnxJHkx55qWv&branch=master)](https://travis-ci.com/bishun945/DAMATO)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/ka7a4j64v11xhive?svg=true)](https://ci.appveyor.com/project/bishun945/damato)
[![CircleCI](https://circleci.com/gh/bishun945/DAMATO/tree/circleci-project-setup.svg?style=svg&circle-token=0e261ccfcb5335a249044d86bab7569c0131db7e)](https://circleci.com/gh/bishun945/DAMATO/tree/circleci-project-setup)
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
<liyunmei@njnu.edu.cn> and cc to <bishun1994@foxmail.com>.

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

# If you are getting start to data collection, use generage function like this:
Generate_ref_spread()
# Then a MS spreadsheet file will be created in fold 'spreads', just add your records to this file.


# Once you finish your collection, use Check functions to make sure the format is okay.
fn  = file.choose() # select your spreadsheet file
res_1 = Check_sheet_name(fn)
res_2 = Check_sample_id(res_1)
res_3 = Check_base_info(res_2)
res_4 = Check_geo_location(res_3)
res_5 = Check_param_quality(res_4) # coming soon
res_6 = Check_spectra_quality(res_5) # coming soon
```
