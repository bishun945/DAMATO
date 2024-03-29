
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAMATO <img src='man/figures/logo.png' align="right" height="139" />

Current version: 0.0.8

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/bishun945/DAMATO.svg?token=ekKczQU5ZnxJHkx55qWv&branch=master)](https://travis-ci.com/bishun945/DAMATO)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/ka7a4j64v11xhive?svg=true)](https://ci.appveyor.com/project/bishun945/damato)
[![CircleCI](https://circleci.com/gh/bishun945/DAMATO/tree/circleci-project-setup.svg?style=svg&circle-token=0e261ccfcb5335a249044d86bab7569c0131db7e)](https://circleci.com/gh/bishun945/DAMATO/tree/circleci-project-setup)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/DAMATO)](https://CRAN.R-project.org/package=DAMATO)
[![Launch
binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/bishun945/DAMATO/master)
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
<bishun1994@foxmail.com> and cc to <liyunmei@njnu.edu.cn>.

## Installation

You can install the released version of DAMATO from [My
Github](https://github.com/bishun945/DAMATO) with:

``` r
install.packages("devtools") # install devtools require
devtools::install_github('bishun945/DAMATO')
```

## Supported spread formats

run `data(DAMATO::dataset_format_1)` to see more details about one of
supported formats.

## Format check

If you are getting start to data collection, use generage function like
this:

``` r
library(DAMATO)
Generate_ref_spread()
```

Then a MS spreadsheet file will be created in fold `spreads` in your
working directory. Just add your records to this file. Once you finish
your collection, use `Check functions` to make sure the format is okay.

``` r

fn  = file.choose() # select your spreadsheet file
res_1 = Check_sheet_name(fn)
res_2 = Check_sample_id(res_1)
res_3 = Check_base_info(res_2)
res_4 = Check_geo_location(res_3)
```

The above `Check functions` are merged to one main function which could
be more convenient for users. But it is okay if you want to check a
specific item, such as `sheet name` or `sample id`.

It is better to use `%>%` for these successional functions. `res =
Check_sheet_name(fn) %>% Check_sample_id()`

``` r
res = Check_format(fn)
```

## Quatliy check (coming soon)

When all format stuff are passed, the more important thing is to check
the quality of data set you collect. Use `Check_param_quality()` and
`Check_spectra_quality()` to inspect (interactively to some extent) the
quality and flag them by `qualtiy_flag()`.

``` r
res_5 = Check_param_quality(res_4) # coming soon
res_6 = Check_spectra_quality(res_5) # coming soon
res_7 = Quatliy_flag(res_6) # coming soon
```

## Example steps by steps

### First thing to do

The first thing for DAMATO is creating a formatted spread if you have to
type your paper records in PC. Try run the following function:

``` r
Generate_ref_spread()
```

Then you will get a supper cool, formatted, and version controlled
spread in your current working directory named `spread`.

Now let us type our valuable data in it.

### Check\_sheet\_name

Once you have a spread need to be check, please use `Check_*` functions
to check their format. Basically, if you have typed your data in the
generated spread by running `Generate_ref_spread`, the format would be
pretty well.

``` r
fn = "/foo/bar/foobar.xlsx"
tmp <- Check_sheet_name(fn, excel_option = "readxl")
```

The result is shown as follows:

![ex1](./man/figures/Check_sheet_name_1.jpg)

> These warning messages mean the colnames in \[base\] sheet has some
> carriage returns or newlines. But this does not matter for the
> follow-up process. Thus, the number of warnings is 1.

### Check\_sample\_id - Error

``` r
fn = "/foo/bar/foobar.xlsx"
tmp <- Check_sheet_name(fn, excel_option = "readxl") %>%
  Check_sample_id()
```

![ex2](./man/figures/Check_sample_id.jpg)

> These messages mean that the first columns of head have been replaced
> by `SampleID` for matching the \[base\] sheet. However, in the \[apc\]
> sheet, several samples (masked) have been found in error SampleID
> format. Check the raw excel file and try to re-submit\! The final
> message of `Sample ID status:` presents `Error` which means there is
> unable to do the follow-up process.

### Check\_sample\_id - Pass

``` r
fn = "/foo/bar/foobar_revised.xlsx"
tmp <- Check_sheet_name(fn, excel_option = "readxl") %>%
  Check_sample_id()
```

![ex3](./man/figures/Check_sample_id_pass.jpg)

> Once you have revised those SampleID with inappropriate format, the
> check function will return the `Pass` information which means you can
> do next step now.

### Check\_base\_info - Error

``` r
fn = "/foo/bar/foobar_revised.xlsx"
tmp <- Check_sheet_name(fn, excel_option = "readxl") %>%
  Check_sample_id() %>%
  Check_base_info()
```

![ex4](./man/figures/Check_base_info_error.jpg) \> Okay, in this check
result, you gonna find some interesting features that if colnames of the
\[base\] sheet to be checked cannot match the designed format, i.e.,
`dataset_format = 1`, then `Check_base_info` would return warnings and
errors indicating which colnames could not be found or have possible
matchings (by means of the fuzzy matching).

It is recommended that you could manually inspect your spread and check
the colnames in the \[base\] sheet following the guides/suggestions from
`Try to find the matched pattern (just guess) ...`. If there are some
required colnames but do not exsit in the current spread, just
add/insert an empty column with those colnames.

And don’t remember those are just guess ;)

### Check\_base\_info - Pass

``` r
fn = "/foo/bar/foobar_revised_2.xlsx"
tmp <- Check_sheet_name(fn, excel_option = "readxl") %>%
  Check_sample_id() %>%
  Check_base_info()
```

![ex5](./man/figures/Check_base_info_pass.jpg)

> Boom\! After your second revision, the format in this spread has been
> passed now.

### Return of `Check_*` functions

Once you finish all format check functions, i.e., `Check_sheet_name`,
`Check_sample_id`, and `Check_base_info`, the final result of those
functions will return a list that includes many elements that could be
further used by upcoming functions or features as well as your own
decision.

The `Status_*` in that list all present `Pass` which means the list has
been checked by the three corresponding functions.

The data.frame of this list, namely `dt`, is restored data from the
spread in the filename `fn` but with formatted data.frame.

![ex6](./man/figures/Check_return.jpg)
