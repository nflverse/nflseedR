# NFL team names and the conferences and divisions they belong to

NFL team names and the conferences and divisions they belong to

## Usage

``` r
divisions
```

## Format

A data frame with 36 rows and 4 variables containing NFL team level
information, including franchises in multiple cities:

- team:

  Team abbreviation

- conf:

  Conference abbreviation

- division:

  Division name

- sdiv:

  Division abbreviation

This data frame is created using the `teams_colors_logos` data frame of
the `nflfastR` package. Please see `data-raw/divisions.R` for the code
to create this data.

## Examples

``` r
str(divisions)
#> tibble [36 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ team    : chr [1:36] "ARI" "ATL" "BAL" "BUF" ...
#>  $ conf    : chr [1:36] "NFC" "NFC" "AFC" "AFC" ...
#>  $ division: chr [1:36] "NFC West" "NFC South" "AFC North" "AFC East" ...
#>  $ sdiv    : chr [1:36] "NFCW" "NFCS" "AFCN" "AFCE" ...
```
