# Format Numerical Values to Special Percentage Strings

This function formats numeric vectors with values between 0 and 1 into
percentage strings with special specifications. Those specifications
are:

- 0 and 1 are converted to "0%" and "100%" respectively (takes machine
  precision into account)

- all other values \< 0.01 are converted to "\<1%"

- all other values between 0.01 and 0.995 are rounded to percentages
  without decimals

- values between 0.995 and 0.999 are rounded to percentages with 1
  decimal

- values between 0.999 and 1 are converted to "\>99.9%" unless closer to
  1 than machine precision.

## Usage

``` r
fmt_pct_special(x)
```

## Arguments

- x:

  A vector of numerical values

## Value

A character vector

## Examples

``` r
x <- c(0, 0.004, 0.009, 0.011, 0.9, 0.98, 0.994,
       .995, .9989, .999, .9991, .99999999)
fmt <- fmt_pct_special(x)
data.frame(x = x, fmt = fmt)
#>         x    fmt
#> 1  0.0000     0%
#> 2  0.0040    <1%
#> 3  0.0090    <1%
#> 4  0.0110     1%
#> 5  0.9000    90%
#> 6  0.9800    98%
#> 7  0.9940    99%
#> 8  0.9950  99.5%
#> 9  0.9989  99.9%
#> 10 0.9990  99.9%
#> 11 0.9991 >99.9%
#> 12 1.0000   100%
```
