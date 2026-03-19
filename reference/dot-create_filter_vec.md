# worker function to create filtering vector

worker function to create filtering vector

## Usage

``` r
.create_filter_vec(object, checks, slt = c("det", "dep", "tag", "ani", "obs"))
```

## Arguments

- object:

  an ATO

- checks:

  a list of column names and respective values to filter with

- slt:

  the ATO slot to filter

## Value

a logical vector
