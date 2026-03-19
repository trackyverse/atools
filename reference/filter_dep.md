# Filter rows from the @dep slot

Filter rows from the @dep slot

## Usage

``` r
filter_dep(object, ...)
```

## Arguments

- object:

  an
  [`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.html)

- ...:

  named arguments to filter for. The argument's name must match one of
  the column names in the deployments If only one value is provided,
  data is filtered by exact match. If two values are provided for
  continuous variables, data is filtered within the interval provided.
  If three or more values are provided, data is filtered by exact match.

## Value

the updated ATO
