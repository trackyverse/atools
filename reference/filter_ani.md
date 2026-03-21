# Filter rows from the @ani slot

Filter rows from the @ani slot

## Usage

``` r
filter_ani(object, ..., hard = FALSE)
```

## Arguments

- object:

  an
  [`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.html)

- ...:

  named arguments to filter for. The argument's name must match one of
  the column names in the animals slot. If only one value is provided,
  data is filtered by exact match. If two values are provided for
  continuous variables, data is filtered within the interval provided.
  If three or more values are provided, data is filtered by exact match.

- hard:

  If false (the default), animal rows are flagged as invalid through the
  valid column, but kept in the dataset. Switch to true to drop the tags
  that do not fit the filtering criteria (useful to improve performance
  when handling very large datasets).

## Value

the updated ATO
