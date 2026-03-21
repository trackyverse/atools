# Exclude rows from the @ani slot

If exclusion parameters are not provided, removes the entire slot.

## Usage

``` r
exclude_ani(object, ..., hard = FALSE)
```

## Arguments

- object:

  an
  [`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.html)

- ...:

  named arguments to exclude by. The argument's name must match one of
  the column names in the animals slot. If only one value is provided,
  data is excluded by exact match. If two values are provided for
  continuous variables, data is filtered within the interval provided.
  If three or more values are provided, data is filtered by exact match.
  If no arguments are provided, the entire slot is removed.

- hard:

  If false (the default), animals are flagged as invalid through the
  invalid column, but kept in the dataset. Switch to true to completely
  remove the animals from the dataset (useful to improve performance
  when handling very large datasets). Ignored if no exclusion arguments
  are provided.

## Value

the updated ATO
