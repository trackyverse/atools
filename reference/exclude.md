# Exclude rows from an ATO slot

If exclusion parameters are not provided, removes the entire slot.

## Usage

``` r
exclude(
  object,
  slt = c("ani", "dep", "det", "tag", "obs"),
  ...,
  hard = FALSE,
  silent = FALSE
)
```

## Arguments

- object:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.html)

- slt:

  The slot to be filtered.

- ...:

  named arguments to exclude by. The argument's name must match one of
  the column names in the target slot. If only one value is provided,
  data is excluded by exact match. If two values are provided for
  continuous variables, data is filtered within the interval provided.
  If three or more values are provided, data is filtered by exact match.
  If no arguments are provided, the entire slot is removed.

- hard:

  If false (the default), animal rows are flagged as invalid through the
  valid column, but kept in the dataset. Switch to true to drop the rows
  that do not fit the filtering criteria (useful to improve performance
  when handling very large datasets).

- silent:

  Logical: Supress summary messages

## Value

the updated ATO
