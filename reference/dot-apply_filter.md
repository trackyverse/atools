# Worker function to apply filtering

Worker function to apply filtering

## Usage

``` r
.apply_filter(
  object,
  filter_vec,
  exclude,
  slt = c("ani", "dep", "det", "tag", "obs"),
  hard = FALSE
)
```

## Arguments

- object:

  an ATO

- filter_vec:

  a logical vector, normally created by
  [`create_filter_vec`](https://atools.trackyverse.org/reference/create_filter_vec.md).

- exclude:

  FALSE to filter by filter_vec, TRUE to exclude by filter_vec.

- slt:

  the ATO slot to filter

- hard:

  Should the filtering mark rows as invalid (FALSE) or remove them
  altogether (TRUE)

## Value

the updated ATO
