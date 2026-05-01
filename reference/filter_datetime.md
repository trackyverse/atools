# Wrapper to filter detections and observations within the desired time period

Wrapper to filter detections and observations within the desired time
period

## Usage

``` r
filter_datetime(object, from = "1970-01-01", to = "3000-01-01", hard = FALSE)
```

## Arguments

- object:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.html)

- from:

  Filter data from this timestamp (included). Can be posixct or
  character.

- to:

  Filter data up to this timestamp (included). Can be posixct or
  character.

- hard:

  If false (the default), animal rows are flagged as invalid through the
  valid column, but kept in the dataset. Switch to true to drop the tags
  that do not fit the filtering criteria (useful to improve performance
  when handling very large datasets).

## Value

the updated ATO
