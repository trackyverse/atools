# Wrapper to filter detections and observations that match the transmitters in the tag and dep slots.

Wrapper to filter detections and observations that match the
transmitters in the tag and dep slots.

## Usage

``` r
filter_targeted(object, hard = FALSE)
```

## Arguments

- object:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.html)

- hard:

  If false (the default), animal rows are flagged as invalid through the
  valid column, but kept in the dataset. Switch to true to drop the tags
  that do not fit the filtering criteria (useful to improve performance
  when handling very large datasets).

## Value

the updated ATO
