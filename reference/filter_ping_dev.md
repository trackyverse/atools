# wrapper to filter detections by ping deviation bands

wrapper to filter detections by ping deviation bands

## Usage

``` r
filter_ping_dev(object, bands, grace, hard = FALSE)
```

## Arguments

- object:

  an
  [`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.html)

- bands:

  a vector of deviation bands to filter

- grace:

  width of the bands to extract

- hard:

  If false (the default), detections are flagged as invalid through the
  invalid column, but kept in the dataset. Switch to true to drop
  detections that do not fit the filtering criteria (useful to improve
  performance when handling very large datasets).

## Value

the updated ATO
