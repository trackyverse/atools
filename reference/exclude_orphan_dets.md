# Wrapper to exclude detections that to not match the deployments

Wrapper to exclude detections that to not match the deployments

## Usage

``` r
exclude_orphan_dets(object, hard = FALSE)
```

## Arguments

- object:

  an
  [`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.html)

- hard:

  If false (the default), detections are flagged as invalid through the
  invalid column, but kept in the dataset. Switch to true to completely
  remove the detections from the dataset (useful to improve performance
  when handling very large datasets).

## Value

the updated ATO
