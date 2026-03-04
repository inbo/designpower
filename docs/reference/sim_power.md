# Simple Power Simulation

A basic example of a power simulation function.

## Usage

``` r
sim_power(
  design = list(trend = -0.03, n_year = 12, n_sample = 20),
  n_sim = 100,
  intercept = 2,
  sigma_error = 0.1
)
```

## Arguments

- design:

  A named list with design parameters:

  - `trend`: The true trend parameter

  - `n_year`: Number of years

  - `n_sample`: Number of samples per year

- n_sim:

  Integer. Number of simulations.

- intercept:

  Numeric. Intercept of the model.

- sigma_error:

  Numeric. Standard deviation of the error term.

## Value

A data frame with columns:

- `p`: P-values from simulations

- `trend`: Estimated trends

## Details

Simulates data under a simple model and returns p-values.

## Examples

``` r
if (FALSE) { # \dontrun{
sim_power(
  design = list(trend = -0.03, n_year = 12, n_sample = 20),
  n_sim = 10
)
} # }
```
