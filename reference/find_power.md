# Find Optimal Design

Iteratively search for optimal design parameters using adaptive
simulation.

## Usage

``` r
find_power(
  design,
  design_digits,
  opti,
  sim_power,
  extra_args = list(),
  power = 0.9,
  alpha = 0.1,
  filename = "power.duckdb"
)
```

## Arguments

- design:

  List. Initial design parameters.

- design_digits:

  Named numeric. Precision (number of decimal places) for each
  parameter. The names must match those in `design`.

- opti:

  Character. Name of parameter to optimize.

- sim_power:

  Function. Simulation function (should accept `design` and `n_sim`).

- extra_args:

  List. Optional additional arguments passed to `sim_power`. Defaults to
  an empty list.

- power:

  Numeric. Target power (default 0.9).

- alpha:

  Numeric. Significance level (default 0.1).

- filename:

  Character. Path to `DuckDB` database file.

## Value

Numeric vector. The optimized parameter value and confidence range.

## Details

Uses `DuckDB` to store simulations and gradually refines design
parameters to achieve target power.

## Examples

``` r
if (FALSE) { # \dontrun{
find_power(
  design = list(trend = -0.03, n_site = 20, n_year = 12),
  design_digits = c(trend = 4, n_site = 0, n_year = 0),
  opti = "trend",
  sim_power = sim_power,
  extra_args = list()
)
} # }
```
