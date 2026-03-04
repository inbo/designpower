# designpower: Design Power Analysis Tools

[Onkelinx, Thierry![ORCID
logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-8804-4216)[^1][^2][^3]
[Research Institute for Nature and Forest
(INBO)](mailto:info%40inbo.be)[^4][^5]

**keywords**: sample size, power analysis, design optimization

Tools for iterative power analysis and design optimization using
simulation and adaptive sampling strategies.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r

# install.packages("remotes")
remotes::install_git("https://github.com/inbo/designpower")
```

## Example

The package provides tools for iterative power analysis. Here’s a basic
example:

``` r

library(designpower)

# Define your simulation function
my_sim <- function(design, n_sim, ...) {
  replicate(n_sim, {
    # Your simulation logic here
    # Should return p-values
    runif(1)
  }) |>
    list(p = _)
}

# Find optimal design parameters
result <- find_power(
  design = list(effect_size = 0.3, n_per_group = 30),
  design_digits = c(effect_size = 2, n_per_group = 0),
  opti = "effect_size",
  sim_power = my_sim,
  extra_args = list(),
  power = 0.8,
  alpha = 0.05
)

result
```

## Functions

The main functions in `designpower` are:

- [`find_power()`](https://inbo.github.io/designpower/reference/find_power.md) -
  Iteratively search for optimal design parameters using adaptive
  simulation
- [`sim_power()`](https://inbo.github.io/designpower/reference/sim_power.md) -
  Example power simulation function
- `observed_power()` - Calculate observed power from simulations
  (internal)
- `get_design_id()` - Manage design identifiers in the database
  (internal)
- `sample_new_design()` - Determine next design parameters using GAM
  modelling (internal)

[^1]: author

[^2]: contact person

[^3]: Research Institute for Nature and Forest (INBO)

[^4]: copyright holder

[^5]: funder
