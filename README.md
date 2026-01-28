
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: Concept - Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GPL-3](https://img.shields.io/badge/License-GPL-3-brightgreen)](https://raw.githubusercontent.com/inbo/checklist/refs/heads/main/inst/generic_template/gplv3.md)
[![Release](https://img.shields.io/github/release/inbo/designpower.svg)](https://github.com/inbo/designpower/releases)
![GitHub Workflow
Status](https://github.com/inbo/designpower/actions/workflows/check_on_main.yml/badge.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/designpower) ![GitHub
code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/designpower.svg)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/designpower)
[![Codecov test
coverage](https://codecov.io/gh/inbo/designpower/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/designpower?branch=main)
<!-- badges: end -->

# designpower: Design Power Analysis Tools

[Onkelinx, Thierry![ORCID
logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-8804-4216)[^1][^2][^3]
[Research Institute for Nature and Forest
(INBO)](mailto:info%40inbo.be)[^4][^5]

**keywords**: sample size, power analysis, design optimization

<!-- description: start -->

Tools for iterative power analysis and design optimization using
simulation and adaptive sampling strategies. <!-- description: end -->

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

- `find_power()` - Iteratively search for optimal design parameters
  using adaptive simulation
- `sim_power()` - Example power simulation function
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
