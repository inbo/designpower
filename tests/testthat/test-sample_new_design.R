test_that("sample_new_design works without estimates", {
  expect_equal(
    sample_new_design(
      power_summary = data.frame(),
      design = c(trend = 0.05),
      design_digits = c(trend = 2),
      opti = "trend"
    ),
    0.05
  )
})
test_that("sample_new_design works with a single estimate", {
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(0.05),
        signif = 100,
        non_signif = 0,
        estimated_power = 1,
        lcl = 1,
        ucl = 1
      ),
      design = c(trend = 0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(0.025, 0.1))
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(-0.05),
        signif = 100,
        non_signif = 0,
        estimated_power = 1,
        lcl = 1,
        ucl = 1
      ),
      design = c(trend = -0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(-0.025, -0.1))
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(0.05),
        signif = 0,
        non_signif = 100,
        estimated_power = 0,
        lcl = 0,
        ucl = 0
      ),
      design = c(trend = 0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(0.025, 0.1))
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(-0.05),
        signif = 0,
        non_signif = 100,
        estimated_power = 0,
        lcl = 0,
        ucl = 0
      ),
      design = c(trend = -0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(-0.025, -0.1))
})
test_that("sample_new_design works with two estimates", {
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(0.05, 0.1),
        signif = 100,
        non_signif = 0,
        estimated_power = 1,
        lcl = 1,
        ucl = 1
      ),
      design = c(trend = 0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(0.025, 0.2))
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(0.05, 0.1),
        signif = 0,
        non_signif = 100,
        estimated_power = 0,
        lcl = 0,
        ucl = 0
      ),
      design = c(trend = 0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(0.025, 0.2))
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(-0.05, -0.1),
        signif = 100,
        non_signif = 0,
        estimated_power = 1,
        lcl = 1,
        ucl = 1
      ),
      design = c(trend = -0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(-0.025, -0.2))
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(-0.05, -0.1),
        signif = 0,
        non_signif = 100,
        estimated_power = 0,
        lcl = 0,
        ucl = 0
      ),
      design = c(trend = -0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(new_design %in% c(-0.025, -0.2))
  expect_equal(
    sample_new_design(
      power_summary = data.frame(
        trend = c(0.05, 0.1),
        signif = c(0, 100),
        non_signif = c(100, 0),
        estimated_power = c(0, 1),
        lcl = c(0, 1),
        ucl = c(0, 1)
      ),
      design = c(trend = 0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    0.075
  )
  expect_equal(
    sample_new_design(
      power_summary = data.frame(
        trend = c(0.05, 0.1),
        signif = c(99, 100),
        non_signif = c(1, 0),
        estimated_power = c(0.99, 1),
        lcl = c(0.99, 1),
        ucl = c(0.99, 1)
      ),
      design = c(trend = 0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    0.025
  )
  expect_type(
    new_design <- sample_new_design(
      power_summary = data.frame(
        trend = c(0.05, 0.1, 0.2),
        signif = c(0, 50, 100),
        non_signif = c(100, 50, 0),
        estimated_power = c(0, 0.5, 1),
        lcl = c(0, 0.5, 1),
        ucl = c(0, 0.5, 1),
        n_sim = c(100, 100, 100)
      ),
      design = c(trend = 0.05),
      design_digits = c(trend = 3),
      opti = "trend"
    ),
    "double"
  )
  expect_true(0.1 < new_design && new_design < 0.2)
})
