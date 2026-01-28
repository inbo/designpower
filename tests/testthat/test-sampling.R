test_that("sample_sigma returns correct structure", {
  result <- sample_sigma(
    nl_mean = 0.5,
    boot_mean = 0.3,
    error_mean = 0.2,
    nl_var = 0.01,
    boot_var = 0.01,
    error_var = 0.01,
    n_sim = 10
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 10)
  expect_equal(colnames(result), c("sim_id", "nl_sd", "boot_sd", "error_sd"))
})

test_that("sample_sigma sim_id sequential", {
  result <- sample_sigma(
    nl_mean = 0.5,
    boot_mean = 0.3,
    error_mean = 0.2,
    nl_var = 0.01,
    boot_var = 0.01,
    error_var = 0.01,
    n_sim = 20
  )

  expect_equal(result$sim_id, seq_len(20))
})

test_that("sample_boot returns correct structure", {
  result <- sample_boot(
    intercept_mean = 0,
    cyear_mean = 0.5,
    intercept_sd = 0.1,
    cyear_sd = 0.1,
    boot_sd = 0.1,
    n_boot = 10
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 10)
  expect_equal(colnames(result), c("boot_id", "intercept", "cyear", "boot"))
})

test_that("sample_year returns correct structure", {
  result <- sample_year(nl_sd = 0.1, error_sd = 0.1, n_year = 12)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 12)
  expect_equal(colnames(result), c("year", "error"))
  expect_equal(result$year, seq_len(12) - 1)
})

test_that("sample_year error is numeric", {
  result <- sample_year(nl_sd = 0.1, error_sd = 0.1, n_year = 5)
  expect_true(is.numeric(result$error))
  expect_equal(length(result$error), 5)
})
