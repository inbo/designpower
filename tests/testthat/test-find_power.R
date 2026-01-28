test_that("observed_power validates input", {
  # Create a temporary in-memory database
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  # Should return empty data frame if table doesn't exist
  result <- observed_power(
    connection = conn,
    alpha = 0.05,
    opti_clause = "d.trend",
    where_clause = "1=1"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  duckdb::dbDisconnect(conn, shutdown = TRUE)
})

test_that("observed_power requires valid alpha", {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  expect_error(
    observed_power(
      connection = conn,
      alpha = 1.5,
      opti_clause = "d.trend",
      where_clause = "1=1"
    )
  )

  duckdb::dbDisconnect(conn, shutdown = TRUE)
})

test_that("get_design_id handles new design", {
  conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  design_id <- get_design_id(
    connection = conn,
    where_clause = "1=1",
    opti_clause = "d.trend",
    design = list(trend = -0.03, n_site = 20, n_year = 12),
    opti = "trend",
    hashes = c(test_hash = "abc123")
  )

  expect_equal(design_id, 1L)

  duckdb::dbDisconnect(conn, shutdown = TRUE)
})
