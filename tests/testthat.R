library(testthat)
library(designpower)
suppressMessages(duckdb::duckdb(shared_home = FALSE))
test_check("designpower")
