

context("check data")

test_that("check_data", {
  expect_true(check_data(cpp, has_hcir = FALSE))

  smc <- brokenstick::smocc.hgtwgt
  expect_false(check_data(smc, has_hcir = FALSE))
})
