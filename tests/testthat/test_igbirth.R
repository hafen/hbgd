# nolint start
context("igbirth")

checkpoints <- read.csv(textConnection("var,sex,ga,p3,p5,p10,p50,p90,p95,p97
lencm,Male,232,39.89,40.45,41.28,43.98,46.70,47.54,48.11
lencm,Male,300,48.49,48.89,49.49,51.44,53.40,54.01,54.42
lencm,Female,232,39.98,40.46,41.19,43.57,45.87,46.55,47.01
lencm,Female,300,47.59,48.00,48.60,50.58,52.50,53.07,53.45
wtkg,Male,232,1.22,1.32,1.47,1.99,2.56,2.74,2.86
wtkg,Male,300,2.96,3.06,3.21,3.71,4.25,4.43,4.54
wtkg,Female,232,1.24,1.33,1.45,1.90,2.40,2.55,2.66
wtkg,Female,300,2.80,2.90,3.04,3.53,4.08,4.26,4.37
hcircm,Male,232,28.35,28.69,29.21,30.97,32.78,33.33,33.69
hcircm,Male,300,33.33,33.60,34.00,35.38,36.80,37.23,37.51
hcircm,Female,232,28.03,28.36,28.86,30.55,32.32,32.86,33.22
hcircm,Female,300,32.67,32.93,33.31,34.60,35.95,36.36,36.63"),
stringsAsFactors = FALSE)
# nolint end

test_that("vectorized intergrowth birth standard to centile conversion works", {
  a <- igb_centile2value(checkpoints$ga,
    var = checkpoints$var, sex = checkpoints$sex)
  expect_true(all(abs(a - checkpoints$p50) < 0.015))

  a <- igb_zscore2value(checkpoints$ga,
    var = checkpoints$var, sex = checkpoints$sex)
  expect_true(all(abs(a - checkpoints$p50) < 0.015))
})

test_that("vectorized intergrowth birth standard centile to value conversion works", {
  a <- igb_value2centile(checkpoints$ga, checkpoints$p50,
    var = checkpoints$var, sex = checkpoints$sex) / 100
  expect_true(all(abs(a - 0.5) < 0.015))

  a <- igb_value2zscore(checkpoints$ga, checkpoints$p50,
    var = checkpoints$var, sex = checkpoints$sex)
  expect_true(all(abs(a - 0) < 0.03))
})
