
test_that("compute_recharge_works", {
  raindata = rep(0, times=730)
  expect_that(compute_recharge(raindata)$totalrain, equals(0))
  expect_that(compute_recharge(raindata)$totalrecharge, equals(0))
})
