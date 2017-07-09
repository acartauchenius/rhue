test_that("Malformed color definitions are rejected",{
  expect_error(convert_xy("foobar"))
  expect_error(convert_xy("#GHIJKL"))
  expect_error(convert_xy("#FFF"))
  expect_error(convert_xy(42))
})

test_that("colors are correctly converted", {
  expect_equal(convert_xy("#FF0000"), c(0.700606233130904199057, 0.299300986842105321006), tolerance = 1e-20)
  expect_equal(convert_xy("#0000FF"), c(0.135503014002903626744, 0.039878670493547159481), tolerance = 1e-20)
  expect_equal(convert_xy("#00FF00"), c(0.172416143149060313577, 0.746796608522043658950), tolerance = 1e-20)
  expect_equal(convert_xy("#000000"), c(0.322726720865567973106, 0.329022909559079257402), tolerance = 1e-20)
  expect_equal(convert_xy("#FFFFFF"), c(0.322726720865568028617, 0.329022909559079257402), tolerance = 1e-20)
})


