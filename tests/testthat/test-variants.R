test_that("data table returned is correct", {

  result <- wb_variants("pot-2")
  expect_true(nrow(result) == 14, TRUE)
  expect_true(ncol(result) == 9, TRUE)
})
test_that("gene cannot be found", {
  result <- wb_variants("crockpot-2")
  expect_true(nrow(result) == 0, TRUE)
  expect_message(wb_variants("crockpot-2"), "gene does not exist")

})
test_that("gene cannot be found in C. elegans", {
  result <- wb_variants("MAPK")
  expect_true(nrow(result) == 0, TRUE)
  expect_message(wb_variants("MAPK"), "gene not found in C. elegans")
})
