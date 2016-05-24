test_that("data table returned is correct", {

  result <- wb_sequence("pot-2")
  expect_true(nrow(result) == 1, TRUE)
  expect_true(ncol(result) == 3, TRUE)
})
test_that("gene cannot be found", {
  result <- wb_sequence("crockpot-2")
  expect_true(nrow(result) == 0, TRUE)
  expect_message(wb_sequence("crockpot-2"), "gene does not exist")

})
test_that("gene cannot be found in C. elegans", {
  result <- wb_sequence("MAPK")
  expect_true(nrow(result) == 0, TRUE)
  expect_message(wb_sequence("MAPK"), "gene not found in C. elegans")
})
