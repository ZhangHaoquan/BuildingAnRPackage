context("make_filename")

## TODO: Rename context
## TODO: Add more tests

test_that("Make Filename Works", {
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
})
