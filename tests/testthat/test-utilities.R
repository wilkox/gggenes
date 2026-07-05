library(ggplot2)

# Direct unit tests for the internal argument validators. The geoms wire these to
# user-facing arguments (e.g. `grow`/`reflow` via assert_flag(), `min.size` via
# assert_scalar_nonnegative_number()), but the validators themselves carry the
# logic and are pinned here.

test_that("assert_flag() accepts a scalar TRUE/FALSE and rejects everything else", {
  expect_no_error(assert_flag(TRUE))
  expect_no_error(assert_flag(FALSE))

  expect_error(assert_flag(NA))
  expect_error(assert_flag(c(TRUE, FALSE)))
  expect_error(assert_flag(logical(0)))
  expect_error(assert_flag("TRUE"))
  expect_error(assert_flag(1))
})

test_that("assert_scalar_nonnegative_number() accepts non-negative scalars only", {
  expect_no_error(assert_scalar_nonnegative_number(0))
  expect_no_error(assert_scalar_nonnegative_number(1.5))

  expect_error(assert_scalar_nonnegative_number(-1))
  expect_error(assert_scalar_nonnegative_number(NA_real_))
  expect_error(assert_scalar_nonnegative_number(c(1, 2)))
  expect_error(assert_scalar_nonnegative_number(numeric(0)))
  expect_error(assert_scalar_nonnegative_number("1"))
})

test_that("assert_scalar_unit() accepts a scalar grid unit only", {
  expect_no_error(assert_scalar_unit(grid::unit(1, "mm")))

  expect_error(assert_scalar_unit(grid::unit(1:2, "mm")))
  expect_error(assert_scalar_unit(1))
  expect_error(assert_scalar_unit("1mm"))
})

test_that("assert_choice() accepts a listed choice only", {
  expect_no_error(assert_choice("left", c("left", "right")))

  expect_error(assert_choice("up", c("left", "right")))
  expect_error(assert_choice(c("left", "right"), c("left", "right")))
  expect_error(assert_choice(1, c("left", "right")))
})

test_that("assert_data_frame() accepts a data frame only", {
  expect_no_error(assert_data_frame(data.frame(x = 1)))

  expect_error(assert_data_frame(list(x = 1)))
  expect_error(assert_data_frame("not a data frame"))
})

test_that("assert_scalar_character() accepts a single string only", {
  expect_no_error(assert_scalar_character("gene"))

  expect_error(assert_scalar_character(c("a", "b")))
  expect_error(assert_scalar_character(NA_character_))
  expect_error(assert_scalar_character(character(0)))
  expect_error(assert_scalar_character(1))
})

test_that("assert_mapping() accepts an aes() mapping only", {
  expect_no_error(assert_mapping(aes(xmin = start)))

  expect_error(assert_mapping(list(xmin = "start")))
  expect_error(assert_mapping("xmin"))
})
