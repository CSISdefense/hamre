# unit tests for deflate function

context("deflate()")

housing <- txhousing %>%
  group_by(year) %>%
  summarize(millions = sum(volume, na.rm = TRUE) / 1e6) %>%
  as.data.frame()

test_that("variant class types don't cause errors or get overwritten", {

  # integer
  ints <- housing %>% mutate(
    year = as.integer(year),
    millions = as.integer(millions))

  expect_equal(
    deflate(ints, "millions", "year")$millions,
    deflate(housing, "millions", "year")$millions,
    tolerance = 0.1)

  expect_is(
    deflate(ints, "millions", "year")$year,
    "integer")

  # character
  chars <- housing %>% mutate(
    year = as.character(year),
    millions = as.character(millions))

  expect_equal(
    deflate(chars, "millions", "year")$millions,
    deflate(housing, "millions", "year")$millions,
    tolerance = 0.1)

  expect_is(
    deflate(chars, "millions", "year")$year,
    "character")

  # factor
  fact <- housing %>% mutate(
    year = factor(year),
    millions = factor(millions))

  expect_equal(
    deflate(fact, "millions", "year")$millions,
    deflate(housing, "millions", "year")$millions,
    tolerance = 0.1)

  expect_is(
    deflate(fact, "millions", "year")$year,
    "factor")
})
