# unit tests for deflate function

housing <- txhousing %>%
  group_by(year) %>%
  summarize(millions = sum(volume, na.rm = TRUE) / 1e6) %>%
  as.data.frame()

test_that("variant class types don't cause errors", {
  ints <- housing %>% mutate(
    year = as.integer(year),
    millions = as.integer(millions))

  expect_equal(
    deflate(ints, "millions", "year"),
    deflate(housing, "millions", "year"),
    tolerance = 0.5)


})
