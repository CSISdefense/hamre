# Unit tests for hamre::money_labels()

context("Axis labels")

test_formatter <- money_labels()

test_that("too-high number is unformatted", {
  expect_equal(
    test_formatter(c(100000000000000000, 1e18)),
    c("1e+17", "1e+18"))
})


test_that("negative number formats correctly", {
  expect_equal(
    test_formatter(c(-150000, -250000)),
    c("$-150k", "$-250k"))

  expect_equal(
    test_formatter(c(-12350, -12400)),
    c("$-12.35k", "$-12.40k"))
})


test_that("narrow range gets lots of decimals", {
  expect_equal(
    test_formatter(c(67890123, 67890126, 67890129)),
    c("$67.890123M", "$67.890126M", "$67.890129M"))

  expect_equal(
    test_formatter(c(210469, 210502, 210477)),
    c("$210.469k", "$210.502k", "$210.477k"))
})

test_that("three digits before decimal gets zero after", {
  expect_equal(
    test_formatter(c(105.219e12, 488.302e12, 969.515e12)),
    c("$105T", "$488T", "$970T"))

  expect_equal(
    test_formatter(c(105.219e9, 488.302e9, 969.515e9)),
    c("$105B", "$488B", "$970B"))

  expect_equal(
    test_formatter(c(105.219e6, 488.302e6, 969.515e6)),
    c("$105M", "$488M", "$970M"))

  expect_equal(
    test_formatter(c(105.219e3, 488.302e3, 969.515e3)),
    c("$105k", "$488k", "$970k"))

  expect_equal(
    test_formatter(c(105.219, 488.302, 969.515)),
    c("$105", "$488", "$970"))
})

test_that("two digits before decimal gets one after", {
  expect_equal(
    test_formatter(c(10.219e12, 48.302e12, 98.515e12)),
    c("$10.2T", "$48.3T", "$98.5T"))

  expect_equal(
    test_formatter(c(10.219e9, 48.302e9, 98.515e9)),
    c("$10.2B", "$48.3B", "$98.5B"))

  expect_equal(
    test_formatter(c(10.219e6, 48.302e6, 98.515e6)),
    c("$10.2M", "$48.3M", "$98.5M"))

  expect_equal(
    test_formatter(c(10.219e3, 48.302e3, 98.515e3)),
    c("$10.2k", "$48.3k", "$98.5k"))

  expect_equal(
    test_formatter(c(10.219, 48.302, 98.515)),
    c("$10.2", "$48.3", "$98.5"))
})

test_that("one digit before decimal gets two after", {
  expect_equal(
    test_formatter(c(1.019e12, 4.302e12, 9.725e12)),
    c("$1.02T", "$4.30T", "$9.72T"))

  expect_equal(
    test_formatter(c(1.019e9, 4.302e9, 9.725e9)),
    c("$1.02B", "$4.30B", "$9.72B"))

  expect_equal(
    test_formatter(c(1.019e6, 4.302e6, 9.725e6)),
    c("$1.02M", "$4.30M", "$9.72M"))

  expect_equal(
    test_formatter(c(1.019e3, 4.302e3, 9.725e3)),
    c("$1.02k", "$4.30k", "$9.72k"))

  expect_equal(
    test_formatter(c(1.019, 4.302, 9.725)),
    c("$1.02", "$4.30", "$9.72"))
})

test_that("receiving character axis warns and coerces", {
  expect_equal(
    suppressWarnings(test_formatter(c("1025723", "1025999"))),
    c("$1.0257M", "$1.0260M"))

  expect_warning(test_formatter(c("1025723", "1025999")))
})

test_that("receiving non-numeric axis throws error", {
  expect_error(test_formatter(factor(c("12345","12789"))))
  expect_error(test_formatter(c(TRUE, TRUE, FALSE)))
})
