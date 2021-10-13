#
test_that("HappyFeedback outputs text", {
  expect_invisible(etc::HappyFeedback())
  #expect_that(prints_text(HappyFeedback()))
  #expect_snapshot()
})

# `expect_that()` was deprecated in the 3rd edition.
# Backtrace:
#   1. testthat::expect_that(prints_text(HappyFeedback())) test-HappyFeedback.R:8:2
# 2. testthat:::edition_deprecate(3, "expect_that()")
##
#\033[3 something about the text not starting from this?
##
test_that("another HappyFeedback test", {
  expect_length(etc::HappyFeedback(), 0)
})

# local({
#   local_test_context()
#   cat(crayon::blue("Text will not be colored"))
#   cat(cli::symbol$ellipsis)
#   cat("\n")
# })
# #> Text will not be colored…test_that("test ellipsis", {
# local_reproducible_output(unicode = FALSE)
# expect_equal(cli::symbol$ellipsis, "...")
#
# local_reproducible_output(unicode = TRUE)
# expect_equal(cli::symbol$ellipsis, "\u2026")
# })
# #> Test passed 😸
