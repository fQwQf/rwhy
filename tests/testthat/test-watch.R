test_that("watch_on is idempotent and watch_off restores previous handler", {
  original_error <- getOption("error")
  original_old_handler <- getOption("rwhy._old_error_handler")
  original_watch_on <- getOption("rwhy.watch_on")
  withr::defer(options(
    error = original_error,
    rwhy._old_error_handler = original_old_handler,
    rwhy.watch_on = original_watch_on
  ))

  old_handler <- function() NULL
  options(
    error = old_handler,
    rwhy._old_error_handler = NULL,
    rwhy.watch_on = FALSE
  )

  watch_on()
  first_handler <- getOption("error")
  expect_true(is.call(first_handler) || is.function(first_handler))
  expect_true(isTRUE(getOption("rwhy.watch_on")))
  expect_true(is.call(getOption("rwhy._old_error_handler")) ||
    is.function(getOption("rwhy._old_error_handler")))

  watch_on()
  expect_identical(getOption("error"), first_handler)

  watch_off()
  expect_true(is.call(getOption("error")) || is.function(getOption("error")))
  expect_false(isTRUE(getOption("rwhy.watch_on")))
})
