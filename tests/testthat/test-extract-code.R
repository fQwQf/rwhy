test_that("extract_code extracts fenced R code", {
  text <- "Here is code:\n```r\nx <- 1\n```\nDone."
  expect_equal(extract_code(text), "x <- 1")
})

test_that("extract_code extracts knitr-style R chunks", {
  text <- "```{r, message=FALSE}\nsummary(mtcars)\n```"
  expect_equal(extract_code(text), "summary(mtcars)")
})

test_that("extract_code keeps raw code", {
  code <- "x <- 1\ny <- 2"
  expect_equal(extract_code(code), code)
})
