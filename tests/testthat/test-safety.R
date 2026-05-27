test_that("validate_generated_code rejects invalid code", {
  expect_error(validate_generated_code("x <- "), "valid R code")
})

test_that("warn_risky_code identifies risky calls", {
  expect_equal(warn_risky_code("unlink('x')"), "unlink")
})

test_that("sanitize_error_body redacts secrets and truncates", {
  body <- paste0("Bearer sk-secret-token ", paste(rep("x", 1100), collapse = ""))
  out <- sanitize_error_body(body, max_chars = 20)
  expect_match(out, "Bearer \\[redacted\\]")
  expect_lte(nchar(out), 23)
})

test_that("parse_event_stream combines streamed delta content", {
  body <- paste(
    'data: {"choices":[{"delta":{"content":"hello "}}]}',
    'data: {"choices":[{"delta":{"content":"world"}}]}',
    "data: [DONE]",
    sep = "\n"
  )

  parsed <- parse_event_stream(body)
  expect_equal(extract_llm_content(parsed), "hello world")
})

test_that("parse_event_stream handles OpenAI-compatible variants", {
  body <- paste(
    ' data: {"message":{"content":"hello "}}',
    'data: {"response":"world"}',
    "data: [DONE]",
    sep = "\n"
  )

  parsed <- parse_event_stream(body)
  expect_equal(extract_llm_content(parsed), "hello world")
})

test_that("parse_event_stream falls back to JSON body mislabeled as event stream", {
  body <- '{"choices":[{"message":{"content":"pong"}}]}'

  parsed <- parse_event_stream(body)
  expect_equal(extract_llm_content(parsed), "pong")
})

test_that("extract_llm_content finds nested text fields", {
  result <- list(output = list(list(content = list(list(text = "nested")))))
  expect_equal(extract_llm_content(result), "nested")
})
