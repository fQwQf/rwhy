test_that("ai_config returns provider metadata", {
  withr::local_options(list(
    rwhy.base_url = NULL,
    rwhy.model = NULL,
    rwhy.provider = NULL
  ))
  withr::local_envvar(c(
    RWHY_BASE_URL = NA,
    RWHY_MODEL = NA,
    RWHY_PROVIDER = NA
  ))

  cfg <- ai_config()
  expect_equal(cfg$provider, "deepseek")
  expect_equal(cfg$model, "deepseek-chat")
})

test_that("provider presets set provider id", {
  withr::local_options(list(
    rwhy.base_url = NULL,
    rwhy.model = NULL,
    rwhy.provider = NULL
  ))

  use_ollama()
  cfg <- ai_config()
  expect_equal(cfg$provider, "ollama")
  expect_match(cfg$base_url, "localhost:11434")
})

test_that("ai_status returns status without exposing key", {
  withr::local_options(list(
    rwhy.base_url = "https://api.deepseek.com/v1/chat/completions",
    rwhy.model = "deepseek-chat",
    rwhy.provider = "deepseek",
    rwhy.watch_on = FALSE
  ))
  withr::local_envvar(c(RWHY_API_KEY = "sk-test-secret"))

  status <- ai_status()
  expect_equal(status$provider, "deepseek")
  expect_true(status$has_key)
  expect_false(status$watch_on)
})

test_that("unset_ai_key clears session key", {
  withr::local_envvar(c(RWHY_API_KEY = "sk-test-secret"))

  unset_ai_key(persist = FALSE)
  expect_null(get_ai_key())
})

test_that("set_ai_key accepts explicit key", {
  withr::local_envvar(c(RWHY_API_KEY = NA))

  set_ai_key("sk-test-secret", persist = FALSE)
  expect_equal(get_ai_key(), "sk-test-secret")
})

test_that("set_ai_key requires explicit key when non-interactive", {
  expect_error(set_ai_key(), "non-interactive")
})

test_that("prompt_model uses provider default for blank choice", {
  expect_equal(
    prompt_model(.provider_db$deepseek, input = function(prompt = "") ""),
    "deepseek-chat"
  )
})

test_that("prompt_provider rejects invalid choices", {
  expect_error(
    prompt_provider(input = function(prompt = "") "99"),
    "Invalid choice"
  )
})

test_that("custom base URLs are normalized", {
  expect_equal(
    normalize_custom_base_url("localhost:8000"),
    "http://localhost:8000/v1/chat/completions"
  )
  expect_equal(
    normalize_custom_base_url("http://localhost:8000/v1"),
    "http://localhost:8000/v1/chat/completions"
  )
  expect_equal(
    build_custom_base_url("localhost", "8000"),
    "http://localhost:8000/v1/chat/completions"
  )
})

test_that("use_custom configures endpoint, model, and key", {
  withr::local_options(list(
    rwhy.base_url = NULL,
    rwhy.model = NULL,
    rwhy.provider = NULL
  ))
  withr::local_envvar(c(RWHY_API_KEY = NA))

  cfg <- use_custom(
    base_url = "localhost:9000",
    model = "custom-model",
    key = "sk-custom",
    persist = FALSE
  )

  expect_equal(cfg$provider, "custom")
  expect_equal(cfg$model, "custom-model")
  expect_equal(cfg$base_url, "http://localhost:9000/v1/chat/completions")
  expect_equal(get_ai_key(), "sk-custom")
})
