#' @title Call the LLM API
#' @description Send a prompt to an OpenAI-compatible chat completions endpoint.
#' @param prompt Character. The user message to send.
#' @param system Character. The system prompt.
#' @param temperature Numeric. Sampling temperature (0-2). Default 0.7.
#' @return A character string with the model's response.
#' @keywords internal
ask_llm <- function(prompt, system = NULL, temperature = 0.7) {
  api_key <- get_ai_key()
  if (is.null(api_key)) {
    cli::cli_abort(c(
      t("llm_no_key"),
      "i" = t("llm_no_key_hint")
    ))
  }

  cfg <- ai_config()

  messages <- list()
  if (!is.null(system) && nchar(system) > 0) {
    messages <- c(messages, list(list(role = "system", content = system)))
  }
  messages <- c(messages, list(list(role = "user", content = prompt)))

  body <- list(
    model       = cfg$model,
    messages    = messages,
    temperature = temperature
  )

  req <- httr2::request(cfg$base_url)
  req <- httr2::req_headers(
    req,
    "Content-Type"  = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )
  req <- httr2::req_body_json(req, body)
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(c(
        t("llm_connect_fail"),
        "i" = "{conditionMessage(e)}"
      ))
    }
  )

  status <- httr2::resp_status(resp)
  if (status != 200) {
    body_text <- httr2::resp_body_string(resp)
    cli::cli_abort(c(
      t("llm_http_error"),
      "i" = "{body_text}"
    ))
  }

  result <- httr2::resp_body_json(resp)
  content <- result$choices[[1]]$message$content

  if (is.null(content) || !nzchar(content)) {
    cli::cli_abort(t("llm_empty"))
  }

  content
}
