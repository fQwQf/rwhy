#' @title Call the LLM API
#' @description Send a prompt to an OpenAI-compatible chat completions endpoint.
#' @param prompt Character. The user message to send.
#' @param system Character. The system prompt.
#' @param temperature Numeric. Sampling temperature (0-2). Default 0.7.
#' @return A character string with the model's response.
#' @noRd
ask_llm <- function(prompt, system = NULL, temperature = 0.7,
                    timeout = getOption("rwhy.timeout", 60),
                    max_tokens = getOption("rwhy.max_tokens", NULL)) {
  api_key <- get_ai_key()
  cfg <- ai_config()
  needs_key <- !identical(cfg$provider, "ollama")

  if (needs_key && is.null(api_key)) {
    cli::cli_abort(c(
      t("llm_no_key"),
      "i" = t("llm_no_key_hint")
    ))
  }

  messages <- list()
  if (!is.null(system) && nchar(system) > 0) {
    messages <- c(messages, list(list(role = "system", content = system)))
  }
  messages <- c(messages, list(list(role = "user", content = prompt)))

  body <- list(
    model       = cfg$model,
    messages    = messages,
    temperature = temperature,
    stream      = FALSE
  )
  if (!is.null(max_tokens)) body$max_tokens <- max_tokens

  req <- httr2::request(cfg$base_url)
  req <- httr2::req_headers(
    req,
    "Content-Type" = "application/json"
  )
  if (needs_key) {
    req <- httr2::req_headers(req, "Authorization" = paste("Bearer", api_key))
  }
  req <- httr2::req_body_json(req, body)
  req <- httr2::req_timeout(req, timeout)
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
    body_text <- sanitize_error_body(httr2::resp_body_string(resp))
    cli::cli_abort(c(
      t("llm_http_error"),
      "i" = "{body_text}"
    ))
  }

  result <- parse_llm_response(resp)
  content <- extract_llm_content(result)

  if (is.null(content) || !nzchar(content)) {
    cli::cli_abort(t("llm_empty"))
  }

  content
}


#' @title Parse an LLM API Response
#' @noRd
parse_llm_response <- function(resp) {
  content_type <- httr2::resp_header(resp, "content-type") %||% ""

  if (grepl("text/event-stream", content_type, ignore.case = TRUE)) {
    return(parse_event_stream(httr2::resp_body_string(resp)))
  }

  tryCatch(
    httr2::resp_body_json(resp, check_type = FALSE),
    error = function(e) {
      cli::cli_abort(c(
        t("llm_parse_fail"),
        "i" = conditionMessage(e)
      ))
    }
  )
}


#' @title Parse OpenAI-compatible Event Stream Text
#' @noRd
parse_event_stream <- function(body_text) {
  if (is.null(body_text) || !nzchar(body_text)) {
    cli::cli_abort(t("llm_empty"))
  }

  lines <- strsplit(body_text, "\r?\n", perl = TRUE)[[1]]
  data_lines <- sub("^\\s*data:\\s*", "", grep("^\\s*data:\\s*", lines, value = TRUE))
  data_lines <- data_lines[nzchar(data_lines) & data_lines != "[DONE]"]

  if (length(data_lines) == 0) {
    json_result <- parse_json_text(body_text)
    if (!is.null(json_result)) return(json_result)

    cli::cli_abort(c(
      t("llm_empty"),
      "i" = format_message(t("llm_response_preview"), body = preview_response(body_text))
    ))
  }

  chunks <- lapply(data_lines, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })
  chunks <- Filter(Negate(is.null), chunks)

  content <- paste0(vapply(chunks, extract_llm_content, character(1)), collapse = "")
  if (!nzchar(content)) {
    json_result <- parse_json_text(body_text)
    if (!is.null(json_result)) return(json_result)

    cli::cli_abort(c(
      t("llm_empty"),
      "i" = format_message(t("llm_response_preview"), body = preview_response(body_text))
    ))
  }

  list(choices = list(list(message = list(content = content))))
}


#' @title Parse Raw JSON Text
#' @noRd
parse_json_text <- function(body_text) {
  tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) NULL
  )
}


#' @title Extract Content from Chat Completion Shapes
#' @noRd
extract_llm_content <- function(result) {
  candidates <- list(
    tryCatch(result$choices[[1]]$message$content, error = function(e) NULL),
    tryCatch(result$choices[[1]]$delta$content, error = function(e) NULL),
    tryCatch(result$choices[[1]]$text, error = function(e) NULL),
    tryCatch(result$message$content, error = function(e) NULL),
    tryCatch(result$response, error = function(e) NULL),
    tryCatch(result$content, error = function(e) NULL),
    tryCatch(result$text, error = function(e) NULL),
    tryCatch(result$output_text, error = function(e) NULL)
  )

  for (candidate in candidates) {
    text <- flatten_text(candidate)
    if (nzchar(text)) return(text)
  }

  recursive <- find_first_text(result)
  if (nzchar(recursive)) return(recursive)

  ""
}


#' @title Flatten Text-like Values
#' @noRd
flatten_text <- function(value) {
  if (is.null(value)) return("")
  if (is.character(value)) return(paste0(value, collapse = ""))
  if (is.atomic(value)) return(paste0(as.character(value), collapse = ""))
  ""
}


#' @title Find First Text Field in Nested Response
#' @noRd
find_first_text <- function(x) {
  if (is.null(x)) return("")
  if (is.character(x)) return(paste0(x, collapse = ""))
  if (!is.list(x)) return("")

  preferred <- c("content", "text", "response", "output_text")
  for (name in preferred) {
    if (!is.null(x[[name]])) {
      text <- flatten_text(x[[name]])
      if (nzchar(text)) return(text)
    }
  }

  for (item in x) {
    text <- find_first_text(item)
    if (nzchar(text)) return(text)
  }

  ""
}


#' @title Preview a Raw Response Body
#' @noRd
preview_response <- function(body_text, max_chars = 500) {
  body_text <- sanitize_error_body(body_text, max_chars = max_chars)
  body_text <- gsub("[\r\n]+", " ", body_text)
  body_text
}


#' @title Sanitize API Error Body
#' @noRd
sanitize_error_body <- function(body_text, max_chars = 1000) {
  if (is.null(body_text) || !nzchar(body_text)) return("")

  body_text <- gsub("Bearer\\s+[A-Za-z0-9._~+/=-]+", "Bearer [redacted]", body_text)
  body_text <- gsub("sk-[A-Za-z0-9._-]+", "sk-[redacted]", body_text)

  if (nchar(body_text) > max_chars) {
    body_text <- paste0(substr(body_text, 1, max_chars), "...")
  }

  body_text
}
