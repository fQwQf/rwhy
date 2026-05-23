#' @title Explain the Last R Error
#' @description Capture the most recent error message and ask an AI assistant
#'   to explain it. Responses are in the user's detected language.
#' @return Invisibly returns the AI's explanation as a character string.
#' @examples
#' \dontrun{
#' # 1 + "a"  # causes error
#' # why()    # asks AI to explain
#' }
#' @export
why <- function() {
  last_error <- .rwhy_geterrmessage()

  if (is.null(last_error) || !nzchar(trimws(last_error))) {
    cli::cli_alert_warning(t("why_no_error"))
    return(invisible(NULL))
  }

  source_context <- try_get_source_context()

  # Language-adaptive system prompt
  lang_instr <- t("prompt_error_lang")
  system_prompt <- paste0(
    "You are an R language expert. A user's R code just produced an error. ",
    lang_instr, "\n",
    "Your job:\n",
    "1. Explain the error clearly and concisely.\n",
    "2. Identify the root cause.\n",
    "3. Provide corrected code wrapped in ```r ... ```.\n",
    "4. Keep the total response under 300 words.\n",
    "Format your response as:\n",
    "- **Error**: (one-line summary)\n",
    "- **Cause**: (2-3 sentences)\n",
    "- **Fix**: (corrected code block)"
  )

  prompt_parts <- c(
    sprintf("My R code produced this error:\n%s", last_error),
    if (!is.null(source_context) && nzchar(source_context)) {
      sprintf("\nHere is the surrounding code for context:\n%s", source_context)
    }
  )
  prompt <- paste(prompt_parts, collapse = "")

  cli::cli_alert_info(t("why_asking"))

  answer <- ask_llm(prompt = prompt, system = system_prompt, temperature = 0.3)

  cli::cli_rule(left = "rwhy")
  cli::cli_text(answer)
  cli::cli_rule()

  invisible(answer)
}


#' @title Auto-explain Errors
#' @description Install a global error handler that auto-explains errors using AI.
#' @return Invisible \code{NULL}.
#' @examples
#' \dontrun{ watch_on(); 1 + "a"; watch_off() }
#' @export
watch_on <- function() {
  old_handler <- getOption("error")
  rwhy_handler <- function() {
    last_error <- .rwhy_geterrmessage()
    if (nzchar(trimws(last_error))) {
      cli::cli_alert_info(t("why_auto_detect"))
      tryCatch({
        lang_instr <- t("prompt_error_lang")
        system_prompt <- paste0(
          "You are an R error explainer. ", lang_instr, " ",
          "In 1-2 short sentences, explain this error and suggest a fix. Be very concise."
        )
        answer <- ask_llm(prompt = last_error, system = system_prompt, temperature = 0.2)
        cli::cli_rule(left = t("why_auto_title"))
        cli::cli_text(answer)
        cli::cli_rule()
      }, error = function(e) {
        cli::cli_alert_danger(t("why_auto_fail"))
      })
    }
  }
  options(error = rwhy_handler, rwhy._old_error_handler = old_handler)
  cli::cli_alert_success(t("watch_on"))
  invisible(NULL)
}


#' @rdname watch_on
#' @export
watch_off <- function() {
  old_handler <- getOption("rwhy._old_error_handler")
  if (is.null(old_handler)) old_handler <- NULL
  options(error = old_handler)
  options(rwhy._old_error_handler = NULL)
  cli::cli_alert_success(t("watch_off"))
  invisible(NULL)
}
