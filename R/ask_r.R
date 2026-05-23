#' @title Ask R to Generate and Run Code
#' @description Describe what you want in natural language; AI generates R code.
#' @param query Character. Natural language description.
#' @param auto_run Logical. Execute without confirmation? Default \code{FALSE}.
#' @param insert Logical. Insert into RStudio script? Default \code{FALSE}.
#' @return Invisibly returns the generated code as a character string.
#' @examples
#' \dontrun{
#' ask_r("Create a bar chart of cylinder counts in mtcars")
#' ask_r("Fit a linear regression of mpg on wt in mtcars")
#' }
#' @export
ask_r <- function(query, auto_run = FALSE, insert = FALSE) {
  if (!is.character(query) || !nzchar(query)) {
    cli::cli_abort(t("ask_invalid"))
  }

  # Language-adaptive system prompt
  lang_instr <- t("prompt_code_lang")
  system_prompt <- paste0(
    "You are an R code generator. Follow these rules strictly:\n",
    "1. Return ONLY executable R code.\n",
    "2. Do NOT wrap code in ```r ... ``` markdown fences.\n",
    "3. Do NOT include any explanations or prose.\n",
    "4. Use tidyverse (dplyr, ggplot2) style by default.\n",
    "5. Include library() calls at the top if using non-base packages.\n",
    "6. If the request is about plotting, always produce a complete ggplot.\n",
    "7. Keep the code concise but complete.\n",
    "8. Output valid, directly runnable R code.\n",
    "9. ", lang_instr
  )

  source_context <- try_get_source_context()
  full_query <- query
  if (!is.null(source_context) && nzchar(source_context)) {
    full_query <- paste0(
      "My current script contains:\n", source_context,
      "\n\nMy request: ", query
    )
  }

  cli::cli_alert_info(t("ask_writing"))

  raw_response <- ask_llm(prompt = full_query, system = system_prompt, temperature = 0.3)
  code <- extract_code(raw_response)

  cli::cli_rule(left = t("ask_generated"))
  cli::cli_code(code)
  cli::cli_rule()

  if (insert && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::insertText(text = paste0("\n", code, "\n"))
    cli::cli_alert_success(t("ask_inserted"))
    return(invisible(code))
  }

  if (auto_run) {
    cli::cli_alert_info(t("ask_running"))
    safe_eval(code)
  } else {
    ans <- readline(prompt = t("ask_run_prompt"))
    ans <- tolower(trimws(ans))
    if (ans == "y" || ans == "yes") {
      safe_eval(code)
    } else if (ans == "e" || ans == "edit") {
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable()) {
        rstudioapi::insertText(text = paste0("\n", code, "\n"))
        cli::cli_alert_success(t("ask_insert_script"))
      } else {
        cli::cli_alert_warning(t("ask_insert_no_rstudio"))
      }
    } else {
      cli::cli_alert_info(t("ask_not_executed"))
    }
  }

  invisible(code)
}


#' @title Safely Evaluate Code String
#' @keywords internal
safe_eval <- function(code) {
  tryCatch(
    eval(parse(text = code), envir = .GlobalEnv),
    error = function(e) {
      cli::cli_alert_danger(t("ask_exec_error"))
      cli::cli_alert_info(t("ask_suggest_why"))
    },
    warning = function(w) {
      cli::cli_alert_warning(t("ask_exec_warning"))
      suppressWarnings(eval(parse(text = code), envir = .GlobalEnv))
    }
  )
}
