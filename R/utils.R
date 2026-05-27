#' @title Get Last Error Message
#' @noRd
.rwhy_geterrmessage <- function() {
  fn <- tryCatch(
    get("geterrmessage", mode = "function"),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    fn <- tryCatch(
      get("geterrmessage", envir = asNamespace("utils"), mode = "function"),
      error = function(e) NULL
    )
  }
  if (is.null(fn)) return("")
  fn()
}


#' @title Try to Get Source Context from RStudio
#' @noRd
try_get_source_context <- function(selection_only = FALSE, max_lines = 50) {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE)) return(NULL)
    if (!rstudioapi::isAvailable()) return(NULL)

    context <- rstudioapi::getActiveDocumentContext()

    selection <- context$selection[[1]]$text
    if (nzchar(trimws(selection))) return(selection)
    if (isTRUE(selection_only)) return(NULL)

    content <- paste(context$contents, collapse = "\n")
    if (nzchar(trimws(content))) {
      lines <- strsplit(content, "\n")[[1]]
      if (length(lines) > max_lines) {
        n <- length(lines)
        lines <- lines[(n - max_lines + 1):n]
      }
      return(paste(lines, collapse = "\n"))
    }
    NULL
  }, error = function(e) NULL)
}


#' @title Print a Welcome Message
#' @noRd
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(
      cli::format_message(c(
        "",
        t("welcome_title"),
        "",
        t("welcome_quick_start"),
        t("welcome_set_key"),
        t("welcome_why"),
        t("welcome_ask_r"),
        t("welcome_watch"),
        ""
      ))
    )

    if (is.null(get_ai_key())) {
      packageStartupMessage(
        cli::format_message(c(
          "x" = t("welcome_no_key"),
          "i" = t("welcome_supported")
        ))
      )
    }
  }
}


#' @noRd
format_message <- function(template, ...) {
  values <- list(...)
  if (length(values) == 0) return(template)

  for (nm in names(values)) {
    template <- gsub(
      paste0("\\{", nm, "\\}"),
      as.character(values[[nm]]),
      template,
      fixed = FALSE
    )
  }

  template
}
