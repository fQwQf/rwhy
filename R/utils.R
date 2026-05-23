#' @title Get Last Error Message
#' @keywords internal
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
#' @keywords internal
try_get_source_context <- function() {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE)) return(NULL)
    if (!rstudioapi::isAvailable()) return(NULL)

    context <- rstudioapi::getActiveDocumentContext()

    selection <- context$selection[[1]]$text
    if (nzchar(trimws(selection))) return(selection)

    content <- paste(context$contents, collapse = "\n")
    if (nzchar(trimws(content))) {
      lines <- strsplit(content, "\n")[[1]]
      if (length(lines) > 50) {
        n <- length(lines)
        lines <- lines[(n - 49):n]
      }
      return(paste(lines, collapse = "\n"))
    }
    NULL
  }, error = function(e) NULL)
}


#' @title Print a Welcome Message
#' @keywords internal
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
