#' @title Extract R Code from Markdown
#' @description Robustly extract R code blocks from LLM responses that may be
#'   wrapped in Markdown fences.
#'
#'   The extraction strategy is:
#'   \enumerate{
#'     \item If the text contains fenced code blocks (\verb{```r} or \verb{```R}),
#'           extract the content inside them and join with newlines.
#'     \item If no fences are found but the text looks like valid R code
#'           (heuristic), return the text as-is after trimming.
#'     \item Otherwise return the original text so the caller can decide.
#'   }
#'
#' @param text Character. The raw response from the LLM.
#' @return A character string containing the extracted R code.
#' @keywords internal
extract_code <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")

  # Strategy 1: Extract fenced code blocks (```r, ```R, ```{r})
  # This regex matches ```r, ```R, ```{r}, ```{r, ...} opening patterns
  pattern <- "(?s)```(?:\\{r[^}]*\\}|[rR])\\s*\\n(.*?)```"

  matches <- gregexpr(pattern, text, perl = TRUE)
  reg_matches <- regmatches(text, matches)

  if (length(reg_matches[[1]]) > 0 && any(nzchar(reg_matches[[1]]))) {
    # We have fenced blocks; extract just the inner content
    # Use a capture group approach
    code_parts <- character(0)
    for (m in reg_matches[[1]]) {
      # Remove the opening fence line and closing ```
      inner <- sub("^```(?:\\{r[^}]*\\}|[rR])\\s*\\n", "", m)
      inner <- sub("\\n?```\\s*$", "", inner)
      code_parts <- c(code_parts, trimws(inner))
    }
    return(paste(code_parts, collapse = "\n\n"))
  }

  # Strategy 2: No fences found. Check if the text looks like raw code.
  trimmed <- trimws(text)

  # Heuristic: if the text starts with common R patterns, treat as code
  r_patterns <- c(
    "^library\\(", "^require\\(", "^ggplot", "^data\\.frame",
    "^df\\s*<-", "^[a-zA-Z][a-zA-Z0-9_.]*\\s*<-", "^read\\.",
    "^source\\(", "^function\\(", "^if\\s*\\(", "^for\\s*\\(",
    "^while\\s*\\(", "^c\\(", "^list\\(", "^seq\\(", "^rep\\(",
    "^summary\\(", "^print\\(", "^plot\\(", "^head\\(", "^str\\("
  )

  starts_like_code <- any(grepl(paste(r_patterns, collapse = "|"), trimmed))

  # Also check: if the text does NOT contain typical prose indicators
  # (multiple sentences, explanations)
  no_prose <- !grepl("[.!?].*[.!?]", trimmed) || starts_like_code

  if (starts_like_code || no_prose) {
    return(trimmed)
  }

  # Strategy 3: Return as-is (the caller will display it as explanation text)
  trimmed
}
