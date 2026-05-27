#' @title Internationalization (i18n) Engine
#' @description Simple language auto-detection and message translation system.
#'   Detects the user's locale and returns messages in Chinese (zh) or
#'   English (en). Can be overridden with \code{options(rwhy.lang = "zh")}.
#' @noRd
NULL


#' @title Detect User Language
#' @description Detect the user's preferred language from environment variables
#'   and R locale settings.
#' @return Character: \code{"zh"} or \code{"en"}.
#' @noRd
.detect_lang <- function() {
  # 1. Explicit override
  override <- getOption("rwhy.lang")
  if (!is.null(override) && override %in% c("zh", "en")) return(override)

  # 2. Environment variable LANG (Unix/macOS)
  lang_env <- tolower(Sys.getenv("LANG", ""))
  if (grepl("^zh", lang_env)) return("zh")

  # 3. R locale
  for (cat in c("LC_MESSAGES", "LC_ALL", "LC_CTYPE")) {
    loc <- tryCatch(Sys.getlocale(cat), error = function(e) "")
    if (grepl("[Cc][Hh][Nn]|zh_", loc)) return("zh")
  }

  # 4. Windows system language
  if (.Platform$OS.type == "windows") {
    sys_lang <- tryCatch(
      system2("cmd", c("/c", "echo %LANG%"), stdout = TRUE, stderr = TRUE),
      error = function(e) ""
    )
    if (any(grepl("zh", sys_lang, ignore.case = TRUE))) return("zh")
  }

  "en"
}


#' @title Get or Set rwhy Language
#' @description Returns the current interface language for rwhy messages.
#'   Override with \code{rwhy_lang("zh")} or \code{rwhy_lang("en")}.
#' @param lang Character. Set to \code{"zh"} or \code{"en"} to override.
#'   Use \code{NULL} to re-enable auto-detection.
#' @return The current language code (\code{"zh"} or \code{"en"}).
#' @export
#' @examples
#' \dontrun{
#' rwhy_lang()       # Check current language
#' rwhy_lang("zh")   # Force Chinese
#' rwhy_lang("en")   # Force English
#' rwhy_lang(NULL)   # Re-enable auto-detection
#' }
rwhy_lang <- function(lang = NULL) {
  if (!is.null(lang)) {
    if (!lang %in% c("zh", "en", "auto")) {
      cli::cli_abort("Language must be {.val zh}, {.val en}, or {.val auto}.")
    }
    if (lang == "auto") {
      options(rwhy.lang = NULL)
    } else {
      options(rwhy.lang = lang)
    }
  }
  getOption("rwhy.lang", .detect_lang())
}


#' @title Translate a Message Key
#' @description Look up a message key in the translation database and return
#'   the string in the current language. Falls back to English if the key
#'   is not found in the current language, and to the key itself if not found
#'   at all.
#' @param key Character. The message key.
#' @return The translated string.
#' @noRd
t <- function(key) {
  lang <- rwhy_lang()
  entry <- .msg[[key]]
  if (is.null(entry)) return(key)
  text <- entry[[lang]]
  if (is.null(text)) text <- entry[["en"]]
  if (is.null(text)) return(key)
  text
}


# ==============================================================================
# Message Database
# ==============================================================================
# Each entry: list(en = "English text", zh = "\u4e2d\u6587\u6587\u672c")
# Uses \u escapes for non-ASCII to pass R CMD check without warnings.
# ==============================================================================

.msg <- list(

  # -- set_ai_key --
  key_invalid = list(
    en = "{.arg key} must be a non-empty character string.",
    zh = "{.arg key} \u5fc5\u987b\u662f\u975e\u7a7a\u5b57\u7b26\u4e32\u3002"
  ),
  key_prompt = list(
    en = "Paste your API key: ",
    zh = "\u7c98\u8d34\u60a8\u7684 API \u5bc6\u94a5\uff1a"
  ),
  key_missing_noninteractive = list(
    en = "{.fn set_ai_key} needs {.arg key} in non-interactive sessions.",
    zh = "\u975e\u4ea4\u4e92\u5f0f\u4f1a\u8bdd\u4e2d {.fn set_ai_key} \u9700\u8981 {.arg key} \u53c2\u6570\u3002"
  ),
  key_missing_hint = list(
    en = "Use {.code set_ai_key(\"sk-...\")} after selecting a provider with {.fn use_*}, or run {.fn set_ai_key} from an interactive R console.",
    zh = "\u8bf7\u5148\u7528 {.fn use_*} \u9009\u62e9\u670d\u52a1\u5546\u540e\u8fd0\u884c {.code set_ai_key(\"sk-...\")}\uff0c\u6216\u5728\u4ea4\u4e92\u5f0f R \u63a7\u5236\u53f0\u4e2d\u8fd0\u884c {.fn set_ai_key}\u3002"
  ),
  key_saved = list(
    en = "API key saved to {.file ~/.Renviron}. Restart R to reload.",
    zh = "API \u5bc6\u94a5\u5df2\u4fdd\u5b58\u81f3 {.file ~/.Renviron}\uff0c\u8bf7\u91cd\u542f R \u4ee5\u751f\u6548\u3002"
  ),
  key_session = list(
    en = "API key set for this session only.",
    zh = "API \u5bc6\u94a5\u4ec5\u5f53\u524d\u4f1a\u8bdd\u6709\u6548\u3002"
  ),
  key_removed = list(
    en = "API key removed from this session and {.file ~/.Renviron}.",
    zh = "API \u5bc6\u94a5\u5df2\u4ece\u5f53\u524d\u4f1a\u8bdd\u548c {.file ~/.Renviron} \u79fb\u9664\u3002"
  ),
  key_removed_session = list(
    en = "API key removed from this session.",
    zh = "API \u5bc6\u94a5\u5df2\u4ece\u5f53\u524d\u4f1a\u8bdd\u79fb\u9664\u3002"
  ),

  # -- ask_llm --
  llm_no_key = list(
    en = "No API key set.",
    zh = "\u5c1a\u672a\u914d\u7f6e API \u5bc6\u94a5\u3002"
  ),
  llm_no_key_hint = list(
    en = "Run {.fn set_ai_key} first: {.code set_ai_key(\"sk-xxx\")}",
    zh = "\u8bf7\u5148\u8fd0\u884c {.fn set_ai_key}\uff1a{.code set_ai_key(\"sk-xxx\")}"
  ),
  llm_connect_fail = list(
    en = "Failed to connect to the AI service.",
    zh = "\u65e0\u6cd5\u8fde\u63a5 AI \u670d\u52a1\u3002"
  ),
  llm_http_error = list(
    en = "API returned HTTP {.val {status}}.",
    zh = "API \u8fd4\u56de HTTP {.val {status}}\u3002"
  ),
  llm_empty = list(
    en = "The model returned an empty response.",
    zh = "\u6a21\u578b\u8fd4\u56de\u4e86\u7a7a\u54cd\u5e94\u3002"
  ),
  llm_parse_fail = list(
    en = "Failed to parse the AI service response.",
    zh = "\u65e0\u6cd5\u89e3\u6790 AI \u670d\u52a1\u54cd\u5e94\u3002"
  ),
  llm_response_preview = list(
    en = "Response preview: {body}",
    zh = "\u54cd\u5e94\u9884\u89c8\uff1a{body}"
  ),

  # -- why --
  why_no_error = list(
    en = "No recent error found. Run some code that fails first!",
    zh = "\u672a\u53d1\u73b0\u6700\u8fd1\u7684\u62a5\u9519\u3002\u8bf7\u5148\u8fd0\u884c\u4e00\u6bb5\u4f1a\u51fa\u9519\u7684\u4ee3\u7801\uff01"
  ),
  why_asking = list(
    en = "Asking AI about the error...",
    zh = "\u6b63\u5728\u8be2\u95ee AI \u5173\u4e8e\u8be5\u62a5\u9519..."
  ),
  why_auto_detect = list(
    en = "rwhy detected an error, asking AI...",
    zh = "rwhy \u68c0\u6d4b\u5230\u62a5\u9519\uff0c\u6b63\u5728\u8be2\u95ee AI..."
  ),
  why_auto_title = list(
    en = "rwhy auto-explain",
    zh = "rwhy \u81ea\u52a8\u89e3\u91ca"
  ),
  why_auto_fail = list(
    en = "rwhy failed to explain: {conditionMessage(e)}",
    zh = "rwhy \u89e3\u91ca\u5931\u8d25\uff1a{conditionMessage(e)}"
  ),

  # -- watch --
  watch_on = list(
    en = "rwhy auto-watch is {.strong ON}. Errors will be auto-explained.",
    zh = "rwhy \u81ea\u52a8\u76d1\u63a7\u5df2{.strong \u5f00\u542f}\u3002\u62a5\u9519\u5c06\u88ab\u81ea\u52a8\u89e3\u91ca\u3002"
  ),
  watch_already_on = list(
    en = "rwhy auto-watch is already {.strong ON}.",
    zh = "rwhy \u81ea\u52a8\u76d1\u63a7\u5df2\u7ecf{.strong \u5f00\u542f}\u3002"
  ),
  watch_off = list(
    en = "rwhy auto-watch is {.strong OFF}.",
    zh = "rwhy \u81ea\u52a8\u76d1\u63a7\u5df2{.strong \u5173\u95ed}\u3002"
  ),

  # -- ask_r --
  ask_invalid = list(
    en = "{.arg query} must be a non-empty character string.",
    zh = "{.arg query} \u5fc5\u987b\u662f\u975e\u7a7a\u5b57\u7b26\u4e32\u3002"
  ),
  ask_empty_code = list(
    en = "The model did not return executable R code.",
    zh = "\u6a21\u578b\u672a\u8fd4\u56de\u53ef\u6267\u884c\u7684 R \u4ee3\u7801\u3002"
  ),
  ask_parse_error = list(
    en = "The generated code is not valid R code.",
    zh = "\u751f\u6210\u7684\u4ee3\u7801\u4e0d\u662f\u6709\u6548\u7684 R \u4ee3\u7801\u3002"
  ),
  ask_risky_code = list(
    en = "The generated code contains potentially risky operations. Review it carefully before running.",
    zh = "\u751f\u6210\u7684\u4ee3\u7801\u5305\u542b\u6f5c\u5728\u9ad8\u98ce\u9669\u64cd\u4f5c\u3002\u8fd0\u884c\u524d\u8bf7\u4ed4\u7ec6\u68c0\u67e5\u3002"
  ),
  ask_writing = list(
    en = "AI is writing code for: {.emph {query}}",
    zh = "AI \u6b63\u5728\u4e3a\u60a8\u7f16\u5199\u4ee3\u7801\uff1a{.emph {query}}"
  ),
  ask_generated = list(
    en = "Generated Code",
    zh = "\u751f\u6210\u7684\u4ee3\u7801"
  ),
  ask_run_prompt = list(
    en = "Run this code? [y/N/e(=edit in script)] ",
    zh = "\u662f\u5426\u8fd0\u884c\u8be5\u4ee3\u7801\uff1f[y/N/e(=\u63d2\u5165\u5230\u811a\u672c)] "
  ),
  ask_running = list(
    en = "Running the code...",
    zh = "\u6b63\u5728\u8fd0\u884c\u4ee3\u7801..."
  ),
  ask_exec_error = list(
    en = "Execution error: {conditionMessage(e)}",
    zh = "\u6267\u884c\u51fa\u9519\uff1a{conditionMessage(e)}"
  ),
  ask_suggest_why = list(
    en = "Run {.fn why} to get an AI explanation of this error.",
    zh = "\u8fd0\u884c {.fn why} \u8ba9 AI \u89e3\u91ca\u8be5\u62a5\u9519\u3002"
  ),
  ask_exec_warning = list(
    en = "Warning: {conditionMessage(w)}",
    zh = "\u8b66\u544a\uff1a{conditionMessage(w)}"
  ),
  ask_inserted = list(
    en = "Code inserted at cursor position in RStudio.",
    zh = "\u4ee3\u7801\u5df2\u63d2\u5165\u5230 RStudio \u5149\u6807\u4f4d\u7f6e\u3002"
  ),
  ask_insert_script = list(
    en = "Code inserted into your script. Edit and run as needed.",
    zh = "\u4ee3\u7801\u5df2\u63d2\u5165\u5230\u60a8\u7684\u811a\u672c\u4e2d\uff0c\u7f16\u8f91\u540e\u8fd0\u884c\u5373\u53ef\u3002"
  ),
  ask_insert_no_rstudio = list(
    en = "Insert mode requires RStudio. Copy the code above manually.",
    zh = "\u63d2\u5165\u6a21\u5f0f\u9700\u8981 RStudio\u3002\u8bf7\u624b\u52a8\u590d\u5236\u4e0a\u65b9\u4ee3\u7801\u3002"
  ),
  ask_not_executed = list(
    en = "Code not executed. Copy it from above if needed.",
    zh = "\u4ee3\u7801\u672a\u6267\u884c\u3002\u5982\u9700\u4f7f\u7528\uff0c\u8bf7\u590d\u5236\u4e0a\u65b9\u4ee3\u7801\u3002"
  ),
  ask_not_interactive = list(
    en = "Code not executed because this is a non-interactive session.",
    zh = "\u5f53\u524d\u662f\u975e\u4ea4\u4e92\u5f0f\u4f1a\u8bdd\uff0c\u4ee3\u7801\u672a\u6267\u884c\u3002"
  ),

  # -- provider / configure --
  provider_unknown = list(
    en = "Unknown provider: {.val {provider_id}}",
    zh = "\u672a\u77e5\u670d\u52a1\u5546\uff1a{.val {provider_id}}"
  ),
  provider_model_warn = list(
    en = "{.val {model}} is not in the recommended model list for {provider$name}. Recommended: {.val {paste(provider$models, collapse = ', ')}}",
    zh = "{.val {model}} \u4e0d\u5728 {provider$name} \u7684\u63a8\u8350\u6a21\u578b\u5217\u8868\u4e2d\u3002\u63a8\u8350\uff1a{.val {paste(provider$models, collapse = ', ')}}"
  ),
  provider_switched = list(
    en = "Switched to {.emph {provider$name}} ({.val {chosen_model}})",
    zh = "\u5df2\u5207\u6362\u81f3 {.emph {provider$name}} ({.val {chosen_model}})"
  ),
  provider_need_key = list(
    en = "Don't forget to set your API key: {.fn set_ai_key}(\"...\")",
    zh = "\u522b\u5fd8\u4e86\u8bbe\u7f6e API \u5bc6\u94a5\uff1a{.fn set_ai_key}(\"...\")"
  ),
  provider_get_key = list(
    en = "Get your key: {.url {provider$key_url}}",
    zh = "\u83b7\u53d6\u5bc6\u94a5\uff1a{.url {provider$key_url}}"
  ),

  # -- configure wizard --
  cfg_title = list(
    en = "rwhy Configuration Wizard",
    zh = "rwhy \u914d\u7f6e\u5411\u5bfc"
  ),
  cfg_interactive = list(
    en = "{.fn configure} is designed for interactive use only.",
    zh = "{.fn configure} \u4ec5\u652f\u6301\u4ea4\u4e92\u5f0f\u4f7f\u7528\u3002"
  ),
  cfg_use_cli = list(
    en = "Use {.fn use_*} and {.fn set_ai_key} in scripts instead.",
    zh = "\u811a\u672c\u4e2d\u8bf7\u4f7f\u7528 {.fn use_*} \u548c {.fn set_ai_key}\u3002"
  ),
  cfg_current = list(
    en = "Current configuration:",
    zh = "\u5f53\u524d\u914d\u7f6e\uff1a"
  ),
  cfg_provider = list(
    en = "Provider: {.emph {name}}",
    zh = "\u670d\u52a1\u5546\uff1a{.emph {name}}"
  ),
  cfg_model = list(
    en = "Model: {.val {model}}",
    zh = "\u6a21\u578b\uff1a{.val {model}}"
  ),
  cfg_key_status = list(
    en = "API Key: {.val {status}}",
    zh = "API \u5bc6\u94a5\uff1a{.val {status}}"
  ),
  cfg_not_set = list(
    en = "(not set)",
    zh = "(\u672a\u8bbe\u7f6e)"
  ),
  cfg_select_provider = list(
    en = "Select an LLM provider:",
    zh = "\u9009\u62e9 AI \u670d\u52a1\u5546\uff1a"
  ),
  cfg_current_tag = list(
    en = " (current)",
    zh = " (\u5f53\u524d)"
  ),
  cfg_keep = list(
    en = "Keep current config",
    zh = "\u4fdd\u6301\u5f53\u524d\u914d\u7f6e"
  ),
  cfg_enter_number = list(
    en = "Enter number (0-6): ",
    zh = "\u8f93\u5165\u7f16\u53f7 (0-6)\uff1a"
  ),
  cfg_keeping = list(
    en = "Keeping current configuration.",
    zh = "\u4fdd\u6301\u5f53\u524d\u914d\u7f6e\u3002"
  ),
  cfg_invalid = list(
    en = "Invalid choice. Run {.fn configure} again.",
    zh = "\u65e0\u6548\u9009\u62e9\u3002\u8bf7\u91cd\u65b0\u8fd0\u884c {.fn configure}\u3002"
  ),
  cfg_select_model = list(
    en = "Select a model for {name}:",
    zh = "\u4e3a {name} \u9009\u62e9\u6a21\u578b\uff1a"
  ),
  cfg_recommended = list(
    en = " (recommended)",
    zh = " (\u63a8\u8350)"
  ),
  cfg_use_default = list(
    en = "Use default ({model})",
    zh = "\u4f7f\u7528\u9ed8\u8ba4 ({model})"
  ),
  cfg_enter_model = list(
    en = "Enter number (0-choice): ",
    zh = "\u8f93\u5165\u7f16\u53f7 (0-\u4efb\u9009)\uff1a"
  ),
  cfg_invalid_model = list(
    en = "Invalid choice, using default: {.val {model}}",
    zh = "\u65e0\u6548\u9009\u62e9\uff0c\u4f7f\u7528\u9ed8\u8ba4\u6a21\u578b\uff1a{.val {model}}"
  ),
  cfg_need_key = list(
    en = "You need an API key for {name}.",
    zh = "\u60a8\u9700\u8981\u4e3a {name} \u914d\u7f6e API \u5bc6\u94a5\u3002"
  ),
  cfg_current_key = list(
    en = "Current key starts with {.val {prefix}}...",
    zh = "\u5f53\u524d\u5bc6\u94a5\u4ee5 {.val {prefix}}... \u5f00\u5934"
  ),
  cfg_get_key = list(
    en = "Get your key at: {.url {url}}",
    zh = "\u83b7\u53d6\u5bc6\u94a5\uff1a{.url {url}}"
  ),
  cfg_paste_key = list(
    en = "Paste your API key (or press Enter to skip): ",
    zh = "\u7c98\u8d34 API \u5bc6\u94a5\uff08\u6216\u6309\u56de\u8f66\u8df3\u8fc7\uff09\uff1a"
  ),
  cfg_key_unchanged = list(
    en = "API key not changed.",
    zh = "API \u5bc6\u94a5\u672a\u53d8\u66f4\u3002"
  ),
  cfg_ollama = list(
    en = "Ollama mode -- no API key needed.",
    zh = "Ollama \u6a21\u5f0f \u2014\u2014 \u65e0\u9700 API \u5bc6\u94a5\u3002"
  ),
  custom_title = list(
    en = "Custom OpenAI-compatible API",
    zh = "\u81ea\u5b9a\u4e49 OpenAI \u517c\u5bb9 API"
  ),
  custom_endpoint_help = list(
    en = "Enter a full endpoint, or press Enter to provide host and port separately.",
    zh = "\u8f93\u5165\u5b8c\u6574 endpoint\uff0c\u6216\u76f4\u63a5\u56de\u8f66\u540e\u5206\u522b\u8f93\u5165 host \u548c\u7aef\u53e3\u3002"
  ),
  custom_endpoint_prompt = list(
    en = "Endpoint [e.g. http://localhost:8000/v1/chat/completions]: ",
    zh = "Endpoint [\u4f8b\u5982 http://localhost:8000/v1/chat/completions]\uff1a"
  ),
  custom_host_prompt = list(
    en = "Host [e.g. localhost or http://localhost]: ",
    zh = "Host [\u4f8b\u5982 localhost \u6216 http://localhost]\uff1a"
  ),
  custom_port_prompt = list(
    en = "Port [e.g. 8000, optional]: ",
    zh = "\u7aef\u53e3 [\u4f8b\u5982 8000\uff0c\u53ef\u9009]\uff1a"
  ),
  custom_model_prompt = list(
    en = "Model name: ",
    zh = "\u6a21\u578b\u540d\u79f0\uff1a"
  ),
  custom_key_help = list(
    en = "Paste the API key for this custom endpoint. Use a dummy value only if your endpoint does not require authentication.",
    zh = "\u7c98\u8d34\u8be5\u81ea\u5b9a\u4e49 endpoint \u7684 API \u5bc6\u94a5\u3002\u5982\u679c\u8be5 endpoint \u4e0d\u9700\u8981\u9274\u6743\uff0c\u53ef\u4f7f\u7528\u4efb\u610f\u5360\u4f4d\u503c\u3002"
  ),
  custom_endpoint_invalid = list(
    en = "Custom endpoint must be a non-empty URL or host.",
    zh = "\u81ea\u5b9a\u4e49 endpoint \u5fc5\u987b\u662f\u975e\u7a7a URL \u6216 host\u3002"
  ),
  custom_port_invalid = list(
    en = "Custom port must contain digits only.",
    zh = "\u81ea\u5b9a\u4e49\u7aef\u53e3\u53ea\u80fd\u5305\u542b\u6570\u5b57\u3002"
  ),
  custom_model_invalid = list(
    en = "Custom model name must be a non-empty character string.",
    zh = "\u81ea\u5b9a\u4e49\u6a21\u578b\u540d\u79f0\u5fc5\u987b\u662f\u975e\u7a7a\u5b57\u7b26\u4e32\u3002"
  ),
  custom_saved = list(
    en = "Custom OpenAI-compatible API configured.",
    zh = "\u81ea\u5b9a\u4e49 OpenAI \u517c\u5bb9 API \u5df2\u914d\u7f6e\u3002"
  ),
  cfg_complete = list(
    en = "Configuration Complete",
    zh = "\u914d\u7f6e\u5b8c\u6210"
  ),
  cfg_configured = list(
    en = "configured",
    zh = "\u5df2\u914d\u7f6e"
  ),
  cfg_ready = list(
    en = "You're ready to go! Try: {.fn why}() or {.fn ask_r}(\"your question\")",
    zh = "\u51c6\u5907\u5c31\u7eea\uff01\u8bd5\u8bd5\uff1a{.fn why}() \u6216 {.fn ask_r}(\"your question\")"
  ),

  # -- status --
  status_title = list(
    en = "rwhy status",
    zh = "rwhy \u72b6\u6001"
  ),
  status_language = list(
    en = "Language: {.val {lang}}",
    zh = "\u8bed\u8a00\uff1a{.val {lang}}"
  ),
  status_watch = list(
    en = "Auto-watch: {.val {status}}",
    zh = "\u81ea\u52a8\u76d1\u63a7\uff1a{.val {status}}"
  ),
  status_on = list(
    en = "on",
    zh = "\u5f00\u542f"
  ),
  status_off = list(
    en = "off",
    zh = "\u5173\u95ed"
  ),
  status_key_hint = list(
    en = "Set an API key with {.fn set_ai_key} or run {.fn configure}.",
    zh = "\u8bf7\u4f7f\u7528 {.fn set_ai_key} \u8bbe\u7f6e API \u5bc6\u94a5\uff0c\u6216\u8fd0\u884c {.fn configure}\u3002"
  ),

  # -- welcome (.onAttach) --
  welcome_title = list(
    en = "{.pkg rwhy} - Your AI R Programming Assistant",
    zh = "{.pkg rwhy} - \u4f60\u7684 AI R \u8bed\u8a00\u7f16\u7a0b\u52a9\u624b"
  ),
  welcome_quick_start = list(
    en = "Quick start:",
    zh = "\u5feb\u901f\u5f00\u59cb\uff1a"
  ),
  welcome_set_key = list(
    en = "  {.fn set_ai_key}(\"sk-xxx\")   # Set your API key (one time)",
    zh = "  {.fn set_ai_key}(\"sk-xxx\")   # \u8bbe\u7f6e API \u5bc6\u94a5\uff08\u4ec5\u4e00\u6b21\uff09"
  ),
  welcome_why = list(
    en = "  {.fn why}()                    # Explain the last error",
    zh = "  {.fn why}()                    # \u89e3\u91ca\u4e0a\u4e00\u6b21\u62a5\u9519"
  ),
  welcome_ask_r = list(
    en = "  {.fn ask_r}(\"...\")            # Generate R code from text",
    zh = "  {.fn ask_r}(\"...\")            # \u7528\u81ea\u7136\u8bed\u8a00\u751f\u6210 R \u4ee3\u7801"
  ),
  welcome_watch = list(
    en = "  {.fn watch_on}()               # Auto-explain every error",
    zh = "  {.fn watch_on}()               # \u81ea\u52a8\u89e3\u91ca\u6bcf\u4e00\u6b21\u62a5\u9519"
  ),
  welcome_no_key = list(
    en = "No API key detected. Run {.fn set_ai_key} to get started.",
    zh = "\u672a\u68c0\u6d4b\u5230 API \u5bc6\u94a5\u3002\u8bf7\u8fd0\u884c {.fn set_ai_key} \u5f00\u59cb\u4f7f\u7528\u3002"
  ),
  welcome_supported = list(
    en = "Supported: DeepSeek, GLM, Tongyi Qwen, Kimi, OpenAI, and any OpenAI-compatible API.",
    zh = "\u652f\u6301\uff1aDeepSeek\u3001\u667a\u8c31GLM\u3001\u901a\u4e49\u5343\u95ee\u3001Kimi\u3001OpenAI \u7b49\u6240\u6709 OpenAI \u517c\u5bb9 API\u3002"
  ),

  # -- LLM system prompts --
  # These are sent to the AI model, telling it which language to respond in
  prompt_error_lang = list(
    en = "Respond in English.",
    zh = "\u8bf7\u7528\u4e2d\u6587\u56de\u7b54\u3002"
  ),
  prompt_code_lang = list(
    en = "Use English for code comments.",
    zh = "\u4ee3\u7801\u6ce8\u91ca\u8bf7\u7528\u4e2d\u6587\u3002"
  ),

  # -- language override messages --
  lang_invalid = list(
    en = "Language must be {.val zh}, {.val en}, or {.val auto}.",
    zh = "\u8bed\u8a00\u53c2\u6570\u5fc5\u987b\u662f {.val zh}\u3001{.val en} \u6216 {.val auto}\u3002"
  )
)
