#' @title Manage AI API Key
#' @description Set and retrieve the API key for the LLM service.
#'   The key is stored in the \code{RWHY_API_KEY} environment variable and
#'   optionally persisted to \code{~/.Renviron} so it survives session restarts.
#' @param key Character. Your API key string (e.g. \code{"sk-xxxx"}).
#' @param persist Logical. If \code{TRUE} (default), the key is written to
#'   \code{~/.Renviron} so it is available in future R sessions.
#' @return \code{set_ai_key} invisibly returns the key.
#'   \code{get_ai_key} returns the key string or \code{NULL} if unset.
#' @examples
#' \dontrun{
#' set_ai_key("sk-your-deepseek-key-here")
#' get_ai_key()
#' }
#' @export
set_ai_key <- function(key, persist = TRUE) {
  if (!is.character(key) || nchar(key) == 0) {
    cli::cli_abort(t("key_invalid"))
  }

  Sys.setenv(RWHY_API_KEY = key)

  if (persist) {
    enr_path <- path.expand("~/.Renviron")
    lines <- if (file.exists(enr_path)) readLines(enr_path, warn = FALSE) else character(0)

    lines <- lines[!grepl("^RWHY_API_KEY\\s*=", lines)]
    lines <- c(lines, paste0("RWHY_API_KEY=\"", key, "\""))
    writeLines(lines, enr_path)

    cli::cli_alert_success(t("key_saved"))
  } else {
    cli::cli_alert_info(t("key_session"))
  }

  invisible(key)
}


#' @rdname set_ai_key
#' @export
get_ai_key <- function() {
  key <- Sys.getenv("RWHY_API_KEY")
  if (nchar(key) == 0) return(NULL)
  key
}


# ==============================================================================
# Provider Presets
# ==============================================================================

.provider_db <- list(
  deepseek = list(
    name     = "DeepSeek",
    base_url = "https://api.deepseek.com/v1/chat/completions",
    models   = c("deepseek-chat", "deepseek-reasoner"),
    default  = "deepseek-chat",
    key_url  = "https://platform.deepseek.com/api_keys",
    note     = "Very cheap, excellent for code tasks"
  ),
  glm = list(
    name     = "GLM (Zhipu AI)",
    base_url = "https://open.bigmodel.cn/api/paas/v4/chat/completions",
    models   = c("glm-4-flash", "glm-4-plus", "glm-4-long", "glm-z1-flash"),
    default  = "glm-4-flash",
    key_url  = "https://open.bigmodel.cn/usercenter/apikeys",
    note     = "GLM Coding Plan supported"
  ),
  qwen = list(
    name     = "Tongyi Qwen (DashScope)",
    base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions",
    models   = c("qwen-turbo", "qwen-plus", "qwen-max"),
    default  = "qwen-turbo",
    key_url  = "https://dashscope.console.aliyun.com/apiKey",
    note     = "Alibaba Cloud, has free tier"
  ),
  kimi = list(
    name     = "Kimi (Moonshot AI)",
    base_url = "https://api.moonshot.cn/v1/chat/completions",
    models   = c("moonshot-v1-8k", "moonshot-v1-32k", "moonshot-v1-128k"),
    default  = "moonshot-v1-8k",
    key_url  = "https://platform.moonshot.cn/console/api-keys",
    note     = "Long context support"
  ),
  openai = list(
    name     = "OpenAI",
    base_url = "https://api.openai.com/v1/chat/completions",
    models   = c("gpt-4o-mini", "gpt-4o", "gpt-4-turbo"),
    default  = "gpt-4o-mini",
    key_url  = "https://platform.openai.com/api-keys",
    note     = "Industry standard, higher cost"
  ),
  ollama = list(
    name     = "Ollama (Local)",
    base_url = "http://localhost:11434/v1/chat/completions",
    models   = c("llama3", "qwen2", "deepseek-coder-v2", "gemma2"),
    default  = "llama3",
    key_url  = "N/A (local)",
    note     = "Free, runs on your machine, no API key needed"
  )
)


#' @name use_provider
#' @aliases use_deepseek use_glm use_qwen use_kimi use_openai use_ollama
#' @title Quick-switch to an LLM Provider
#' @description One-line functions to switch the active LLM provider.
#' @param model Character. Override the default model.
#' @return Invisibly returns the updated configuration as a list.
#' @examples
#' \dontrun{
#' use_glm()
#' use_glm(model = "glm-4-plus")
#' use_openai(model = "gpt-4o")
#' }
NULL

#' @rdname use_provider
#' @export
use_deepseek <- function(model = NULL) .apply_provider("deepseek", model)
#' @rdname use_provider
#' @export
use_glm <- function(model = NULL) .apply_provider("glm", model)
#' @rdname use_provider
#' @export
use_qwen <- function(model = NULL) .apply_provider("qwen", model)
#' @rdname use_provider
#' @export
use_kimi <- function(model = NULL) .apply_provider("kimi", model)
#' @rdname use_provider
#' @export
use_openai <- function(model = NULL) .apply_provider("openai", model)
#' @rdname use_provider
#' @export
use_ollama <- function(model = NULL) .apply_provider("ollama", model)


#' @title Apply a Provider Preset
#' @keywords internal
.apply_provider <- function(provider_id, model = NULL) {
  provider <- .provider_db[[provider_id]]
  if (is.null(provider)) {
    cli::cli_abort(t("provider_unknown"))
  }

  chosen_model <- model %||% provider$default

  if (!is.null(model) && !(model %in% provider$models)) {
    cli::cli_alert_warning(t("provider_model_warn"))
  }

  ai_config(base_url = provider$base_url, model = chosen_model)

  cli::cli_alert_success(t("provider_switched"))

  if (is.null(get_ai_key()) && provider_id != "ollama") {
    cli::cli_alert_info(t("provider_need_key"))
    cli::cli_text(t("provider_get_key"))
  }

  invisible(ai_config())
}


#' @title Interactive Configuration Wizard
#' @description Launch an interactive menu to choose provider, model, and API key.
#' @return Invisibly returns the final configuration as a list.
#' @examples
#' \dontrun{ configure() }
#' @export
configure <- function() {
  if (!interactive()) {
    cli::cli_alert_warning(t("cfg_interactive"))
    cli::cli_text(t("cfg_use_cli"))
    return(invisible(NULL))
  }

  cli::cli_rule(left = t("cfg_title"), right = cli::symbol$star)
  cat("\n")

  current_key  <- get_ai_key()
  current_cfg  <- ai_config()
  current_prov <- .detect_provider(current_cfg$base_url)

  if (!is.null(current_key) || !is.null(current_prov)) {
    cli::cli_text("{.strong {t('cfg_current')}}")
    cli::cli_ul()
    name_val <- current_prov$name %||% "Unknown"
    cli::cli_li(t("cfg_provider"), name = name_val)
    cli::cli_li(t("cfg_model"), model = current_cfg$model)
    key_status <- if (is.null(current_key)) t("cfg_not_set") else paste0(substr(current_key, 1, 8), "...")
    cli::cli_li(t("cfg_key_status"), status = key_status)
    cli::cli_end()
    cat("\n")
  }

  cli::cli_text("{.strong {t('cfg_select_provider')}}\n")
  provider_names <- vapply(.provider_db, function(p) p$name, character(1))
  provider_notes <- vapply(.provider_db, function(p) p$note, character(1))

  for (i in seq_along(provider_names)) {
    default_tag <- if (.provider_db[[i]]$name == (current_prov$name %||% "")) t("cfg_current_tag") else ""
    cli::cli_text("  {.strong {i}}. {provider_names[i]}{default_tag} -- {.emph {provider_notes[i]}}")
  }
  cli::cli_text("  {.strong 0}. {t('cfg_keep')}\n")

  choice <- readline(prompt = t("cfg_enter_number"))
  choice <- suppressWarnings(as.integer(trimws(choice)))

  if (is.na(choice) || choice == 0) {
    cli::cli_alert_info(t("cfg_keeping"))
    return(invisible(ai_config()))
  }
  if (choice < 1 || choice > length(.provider_db)) {
    cli::cli_alert_danger(t("cfg_invalid"))
    return(invisible(NULL))
  }

  provider_id <- names(.provider_db)[choice]
  provider    <- .provider_db[[provider_id]]

  cat("\n")
  cli::cli_text("{.strong {t('cfg_select_model')}}\n", name = provider$name)
  for (j in seq_along(provider$models)) {
    default_tag <- if (provider$models[j] == provider$default) t("cfg_recommended") else ""
    cli::cli_text("  {.strong {j}}. {.val {provider$models[j]}}{default_tag}")
  }
  cli::cli_text("  {.strong 0}. {t('cfg_use_default')}\n", model = provider$default)

  model_choice <- readline(prompt = t("cfg_enter_model"))
  model_choice <- suppressWarnings(as.integer(trimws(model_choice)))

  if (is.na(model_choice) || model_choice == 0) {
    chosen_model <- provider$default
  } else if (model_choice >= 1 && model_choice <= length(provider$models)) {
    chosen_model <- provider$models[model_choice]
  } else {
    cli::cli_alert_warning(t("cfg_invalid_model"), model = provider$default)
    chosen_model <- provider$default
  }

  ai_config(base_url = provider$base_url, model = chosen_model)

  if (provider_id != "ollama") {
    cat("\n")
    if (is.null(current_key)) {
      cli::cli_text(t("cfg_need_key"), name = provider$name)
    } else {
      cli::cli_text(t("cfg_current_key"), prefix = substr(current_key, 1, 8))
    }
    cli::cli_text(t("cfg_get_key"), url = provider$key_url)
    cat("\n")

    key_input <- readline(prompt = t("cfg_paste_key"))
    if (nzchar(trimws(key_input))) {
      set_ai_key(trimws(key_input), persist = TRUE)
    } else {
      cli::cli_alert_info(t("cfg_key_unchanged"))
    }
  } else {
    if (is.null(current_key)) {
      Sys.setenv(RWHY_API_KEY = "ollama-local")
      cli::cli_alert_info(t("cfg_ollama"))
    }
  }

  .persist_config(provider$base_url, chosen_model)

  cat("\n")
  cli::cli_rule(left = t("cfg_complete"))
  cli::cli_ul()
  cli::cli_li(t("cfg_provider"), name = provider$name)
  cli::cli_li(t("cfg_model"), model = chosen_model)
  key_label <- if (!is.null(get_ai_key())) t("cfg_configured") else t("cfg_not_set")
  cli::cli_li(t("cfg_key_status"), status = key_label)
  cli::cli_end()
  cli::cli_text("\n{t('cfg_ready')}")
  cli::cli_rule()

  invisible(ai_config())
}


#' @title Detect Current Provider
#' @keywords internal
.detect_provider <- function(base_url) {
  for (id in names(.provider_db)) {
    if (.provider_db[[id]]$base_url == base_url) return(.provider_db[[id]])
  }
  NULL
}


#' @title Persist Provider/Model to .Renviron
#' @keywords internal
.persist_config <- function(base_url, model) {
  enr_path <- path.expand("~/.Renviron")
  lines <- if (file.exists(enr_path)) readLines(enr_path, warn = FALSE) else character(0)
  lines <- lines[!grepl("^RWHY_(BASE_URL|MODEL)\\s*=", lines)]
  lines <- c(lines,
    paste0("RWHY_BASE_URL=\"", base_url, "\""),
    paste0("RWHY_MODEL=\"", model, "\"")
  )
  writeLines(lines, enr_path)
}


#' @title Configure AI Provider (Low-level)
#' @description Get or set the base URL and model. For interactive setup use \code{\link{configure}()}.
#' @param base_url Character. The API base URL.
#' @param model Character. The model identifier.
#' @return A list with components \code{base_url} and \code{model}.
#' @export
ai_config <- function(base_url = NULL, model = NULL) {
  if (!is.null(base_url)) options(rwhy.base_url = base_url)
  if (!is.null(model))    options(rwhy.model = model)
  list(
    base_url = getOption("rwhy.base_url",
      Sys.getenv("RWHY_BASE_URL", unset = "https://api.deepseek.com/v1/chat/completions")),
    model = getOption("rwhy.model",
      Sys.getenv("RWHY_MODEL", unset = "deepseek-chat"))
  )
}


#' @title Null coalescing operator
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a
