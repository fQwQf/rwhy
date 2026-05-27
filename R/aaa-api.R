#' @title Manage AI API Key
#' @description Set and retrieve the API key for the LLM service.
#'   The key is stored in the \code{RWHY_API_KEY} environment variable and
#'   optionally persisted to \code{~/.Renviron} so it survives session restarts.
#'   When called without \code{key} in an interactive session, \code{set_ai_key()}
#'   first guides you through provider and model selection.
#' @param key Character. Your API key string (e.g. \code{"sk-xxxx"}). If
#'   \code{NULL}, \code{set_ai_key()} prompts for it in an interactive session.
#' @param persist Logical. If \code{TRUE} (default), the key is written to
#'   \code{~/.Renviron} so it is available in future R sessions.
#' @return \code{set_ai_key} invisibly returns the key.
#'   \code{get_ai_key} returns the key string or \code{NULL} if unset.
#'   \code{unset_ai_key} invisibly returns \code{NULL}.
#' @examples
#' \dontrun{
#' set_ai_key("sk-your-deepseek-key-here")
#' set_ai_key()
#' get_ai_key()
#' }
#' @export
set_ai_key <- function(key = NULL, persist = TRUE) {
  if (is.null(key)) {
    if (!interactive()) {
      cli::cli_abort(c(
        t("key_missing_noninteractive"),
        "i" = t("key_missing_hint")
      ))
    }

    setup <- interactive_provider_setup(persist = persist)
    if (identical(setup$provider_id, "ollama")) {
      return(invisible(NULL))
    }
    key <- setup$key
  }

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


#' @noRd
read_ai_key <- function(input = readline) {
  key <- input(prompt = t("key_prompt"))
  trimws(key)
}


#' @noRd
interactive_provider_setup <- function(persist = TRUE, allow_keep = FALSE,
                                       input = readline) {
  current_cfg <- ai_config()
  current_prov <- .detect_provider(current_cfg$base_url)

  provider_choice <- prompt_provider(
    current_prov = current_prov,
    allow_keep = allow_keep,
    input = input
  )
  if (is.null(provider_choice)) {
    return(list(provider_id = NULL, key = NULL))
  }

  provider_id <- provider_choice$id
  provider <- provider_choice$provider
  if (identical(provider_id, "custom")) {
    custom <- prompt_custom_provider(input = input)
    provider$base_url <- custom$base_url
    chosen_model <- custom$model
  } else {
    chosen_model <- prompt_model(provider, input = input)
  }

  ai_config(base_url = provider$base_url, model = chosen_model, provider = provider_id)
  .persist_config(provider$base_url, chosen_model, provider_id)

  if (identical(provider_id, "ollama")) {
    cli::cli_alert_info(t("cfg_ollama"))
    return(list(provider_id = provider_id, model = chosen_model, key = NULL))
  }

  if (!identical(provider_id, "custom")) {
    cli::cli_text(format_message(t("cfg_get_key"), url = provider$key_url))
  }
  key <- read_ai_key(input = input)
  if (!nzchar(key)) {
    cli::cli_abort(t("key_invalid"))
  }

  list(provider_id = provider_id, model = chosen_model, key = key, persist = persist)
}


#' @noRd
prompt_provider <- function(current_prov = NULL, allow_keep = FALSE,
                            input = readline) {
  cli::cli_text("{.strong {t('cfg_select_provider')}}\n")
  provider_names <- vapply(.provider_db, function(p) p$name, character(1))
  provider_notes <- vapply(.provider_db, function(p) p$note, character(1))

  for (i in seq_along(provider_names)) {
    default_tag <- if (.provider_db[[i]]$name == (current_prov$name %||% "")) t("cfg_current_tag") else ""
    cli::cli_text("  {.strong {i}}. {provider_names[i]}{default_tag} -- {.emph {provider_notes[i]}}")
  }
  if (isTRUE(allow_keep)) cli::cli_text("  {.strong 0}. {t('cfg_keep')}")
  cat("\n")

  choice <- input(prompt = t("cfg_enter_number"))
  choice <- suppressWarnings(as.integer(trimws(choice)))

  if (isTRUE(allow_keep) && (is.na(choice) || choice == 0)) {
    cli::cli_alert_info(t("cfg_keeping"))
    return(NULL)
  }
  if (is.na(choice) || choice < 1 || choice > length(.provider_db)) {
    cli::cli_abort(t("cfg_invalid"))
  }

  provider_id <- names(.provider_db)[choice]
  list(id = provider_id, provider = .provider_db[[provider_id]])
}


#' @noRd
prompt_model <- function(provider, input = readline) {
  cat("\n")
  cli::cli_text("{.strong {format_message(t('cfg_select_model'), name = provider$name)}}\n")
  for (j in seq_along(provider$models)) {
    default_tag <- if (provider$models[j] == provider$default) t("cfg_recommended") else ""
    cli::cli_text("  {.strong {j}}. {.val {provider$models[j]}}{default_tag}")
  }
  cli::cli_text("  {.strong 0}. {format_message(t('cfg_use_default'), model = provider$default)}\n")

  model_choice <- input(prompt = t("cfg_enter_model"))
  model_choice <- suppressWarnings(as.integer(trimws(model_choice)))

  if (is.na(model_choice) || model_choice == 0) {
    provider$default
  } else if (model_choice >= 1 && model_choice <= length(provider$models)) {
    provider$models[model_choice]
  } else {
    cli::cli_alert_warning(format_message(t("cfg_invalid_model"), model = provider$default))
    provider$default
  }
}


#' @noRd
prompt_custom_provider <- function(input = readline) {
  cli::cli_text("{.strong {t('custom_title')}}")
  cli::cli_text(t("custom_endpoint_help"))
  endpoint <- trimws(input(prompt = t("custom_endpoint_prompt")))

  if (!nzchar(endpoint)) {
    host <- trimws(input(prompt = t("custom_host_prompt")))
    port <- trimws(input(prompt = t("custom_port_prompt")))
    endpoint <- build_custom_base_url(host, port)
  } else {
    endpoint <- normalize_custom_base_url(endpoint)
  }

  model <- trimws(input(prompt = t("custom_model_prompt")))
  if (!nzchar(model)) {
    cli::cli_abort(t("custom_model_invalid"))
  }

  list(base_url = endpoint, model = model)
}


#' @noRd
build_custom_base_url <- function(host, port = NULL) {
  if (!is.character(host) || length(host) != 1 || !nzchar(trimws(host))) {
    cli::cli_abort(t("custom_endpoint_invalid"))
  }

  host <- trimws(host)
  if (!grepl("^https?://", host)) {
    host <- paste0("http://", host)
  }
  host <- sub("/+$", "", host)

  if (!is.null(port) && nzchar(trimws(port))) {
    port <- trimws(port)
    if (!grepl("^[0-9]+$", port)) {
      cli::cli_abort(t("custom_port_invalid"))
    }
    host <- sub(":[0-9]+$", "", host)
    host <- paste0(host, ":", port)
  }

  normalize_custom_base_url(host)
}


#' @noRd
normalize_custom_base_url <- function(base_url) {
  if (!is.character(base_url) || length(base_url) != 1 || !nzchar(trimws(base_url))) {
    cli::cli_abort(t("custom_endpoint_invalid"))
  }

  base_url <- trimws(base_url)
  if (!grepl("^https?://", base_url)) {
    base_url <- paste0("http://", base_url)
  }
  base_url <- sub("/+$", "", base_url)

  if (!grepl("/chat/completions$", base_url)) {
    base_url <- paste0(sub("/v1$", "", base_url), "/v1/chat/completions")
  }

  base_url
}


#' @rdname set_ai_key
#' @export
get_ai_key <- function() {
  key <- Sys.getenv("RWHY_API_KEY")
  if (nchar(key) == 0) return(NULL)
  key
}


#' @rdname set_ai_key
#' @export
unset_ai_key <- function(persist = TRUE) {
  Sys.unsetenv("RWHY_API_KEY")

  if (isTRUE(persist)) {
    enr_path <- path.expand("~/.Renviron")
    if (file.exists(enr_path)) {
      lines <- readLines(enr_path, warn = FALSE)
      lines <- lines[!grepl("^RWHY_API_KEY\\s*=", lines)]
      writeLines(lines, enr_path)
    }
    cli::cli_alert_success(t("key_removed"))
  } else {
    cli::cli_alert_info(t("key_removed_session"))
  }

  invisible(NULL)
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
    name     = "GLM (ZAI)",
    base_url = "https://open.bigmodel.cn/api/paas/v4/chat/completions",
    models   = c("glm-4-flash", "glm-4-plus", "glm-4-long", "glm-z1-flash"),
    default  = "glm-4-flash",
    key_url  = "https://open.bigmodel.cn/usercenter/apikeys",
    note     = "GLM Coding Plan supported"
  ),
  qwen = list(
    name     = "Qwen",
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
  ),
  custom = list(
    name     = "Custom OpenAI-compatible",
    base_url = NA_character_,
    models   = character(0),
    default  = NA_character_,
    key_url  = "your provider dashboard",
    note     = "Use your own endpoint, port, model, and key"
  )
)


#' @name use_provider
#' @aliases use_deepseek use_glm use_qwen use_kimi use_openai use_ollama use_custom
#' @title Quick-switch to an LLM Provider
#' @description One-line functions to switch the active LLM provider.
#' @param model Character. Override the default model.
#' @param base_url Character. Full chat completions endpoint for
#'   \code{use_custom()}.
#' @param key Character. Optional API key for \code{use_custom()}.
#' @param persist Logical. Persist custom configuration and key to
#'   \code{~/.Renviron}.
#' @return Invisibly returns the updated configuration as a list.
#' @examples
#' \dontrun{
#' use_glm()
#' use_glm(model = "glm-4-plus")
#' use_openai(model = "gpt-4o")
#' use_custom(
#'   base_url = "http://localhost:8000/v1/chat/completions",
#'   model = "my-model",
#'   key = "sk-local"
#' )
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
#' @rdname use_provider
#' @export
use_custom <- function(base_url, model, key = NULL, persist = TRUE) {
  base_url <- normalize_custom_base_url(base_url)
  if (!is.character(model) || length(model) != 1 || !nzchar(trimws(model))) {
    cli::cli_abort(t("custom_model_invalid"))
  }

  model <- trimws(model)
  ai_config(base_url = base_url, model = model, provider = "custom")
  if (isTRUE(persist)) .persist_config(base_url, model, "custom")

  if (!is.null(key)) {
    set_ai_key(key, persist = persist)
  } else if (is.null(get_ai_key())) {
    cli::cli_alert_info(t("provider_need_key"))
  }

  cli::cli_alert_success(t("custom_saved"))
  invisible(ai_config())
}


#' @title Apply a Provider Preset
#' @noRd
.apply_provider <- function(provider_id, model = NULL) {
  provider <- .provider_db[[provider_id]]
  if (is.null(provider)) {
    cli::cli_abort(t("provider_unknown"))
  }

  chosen_model <- model %||% provider$default

  if (!is.null(model) && !(model %in% provider$models)) {
    cli::cli_alert_warning(t("provider_model_warn"))
  }

  ai_config(base_url = provider$base_url, model = chosen_model, provider = provider_id)

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
    cli::cli_li(format_message(t("cfg_provider"), name = name_val))
    cli::cli_li(format_message(t("cfg_model"), model = current_cfg$model))
    key_status <- if (is.null(current_key)) t("cfg_not_set") else paste0(substr(current_key, 1, 8), "...")
    cli::cli_li(format_message(t("cfg_key_status"), status = key_status))
    cli::cli_end()
    cat("\n")
  }

  provider_choice <- prompt_provider(current_prov = current_prov, allow_keep = TRUE)
  if (is.null(provider_choice)) {
    return(invisible(ai_config()))
  }

  provider_id <- provider_choice$id
  provider <- provider_choice$provider
  if (identical(provider_id, "custom")) {
    custom <- prompt_custom_provider()
    provider$base_url <- custom$base_url
    chosen_model <- custom$model
  } else {
    chosen_model <- prompt_model(provider)
  }
  ai_config(base_url = provider$base_url, model = chosen_model, provider = provider_id)

  if (provider_id != "ollama") {
    cat("\n")
    if (identical(provider_id, "custom")) {
      cli::cli_text(t("custom_key_help"))
    } else if (is.null(current_key)) {
      cli::cli_text(format_message(t("cfg_need_key"), name = provider$name))
    } else {
      cli::cli_text(format_message(t("cfg_current_key"), prefix = substr(current_key, 1, 8)))
    }
    if (!identical(provider_id, "custom")) {
      cli::cli_text(format_message(t("cfg_get_key"), url = provider$key_url))
    }
    cat("\n")

    key_input <- readline(prompt = t("cfg_paste_key"))
    if (nzchar(trimws(key_input))) {
      set_ai_key(trimws(key_input), persist = TRUE)
    } else {
      cli::cli_alert_info(t("cfg_key_unchanged"))
    }
  } else {
    cli::cli_alert_info(t("cfg_ollama"))
  }

  .persist_config(provider$base_url, chosen_model, provider_id)

  cat("\n")
  cli::cli_rule(left = t("cfg_complete"))
  cli::cli_ul()
  cli::cli_li(format_message(t("cfg_provider"), name = provider$name))
  cli::cli_li(format_message(t("cfg_model"), model = chosen_model))
  key_label <- if (!is.null(get_ai_key())) t("cfg_configured") else t("cfg_not_set")
  cli::cli_li(format_message(t("cfg_key_status"), status = key_label))
  cli::cli_end()
  cli::cli_text("\n{t('cfg_ready')}")
  cli::cli_rule()

  invisible(ai_config())
}


#' @title Detect Current Provider
#' @noRd
.detect_provider <- function(base_url) {
  for (id in names(.provider_db)) {
    provider_url <- .provider_db[[id]]$base_url
    if (!is.na(provider_url) && identical(provider_url, base_url)) {
      provider <- .provider_db[[id]]
      provider$id <- id
      return(provider)
    }
  }
  NULL
}


#' @title Persist Provider/Model to .Renviron
#' @noRd
.persist_config <- function(base_url, model, provider = NULL) {
  enr_path <- path.expand("~/.Renviron")
  lines <- if (file.exists(enr_path)) readLines(enr_path, warn = FALSE) else character(0)
  lines <- lines[!grepl("^RWHY_(BASE_URL|MODEL|PROVIDER)\\s*=", lines)]
  lines <- c(lines,
    paste0("RWHY_BASE_URL=\"", base_url, "\""),
    paste0("RWHY_MODEL=\"", model, "\"")
  )
  if (!is.null(provider)) {
    lines <- c(lines, paste0("RWHY_PROVIDER=\"", provider, "\""))
  }
  writeLines(lines, enr_path)
}


#' @title Configure AI Provider (Low-level)
#' @description Get or set the base URL and model. For interactive setup use \code{\link{configure}()}.
#' @param base_url Character. The API base URL.
#' @param model Character. The model identifier.
#' @param provider Character. Optional provider identifier.
#' @return A list with components \code{base_url}, \code{model}, and
#'   \code{provider}.
#' @export
ai_config <- function(base_url = NULL, model = NULL, provider = NULL) {
  if (!is.null(base_url)) options(rwhy.base_url = base_url)
  if (!is.null(model))    options(rwhy.model = model)
  if (!is.null(provider)) options(rwhy.provider = provider)

  base_url <- getOption("rwhy.base_url",
    Sys.getenv("RWHY_BASE_URL", unset = "https://api.deepseek.com/v1/chat/completions"))
  detected <- .detect_provider(base_url)

  list(
    base_url = base_url,
    model = getOption("rwhy.model",
      Sys.getenv("RWHY_MODEL", unset = "deepseek-chat")),
    provider = getOption("rwhy.provider",
      Sys.getenv("RWHY_PROVIDER", unset = detected$id %||% "custom"))
  )
}


#' @title Show rwhy AI Status
#' @description Print a concise summary of the active provider, model, API key,
#'   language, and auto-watch status.
#' @return Invisibly returns a list with the current status values.
#' @export
ai_status <- function() {
  cfg <- ai_config()
  provider <- .provider_db[[cfg$provider]]
  provider_name <- provider$name %||% cfg$provider
  key <- get_ai_key()
  key_status <- if (is.null(key)) {
    t("cfg_not_set")
  } else {
    paste0(substr(key, 1, 4), "...", substr(key, max(1, nchar(key) - 3), nchar(key)))
  }
  watch_status <- if (isTRUE(getOption("rwhy.watch_on", FALSE))) t("status_on") else t("status_off")

  cli::cli_rule(left = t("status_title"))
  cli::cli_ul()
  cli::cli_li(format_message(t("cfg_provider"), name = provider_name))
  cli::cli_li(format_message(t("cfg_model"), model = cfg$model))
  cli::cli_li(format_message(t("cfg_key_status"), status = key_status))
  cli::cli_li(format_message(t("status_language"), lang = rwhy_lang()))
  cli::cli_li(format_message(t("status_watch"), status = watch_status))
  cli::cli_end()
  if (is.null(key) && !identical(cfg$provider, "ollama")) {
    cli::cli_alert_info(t("status_key_hint"))
  }
  cli::cli_rule()

  invisible(list(
    provider = cfg$provider,
    provider_name = provider_name,
    base_url = cfg$base_url,
    model = cfg$model,
    has_key = !is.null(key),
    language = rwhy_lang(),
    watch_on = isTRUE(getOption("rwhy.watch_on", FALSE))
  ))
}


#' @title Null coalescing operator
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
