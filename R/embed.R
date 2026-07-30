# ============================================================
# embed_api.R
# 純粋なRで使えるLLM Embedding APIラッパー
#
# 対応プロバイダー:
#   - Gemini  (Google AI Studio / 無料枠あり・推奨)
#   - Voyage  (Anthropic推奨・無料枠あり)
#   - OpenAI  (有料だが低コスト)
#
# 必要パッケージ: httr2 のみ
#   install.packages("httr2")
#
# APIキーの設定（プロジェクト直下の .Renviron か ~/.Renviron に記載）:
#   GEMINI_API_KEY=xxxx
#   VOYAGE_API_KEY=xxxx
#   OPENAI_API_KEY=xxxx
#
# APIキー取得先:
#   Gemini:  https://aistudio.google.com/apikey   （無料）
#   Voyage:  https://dash.voyageai.com            （無料枠あり）
#   OpenAI:  https://platform.openai.com/api-keys
#
# デフォルトモデル（2026-07-30 に全プロバイダで生存確認済み）:
#   gemini : gemini-embedding-001    3072次元
#            ※ 旧 text-embedding-004 / embedding-001 は廃止済み（404）
#   voyage : voyage-multilingual-2   1024次元
#   openai : text-embedding-3-small  1536次元
#
# レート制限・一時的エラーは httr2::req_retry() の指数バックオフで
# 自動リトライする（Retry-After ヘッダがあればそれに従う）。
# ============================================================

# 各プロバイダのデフォルトモデル
.default_models <- c(
  gemini = "gemini-embedding-001",
  voyage = "voyage-multilingual-2",
  openai = "text-embedding-3-small"
)

# HTTPエラーを初心者にも分かる言葉に翻訳して停止する
# （リトライは req_retry が済ませた後なので、ここに来た429は「使い切り」）
.perform_or_explain <- function(req, provider) {
  tryCatch(
    req_perform(req),
    httr2_http_401 = function(e) stop(
      "[", provider, "] The API rejected your key (HTTP 401). ",
      "Check the key stored in ~/.Renviron, save the file, and restart R.",
      call. = FALSE),
    httr2_http_403 = function(e) stop(
      "[", provider, "] The API refused the request (HTTP 403). ",
      "Your key may lack permission for this model, or the service may be ",
      "unavailable in your region. Original message: ", conditionMessage(e),
      call. = FALSE),
    httr2_http_429 = function(e) stop(
      "[", provider, "] Rate or quota limit reached and automatic retries ",
      "are exhausted (HTTP 429). Free tiers reset daily -- wait and rerun, ",
      "or enable billing. Original message: ", conditionMessage(e),
      call. = FALSE),
    httr2_http_404 = function(e) stop(
      "[", provider, "] Model not found (HTTP 404). The model name may be ",
      "misspelled or retired by the provider. Original message: ",
      conditionMessage(e), call. = FALSE),
    httr2_failure = function(e) stop(
      "[", provider, "] Could not reach the API at all -- check your ",
      "internet connection (or proxy/VPN settings). Original message: ",
      conditionMessage(e), call. = FALSE),
    error = function(e) stop(
      "[", provider, "] Request failed: ", conditionMessage(e),
      call. = FALSE)
  )
}

# ── 統一インターフェース ────────────────────────────────────
#
# embed(texts, provider, model, api_key)
#   texts    : character vector  （埋め込むテキスト群）
#   provider : "gemini" | "openai" | "voyage"
#   model    : NULL で各プロバイダのデフォルトモデルを使用
#   api_key  : NULL で環境変数から自動取得
#   戻り値   : 行列 (length(texts) × 次元数)、行名=texts
#              再現性のため provider / model / access_date を属性に記録
#
#' Embed texts with a commercial LLM embedding API
#'
#' Sends a character vector of texts to Gemini, Voyage AI, or OpenAI and
#' returns the embedding matrix (one row per text). Inputs are validated
#' before any API call; API-side failures are translated into
#' plain-language error messages. Rate limits are retried automatically
#' with exponential backoff.
#'
#' @param texts Character vector of texts to embed. A named vector's
#'   names become the row names of the result.
#' @param provider One of \code{"gemini"}, \code{"voyage"}, \code{"openai"}.
#' @param model Model name; \code{NULL} uses the provider default.
#' @param api_key API key; \code{NULL} reads the environment variable
#'   (\code{GEMINI_API_KEY}, \code{VOYAGE_API_KEY}, or \code{OPENAI_API_KEY}),
#'   typically stored in \code{~/.Renviron}.
#' @return A numeric matrix (texts x dimensions) with attributes
#'   \code{provider}, \code{model}, and \code{access_date}.
#' @examples
#' \dontrun{
#' emb <- embed(c("physician", "nurse", "athlete"))
#' cos_sim_matrix(emb)
#' }
#' @export
embed <- function(texts,
                  provider = "gemini",
                  model    = NULL,
                  api_key  = NULL) {
  # ── 入力検証: API呼び出しの前に、平易な言葉で止める ──────
  if (is.factor(texts)) texts <- as.character(texts)
  if (!is.character(texts))
    stop("`texts` must be a character vector (got ", class(texts)[1], "). ",
         "If your responses are in a data frame `d`, pass the column: ",
         "embed(d$response_text).", call. = FALSE)
  if (length(texts) == 0)
    stop("`texts` is empty -- there is nothing to embed.", call. = FALSE)
  bad <- which(is.na(texts) | trimws(texts) == "")
  if (length(bad) > 0)
    stop("`texts` contains missing (NA) or empty entries at position(s) ",
         paste(head(bad, 10), collapse = ", "),
         if (length(bad) > 10) " ..." else "",
         ". Remove or fill these rows before embedding.", call. = FALSE)
  long <- which(nchar(texts) > 20000)
  if (length(long) > 0)
    warning("Very long text at position(s) ",
            paste(head(long, 5), collapse = ", "),
            " (> 20,000 characters); providers may truncate or reject ",
            "inputs beyond their token limits.", call. = FALSE)

  provider <- match.arg(provider, c("gemini", "openai", "voyage"))
  if (is.null(model)) model <- .default_models[[provider]]

  # 名前付きベクトルの場合、名前を保存して削除
  text_names  <- names(texts)
  texts_clean <- as.character(unname(texts))

  fn <- switch(provider,
    gemini = .embed_gemini,
    openai = .embed_openai,
    voyage = .embed_voyage
  )
  mat <- fn(texts_clean, model = model, api_key = api_key)

  # ── 応答検証: 件数と数値の健全性 ─────────────────────────
  if (!is.matrix(mat) || nrow(mat) != length(texts_clean))
    stop("[", provider, "] The API returned ",
         if (is.matrix(mat)) nrow(mat) else 0, " embeddings for ",
         length(texts_clean), " texts. This should not happen -- ",
         "please rerun; if it persists, report it with the model name.",
         call. = FALSE)
  if (!all(is.finite(mat)))
    stop("[", provider, "] The returned embeddings contain non-finite ",
         "values. Please rerun; if it persists, report it.", call. = FALSE)

  # 元の名前を復元（名前付きベクトルだった場合）
  if (!is.null(text_names)) {
    rownames(mat) <- text_names
  } else {
    rownames(mat) <- texts_clean
  }
  if (anyDuplicated(rownames(mat)) > 0)
    warning("Duplicated row names in the embedding matrix (identical texts ",
            "or names). Name-based indexing like mat[\"text\", ] will ",
            "silently pick the first match; supply unique names(texts) ",
            "if you need name-based access.", call. = FALSE)
  attr(mat, "provider")    <- provider
  attr(mat, "model")       <- model
  attr(mat, "access_date") <- as.character(Sys.Date())
  mat
}


# ── Gemini ─────────────────────────────────────────────────
# モデル: gemini-embedding-001 (3072次元・多言語・無料枠あり)
# 公式:   https://ai.google.dev/gemini-api/docs/embeddings
# batchEmbedContents で最大100テキストを1リクエストにまとめる

.embed_gemini <- function(texts,
                           model   = "gemini-embedding-001",
                           api_key = NULL,
                           batch   = 100) {
  api_key <- api_key %||% Sys.getenv("GEMINI_API_KEY")
  if (api_key == "")
    stop("GEMINI_API_KEY が設定されていません。\n",
         "  Sys.setenv(GEMINI_API_KEY = 'your_key') か\n",
         "  ~/.Renviron に GEMINI_API_KEY=your_key を追記してください。\n",
         "  取得: https://aistudio.google.com/apikey")

  if (is.null(model)) model <- "gemini-embedding-001"

  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:batchEmbedContents",
    model
  )

  chunks <- split(texts, ceiling(seq_along(texts) / batch))
  rows   <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    body <- list(requests = lapply(unname(chunks[[i]]), function(t) list(
      model   = paste0("models/", model),
      content = list(parts = list(list(text = t)))
    )))

    req <- request(url) |>
      req_headers(`x-goog-api-key` = api_key, .redact = "x-goog-api-key") |>
      req_body_json(body) |>
      req_retry(max_tries = 8, backoff = ~ min(60, 2^.x)) |>
      req_error(body = function(resp) resp_body_string(resp))
    resp <- .perform_or_explain(req, "gemini")

    json <- resp_body_json(resp)
    rows[[i]] <- do.call(rbind, lapply(json$embeddings,
                                       function(e) unlist(e$values)))
    if (i < length(chunks)) Sys.sleep(0.2)
  }
  do.call(rbind, rows)
}


# ── OpenAI ─────────────────────────────────────────────────
# モデル: text-embedding-3-small  (1536次元)
#         text-embedding-3-large  (3072次元)
# コスト: 3-small → $0.02/1M tokens（非常に安価）
# 公式:   https://platform.openai.com/docs/api-reference/embeddings

.embed_openai <- function(texts,
                           model   = "text-embedding-3-small",
                           api_key = NULL,
                           batch   = 2048) {
  api_key <- api_key %||% Sys.getenv("OPENAI_API_KEY")
  if (api_key == "")
    stop("OPENAI_API_KEY が設定されていません。\n",
         "  取得: https://platform.openai.com/api-keys")

  if (is.null(model)) model <- "text-embedding-3-small"

  # 429 でも insufficient_quota（課金枠切れ）はリトライしても無駄なので除外
  is_transient <- function(resp) {
    s <- resp_status(resp)
    if (s %in% c(500, 502, 503, 504)) return(TRUE)
    if (s == 429)
      return(!grepl("insufficient_quota", resp_body_string(resp), fixed = TRUE))
    FALSE
  }

  chunks <- split(texts, ceiling(seq_along(texts) / batch))
  rows   <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    req <- request("https://api.openai.com/v1/embeddings") |>
      req_auth_bearer_token(api_key) |>
      req_body_json(list(input = as.list(unname(chunks[[i]])), model = model)) |>
      req_retry(max_tries = 5, backoff = ~ min(60, 2^.x),
                is_transient = is_transient) |>
      req_error(body = function(resp) resp_body_string(resp))
    resp <- .perform_or_explain(req, "openai")

    json    <- resp_body_json(resp)
    ordered <- json$data[order(sapply(json$data, function(x) x$index))]
    rows[[i]] <- do.call(rbind, lapply(ordered, function(x) unlist(x$embedding)))
    if (i < length(chunks)) Sys.sleep(0.1)
  }
  do.call(rbind, rows)
}


# ── Voyage AI (Anthropic推奨) ──────────────────────────────
# モデル: voyage-multilingual-2   (1024次元・多言語)
# 公式:   https://docs.voyageai.com/docs/embeddings
# キー取得: https://dash.voyageai.com
# 無料枠は 3 req/分 と厳しいが、429 は Retry-After に従い自動リトライする

.embed_voyage <- function(texts,
                           model   = "voyage-multilingual-2",
                           api_key = NULL,
                           batch   = 128) {
  api_key <- api_key %||% Sys.getenv("VOYAGE_API_KEY")
  if (api_key == "")
    stop("VOYAGE_API_KEY が設定されていません。\n",
         "  Sys.setenv(VOYAGE_API_KEY = 'your_key') か\n",
         "  ~/.Renviron に VOYAGE_API_KEY=your_key を追記してください。\n",
         "  取得（無料）: https://dash.voyageai.com")

  if (is.null(model)) model <- "voyage-multilingual-2"

  chunks <- split(texts, ceiling(seq_along(texts) / batch))
  rows   <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    req <- request("https://api.voyageai.com/v1/embeddings") |>
      req_auth_bearer_token(api_key) |>
      req_body_json(list(
        input      = as.list(unname(chunks[[i]])),
        model      = model,
        input_type = "document"   # "query" or "document"
      )) |>
      req_retry(max_tries = 8, backoff = ~ min(30, 2^.x)) |>
      req_error(body = function(resp) resp_body_string(resp))
    resp <- .perform_or_explain(req, "voyage")

    json      <- resp_body_json(resp)
    # OpenAIと同じレスポンス形式: data[i]$embedding
    ordered   <- json$data[order(sapply(json$data, function(x) x$index))]
    rows[[i]] <- do.call(rbind, lapply(ordered, function(x) unlist(x$embedding)))

    if (i < length(chunks)) Sys.sleep(1)
  }
  do.call(rbind, rows)
}


# ── 簡易動作確認 ────────────────────────────────────────────
# 分析・可視化ユーティリティは utils.R に分離しています。

#' Quick API connectivity check
#'
#' Embeds a single test sentence and prints the model and dimensionality.
#' @param provider One of \code{"gemini"}, \code{"voyage"}, \code{"openai"}.
#' @param ... Passed to \code{\link{embed}}.
#' @export
check_api <- function(provider = "gemini", ...) {
  cat("API接続テスト:", provider, "\n")
  test <- embed("This is a test.", provider = provider, ...)
  cat("  モデル:", attr(test, "model"), "\n")
  cat("  次元数:", ncol(test), "\n")
  cat("  最初の5成分:", round(test[1, 1:5], 4), "\n")
  cat("  OK\n")
  invisible(test)
}
# check_api("gemini")  # GEMINI_API_KEY 要
# check_api("voyage")  # VOYAGE_API_KEY 要
# check_api("openai")  # OPENAI_API_KEY 要
