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
#
# キャッシュ:
#   埋め込みは決定的なので、embed() は取得済みのテキストを
#   output/embed_cache/<provider>_<model>.rds に保存し、次回以降は
#   未取得分だけをAPIに送る。解析の再実行でクォータを消費しない。
#   無効化するには embed(..., cache = FALSE)。
#   アーカイブ済み行列からキャッシュを作るには
#   source("seed_cache_from_archive.R")。
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


# ── 進捗表示・レート制御・逐次保存の共通ヘルパー ────────────
# バッチごとに (a) 進捗を1行更新、(b) 指定RPMに合わせて待機、
# (c) 取得済み分をキャッシュへ即時保存する。途中で中断・失敗しても
# それまでに取得したベクトルは失われない。
.chunk_reporter <- function(provider, n_total, n_chunks, progress, rpm) {
  t0 <- Sys.time(); done <- 0L
  list(
    tick = function(n_new) {
      done <<- done + n_new
      if (isTRUE(progress) && n_chunks > 1) {
        el  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        eta <- if (done > 0) el / done * (n_total - done) else NA_real_
        message(sprintf("\r  [%s] %d/%d texts (%.0f%%)%s",
                        provider, done, n_total, 100 * done / n_total,
                        if (is.finite(eta) && eta > 5)
                          sprintf("  ETA ~%.0fs", eta) else ""),
                appendLF = done >= n_total)
      }
    },
    pace = function() {
      if (!is.null(rpm) && rpm > 0) Sys.sleep(60 / rpm)
    }
  )
}

# ── 統一インターフェース ────────────────────────────────────
#
# embed(texts, provider, model, api_key)
#   texts     : character vector  （埋め込むテキスト群）
#   provider  : "gemini" | "openai" | "voyage"
#   model     : NULL で各プロバイダのデフォルトモデルを使用
#   api_key   : NULL で環境変数から自動取得
#   cache     : TRUE で取得済みテキストを再利用（既定・クォータ節約）
#   refresh   : TRUE でキャッシュを無視して取り直す（既定 FALSE）
#   progress  : バッチ進捗を表示する（既定 TRUE）
#   rpm       : 1分あたりの最大リクエスト数。無料枠での事前ペース調整用
#   dry_run   : 何件を新規取得するか報告するだけで、APIを呼ばない
#   ...       : プロバイダ固有オプション（batch, dims, task_type, input_type）
#               ベクトルを変えるオプションはキャッシュを自動的に分離する
#   cache_dir : キャッシュRDSの保存先
#   戻り値    : 行列 (length(texts) × 次元数)、行名=texts
#               再現性のため provider / model / access_date を属性に記録
#
#' Embed texts with a commercial LLM embedding API
#'
#' Sends a character vector of texts to Gemini, Voyage AI, or OpenAI and
#' returns the embedding matrix (one row per text). Results are cached per
#' provider and model, so re-running an analysis costs no API calls; only
#' texts not seen before are fetched. Inputs are validated before any request,
#' API failures are reported in plain language, and partial results are written
#' to the cache after every batch, so an interrupted run loses nothing.
#'
#' @param texts Character vector of texts to embed. A named vector's names
#'   become the row names of the result.
#' @param provider One of \code{"gemini"}, \code{"voyage"}, \code{"openai"}.
#' @param model Model name; \code{NULL} uses the provider default.
#' @param api_key API key; \code{NULL} reads the environment variable
#'   (\code{GEMINI_API_KEY}, \code{VOYAGE_API_KEY}, \code{OPENAI_API_KEY}),
#'   normally stored in \code{~/.Renviron}.
#' @param cache Reuse and store previously fetched embeddings (default TRUE).
#' @param refresh Ignore cached entries and fetch again, overwriting them
#'   (default FALSE). Useful after a provider updates a model.
#' @param cache_dir Directory holding the cache files.
#' @param progress Show per-batch progress for large jobs (default TRUE).
#' @param rpm Cap requests per minute. Use on free tiers to stay under a rate
#'   limit instead of relying on retry-after-failure.
#' @param dry_run Report how many texts would be fetched and return without
#'   calling the API.
#' @param ... Provider-specific options: \code{batch}, \code{dims}
#'   (output dimensionality), \code{task_type} (Gemini), \code{input_type}
#'   (Voyage). Options that change the returned vectors are given their own
#'   cache file automatically.
#' @return A numeric matrix (texts x dimensions) with attributes
#'   \code{provider}, \code{model} and \code{access_date}. With
#'   \code{dry_run = TRUE}, an invisible list of counts.
#' @examples
#' \dontrun{
#' emb <- embed(c("physician", "nurse", "athlete"))
#' cos_sim_matrix(emb)
#'
#' # How much would this cost me? (no API calls)
#' embed(my_responses, provider = "gemini", dry_run = TRUE)
#'
#' # Free tier: pace requests instead of hitting the limit
#' emb <- embed(my_responses, provider = "gemini", rpm = 60)
#' }
#' @export
embed <- function(texts,
                  provider  = "gemini",
                  model     = NULL,
                  api_key   = NULL,
                  cache     = TRUE,
                  refresh   = FALSE,
                  cache_dir = file.path("output", "embed_cache"),
                  progress  = TRUE,
                  rpm       = NULL,
                  dry_run   = FALSE,
                  ...) {
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

  # ── キャッシュ: 埋め込みは決定的なので、同一テキストの再送信は無駄 ──
  # provider×model ごとのRDS（テキスト→ベクトルの名前付きリスト）に保存し、
  # 未取得のテキストだけをAPIに送る。cache = FALSE で無効化できる。
  cache_file <- NULL
  cached <- list()
  if (isTRUE(cache)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    # オプションがベクトルを変える場合はキャッシュを分ける
    # （例: Gemini の taskType、出力次元数、Voyage の input_type）
    opts <- list(...)
    opts <- opts[intersect(names(opts),
                           c("dims", "task_type", "input_type"))]
    sig <- if (length(opts) == 0) "" else paste0("_", paste(
      vapply(names(opts), function(k)
        paste0(substr(k, 1, 3), gsub("[^A-Za-z0-9]", "", as.character(opts[[k]]))),
        character(1)), collapse = "-"))
    cache_file <- file.path(
      cache_dir, paste0(provider, "_",
                        gsub("[^A-Za-z0-9._-]", "_", model), sig, ".rds"))
    if (file.exists(cache_file)) cached <- readRDS(cache_file)
  }
  uniq     <- unique(texts_clean)
  # refresh = TRUE なら既存のキャッシュを無視して取り直す（結果は上書き保存）。
  # モデル更新後の再取得や、決定性の確認に使う。
  to_fetch <- if (isTRUE(refresh)) uniq else uniq[!(uniq %in% names(cached))]

  # ── dry run: 何件を新規取得するかだけ報告して終了 ─────────
  if (isTRUE(dry_run)) {
    message(sprintf(
      "embed[%s/%s] dry run: %d texts -> %d cached, %d would be fetched%s",
      provider, model, length(texts_clean),
      length(texts_clean) - length(to_fetch), length(to_fetch),
      if (provider == "gemini" && length(to_fetch) > 0)
        sprintf(" (Gemini free tier allows ~1,000 per day)") else ""))
    return(invisible(list(n_texts = length(texts_clean),
                          n_cached = length(texts_clean) - length(to_fetch),
                          n_to_fetch = length(to_fetch),
                          provider = provider, model = model)))
  }

  if (length(to_fetch) > 0) {
    if (isTRUE(progress) && length(to_fetch) >= 200)
      message(sprintf("embed[%s/%s]: fetching %d new texts (%d already cached)",
                      provider, model, length(to_fetch),
                      length(texts_clean) - length(to_fetch)))
    fn <- switch(provider,
      gemini = .embed_gemini,
      openai = .embed_openai,
      voyage = .embed_voyage
    )
    # バッチごとにキャッシュへ書き出すコールバック（中断耐性）
    on_chunk <- function(texts_chunk, mat_chunk) {
      for (i in seq_along(texts_chunk))
        cached[[texts_chunk[i]]] <<- mat_chunk[i, ]
      if (!is.null(cache_file)) saveRDS(cached, cache_file)
    }
    new_mat <- fn(to_fetch, model = model, api_key = api_key,
                  progress = progress, rpm = rpm, on_chunk = on_chunk, ...)

    # ── 応答検証: 件数と数値の健全性 ───────────────────────
    if (!is.matrix(new_mat) || nrow(new_mat) != length(to_fetch))
      stop("[", provider, "] The API returned ",
           if (is.matrix(new_mat)) nrow(new_mat) else 0, " embeddings for ",
           length(to_fetch), " texts. This should not happen -- ",
           "please rerun; if it persists, report it with the model name.",
           call. = FALSE)
    if (!all(is.finite(new_mat)))
      stop("[", provider, "] The returned embeddings contain non-finite ",
           "values. Please rerun; if it persists, report it.", call. = FALSE)

  }
  if (isTRUE(cache))
    message(sprintf(
      "embed[%s/%s]: %d texts (%d from cache, %d fetched%s)",
      provider, model, length(texts_clean),
      length(texts_clean) - length(to_fetch), length(to_fetch),
      if (isTRUE(refresh)) "; cache refreshed" else ""))

  mat <- do.call(rbind, cached[texts_clean])

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
                           model     = "gemini-embedding-001",
                           api_key   = NULL,
                           batch     = 100,
                           dims      = NULL,
                           task_type = NULL,
                           progress  = TRUE,
                           rpm       = NULL,
                           on_chunk  = NULL, ...) {
  api_key <- api_key %||% Sys.getenv("GEMINI_API_KEY")
  if (api_key == "")
    stop("GEMINI_API_KEY is not set.\n",
         "  Add GEMINI_API_KEY=your_key to ~/.Renviron and restart R,\n",
         "  or call Sys.setenv(GEMINI_API_KEY = 'your_key').\n",
         "  Get a key (free): https://aistudio.google.com/apikey",
         call. = FALSE)
  if (is.null(model)) model <- "gemini-embedding-001"

  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:batchEmbedContents",
    model)
  chunks <- split(texts, ceiling(seq_along(texts) / batch))
  rep_ <- .chunk_reporter("gemini", length(texts), length(chunks), progress, rpm)
  rows <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    body <- list(requests = lapply(unname(chunks[[i]]), function(x) {
      one <- list(model = paste0("models/", model),
                  content = list(parts = list(list(text = x))))
      if (!is.null(task_type)) one$taskType <- task_type
      if (!is.null(dims))      one$outputDimensionality <- dims
      one
    }))
    req <- request(url) |>
      req_headers(`x-goog-api-key` = api_key, .redact = "x-goog-api-key") |>
      req_body_json(body) |>
      req_retry(max_tries = 8, backoff = ~ min(60, 2^.x)) |>
      req_error(body = function(resp) resp_body_string(resp))
    resp <- .perform_or_explain(req, "gemini")
    json <- resp_body_json(resp)
    rows[[i]] <- do.call(rbind, lapply(json$embeddings,
                                       function(e) unlist(e$values)))
    if (!is.null(on_chunk)) on_chunk(chunks[[i]], rows[[i]])
    rep_$tick(length(chunks[[i]]))
    if (i < length(chunks)) { rep_$pace(); Sys.sleep(0.2) }
  }
  do.call(rbind, rows)
}


# ── OpenAI ─────────────────────────────────────────────────
# モデル: text-embedding-3-small  (1536次元)
#         text-embedding-3-large  (3072次元)
# コスト: 3-small → $0.02/1M tokens（非常に安価）
# 公式:   https://platform.openai.com/docs/api-reference/embeddings

.embed_openai <- function(texts,
                           model    = "text-embedding-3-small",
                           api_key  = NULL,
                           batch    = 2048,
                           dims     = NULL,
                           progress = TRUE,
                           rpm      = NULL,
                           on_chunk = NULL, ...) {
  api_key <- api_key %||% Sys.getenv("OPENAI_API_KEY")
  if (api_key == "")
    stop("OPENAI_API_KEY is not set.\n",
         "  Add OPENAI_API_KEY=your_key to ~/.Renviron and restart R.\n",
         "  Get a key: https://platform.openai.com/api-keys", call. = FALSE)
  if (is.null(model)) model <- "text-embedding-3-small"

  is_transient <- function(resp) {
    s <- resp_status(resp)
    if (s %in% c(500, 502, 503, 504)) return(TRUE)
    if (s == 429)
      return(!grepl("insufficient_quota", resp_body_string(resp), fixed = TRUE))
    FALSE
  }

  chunks <- split(texts, ceiling(seq_along(texts) / batch))
  rep_ <- .chunk_reporter("openai", length(texts), length(chunks), progress, rpm)
  rows <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    body <- list(input = as.list(unname(chunks[[i]])), model = model)
    if (!is.null(dims)) body$dimensions <- dims
    req <- request("https://api.openai.com/v1/embeddings") |>
      req_auth_bearer_token(api_key) |>
      req_body_json(body) |>
      req_retry(max_tries = 5, backoff = ~ min(60, 2^.x),
                is_transient = is_transient) |>
      req_error(body = function(resp) resp_body_string(resp))
    resp    <- .perform_or_explain(req, "openai")
    json    <- resp_body_json(resp)
    ordered <- json$data[order(sapply(json$data, function(x) x$index))]
    rows[[i]] <- do.call(rbind, lapply(ordered, function(x) unlist(x$embedding)))
    if (!is.null(on_chunk)) on_chunk(chunks[[i]], rows[[i]])
    rep_$tick(length(chunks[[i]]))
    if (i < length(chunks)) { rep_$pace(); Sys.sleep(0.1) }
  }
  do.call(rbind, rows)
}


# ── Voyage AI (Anthropic推奨) ──────────────────────────────
# モデル: voyage-multilingual-2   (1024次元・多言語)
# 公式:   https://docs.voyageai.com/docs/embeddings
# キー取得: https://dash.voyageai.com
# 無料枠は 3 req/分 と厳しいが、429 は Retry-After に従い自動リトライする

.embed_voyage <- function(texts,
                           model      = "voyage-multilingual-2",
                           api_key    = NULL,
                           batch      = 128,
                           input_type = "document",
                           dims       = NULL,
                           progress   = TRUE,
                           rpm        = NULL,
                           on_chunk   = NULL, ...) {
  api_key <- api_key %||% Sys.getenv("VOYAGE_API_KEY")
  if (api_key == "")
    stop("VOYAGE_API_KEY is not set.\n",
         "  Add VOYAGE_API_KEY=your_key to ~/.Renviron and restart R.\n",
         "  Get a key (free tier available): https://dash.voyageai.com",
         call. = FALSE)
  if (is.null(model)) model <- "voyage-multilingual-2"

  chunks <- split(texts, ceiling(seq_along(texts) / batch))
  rep_ <- .chunk_reporter("voyage", length(texts), length(chunks), progress, rpm)
  rows <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    body <- list(input = as.list(unname(chunks[[i]])), model = model,
                 input_type = input_type)
    if (!is.null(dims)) body$output_dimension <- dims
    req <- request("https://api.voyageai.com/v1/embeddings") |>
      req_auth_bearer_token(api_key) |>
      req_body_json(body) |>
      req_retry(max_tries = 8, backoff = ~ min(30, 2^.x)) |>
      req_error(body = function(resp) resp_body_string(resp))
    resp      <- .perform_or_explain(req, "voyage")
    json      <- resp_body_json(resp)
    ordered   <- json$data[order(sapply(json$data, function(x) x$index))]
    rows[[i]] <- do.call(rbind, lapply(ordered, function(x) unlist(x$embedding)))
    if (!is.null(on_chunk)) on_chunk(chunks[[i]], rows[[i]])
    rep_$tick(length(chunks[[i]]))
    if (i < length(chunks)) { rep_$pace(); Sys.sleep(1) }
  }
  do.call(rbind, rows)
}



# ── キャッシュの検査・管理 ──────────────────────────────────

#' キャッシュの中身を一覧する
#' @param cache_dir キャッシュディレクトリ
#' @return provider / model / n_texts / dims / size_mb のデータフレーム
#' Inspect the embedding cache
#'
#' @param cache_dir Directory holding the cache files.
#' @return A data frame of provider, model, number of cached texts,
#'   dimensionality and file size.
#' @export
cache_info <- function(cache_dir = file.path("output", "embed_cache")) {
  files <- list.files(cache_dir, pattern = "[.]rds$", full.names = TRUE)
  if (length(files) == 0) {
    message("No embedding cache found in ", cache_dir)
    return(invisible(data.frame()))
  }
  do.call(rbind, lapply(files, function(f) {
    x <- readRDS(f)
    key <- sub("[.]rds$", "", basename(f))
    data.frame(provider = sub("_.*$", "", key),
               model    = sub("^[^_]+_", "", key),
               n_texts  = length(x),
               dims     = if (length(x)) length(x[[1]]) else NA_integer_,
               size_mb  = round(file.size(f) / 1024^2, 1),
               row.names = NULL)
  }))
}

#' キャッシュを削除する（provider/model を指定しなければ全消去）
#' @param provider 消去対象のプロバイダ（NULL で全部）
#' @param cache_dir キャッシュディレクトリ
#' Delete cached embeddings
#'
#' @param provider Provider to clear; \code{NULL} clears everything.
#' @param cache_dir Directory holding the cache files.
#' @return Invisibly, the number of files removed.
#' @export
cache_clear <- function(provider = NULL,
                        cache_dir = file.path("output", "embed_cache")) {
  pat <- if (is.null(provider)) "[.]rds$" else paste0("^", provider, "_.*[.]rds$")
  files <- list.files(cache_dir, pattern = pat, full.names = TRUE)
  if (length(files) == 0) { message("Nothing to clear."); return(invisible(0L)) }
  unlink(files)
  message("Cleared ", length(files), " cache file(s).")
  invisible(length(files))
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
