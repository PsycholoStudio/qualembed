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
#   voyage : voyage-4                1024次元
#            ※ 旧 voyage-multilingual-2 は legacy 扱い
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
  voyage = "voyage-4",
  openai = "text-embedding-3-small"
)

#' Locate the cache file that \code{embed()} uses
#'
#' Returns the path of the RDS file that \code{\link{embed}} reads and writes
#' for a given provider, model and set of request options. Request options that
#' change the returned vector (Gemini's \code{task_type}, Voyage's
#' \code{input_type}, an explicit output dimensionality) are part of the file
#' name, so one provider can hold several caches at once. Use this rather than
#' assembling the name yourself: a script that guessed
#' \code{"<provider>_<model>.rds"} silently missed the Gemini cache, which
#' carries a task-type suffix, and skipped the cells that needed it.
#'
#' @param provider Character; one of \code{"gemini"}, \code{"openai"},
#'   \code{"voyage"}.
#' @param model Character; the model identifier. Defaults to \code{NULL},
#'   which uses the provider default.
#' @param cache_dir Character; the directory holding the cache files. Defaults
#'   to \code{file.path("output", "embed_cache")}, which is relative to the
#'   working directory, so a session started elsewhere will not find an
#'   existing cache.
#' @param ... Request options, as passed to \code{\link{embed}}.
#' @return A file path, as a length-one character vector. The file need not
#'   exist.
#' @seealso \code{\link{embed}}, whose cache this names, and
#'   \code{\link{cache_info}} for what the cache currently holds.
#' @export
cache_path <- function(provider, model = NULL,
                       cache_dir = file.path("output", "embed_cache"), ...) {
  provider <- match.arg(provider, c("gemini", "openai", "voyage"))
  if (is.null(model)) model <- .default_models[[provider]]
  fn <- switch(provider, gemini = .embed_gemini,
                         openai = .embed_openai, voyage = .embed_voyage)
  .dots <- list(...)
  nm <- intersect(names(formals(fn)), c("dims", "task_type", "input_type"))
  opts <- lapply(nm, function(k)
    if (k %in% names(.dots)) .dots[[k]] else eval(formals(fn)[[k]]))
  names(opts) <- nm
  file.path(cache_dir, paste0(provider, "_",
            gsub("[^A-Za-z0-9._-]", "_", model), .cache_sig(opts), ".rds"))
}

# 鍵の接尾辞。NULL は「そのパラメータを送らない」を意味するので落とす。
.cache_sig <- function(opts) {
  opts <- opts[!vapply(opts, is.null, logical(1))]
  if (length(opts) == 0) return("")
  paste0("_", paste(vapply(names(opts), function(k)
    paste0(substr(k, 1, 3), gsub("[^A-Za-z0-9]", "", as.character(opts[[k]]))),
    character(1)), collapse = "-"))
}

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
#               再現性のため provider / model / access_date / texts を属性に記録
#               texts は「APIに送った文字列そのもの」。行名は呼び出し側の
#               都合で参加者IDに置き換わりうるので、本文は別に保持する。
#               attr(emb, "texts") で確認できる。
#

# ── 一括リクエストの指紋 ────────────────────────────────────
# プロバイダは一回の呼び出しでテキストの配列を受け取り、あるテキストに
# 返るベクトルは、その配列に何が同居していたかに依存する。名前のついた
# オプションではないので request_options には現れない。ここでは順序に
# 依存しない指紋（件数とハッシュ）を作り、読者が「自分は同じ集合を送ったか」
# を照合できるようにする。依存を増やさないため base R だけで書く。
# 2^24 を法にしているので衝突は皆無ではない。同定ではなく照合のための値である。
.request_fingerprint <- function(texts) {
  s <- sort(unique(texts))
  h <- 0
  for (tx in s) {
    for (cp in utf8ToInt(tx)) h <- (h * 131 + cp) %% 16777216
    h <- (h * 131 + 10) %% 16777216
  }
  sprintf("n=%d;h=%06x", length(s), h)
}

#' Embed texts with a commercial LLM embedding API
#'
#' Sends a character vector of texts to Gemini, Voyage AI, or OpenAI and
#' returns the embedding matrix (one row per text). Results are cached per
#' provider and model, so re-running an analysis costs no API calls; only
#' texts not seen before are fetched. Inputs are validated before any request,
#' API failures are reported in plain language, and partial results are written
#' to the cache after every batch, so an interrupted run loses nothing.
#'
#' @param texts A character vector of texts to embed. A named vector's names
#'   become the row names of the result.
#' @param provider Character; one of \code{"gemini"}, \code{"voyage"},
#'   \code{"openai"}. Defaults to \code{"gemini"}.
#' @param model Character; the model name. Defaults to \code{NULL}, which uses
#'   the provider default.
#' @param api_key Character; the API key. Defaults to \code{NULL}, which reads
#'   the environment variable
#'   (\code{GEMINI_API_KEY}, \code{VOYAGE_API_KEY}, \code{OPENAI_API_KEY}),
#'   normally stored in \code{~/.Renviron}.
#' @param cache Logical; whether to reuse and store previously fetched
#'   embeddings. Defaults to \code{TRUE}.
#'   One RDS file per provider, model, and set of vector-changing request
#'   options; within a file, one entry per text.
#'
#'   \strong{The cache stores your texts verbatim and unencrypted.} For
#'   participant data that makes a second plaintext copy on disk, which the
#'   study's data-management plan has to account for: pass
#'   \code{cache = FALSE}, or set \code{cache_dir} to a controlled-access
#'   location.
#'
#'   The cache key is built from the \emph{resolved} options, not from the
#'   arguments the caller typed. \code{embed(x, provider = "gemini")} and
#'   \code{embed(x, provider = "gemini", task_type = "SEMANTIC_SIMILARITY")}
#'   therefore share a file, because they send the same request; an option
#'   left at \code{NULL} is not sent and does not enter the key. The
#'   alternative --- keying on what the caller passed --- looks equivalent and
#'   is not: change a default and every call that relies on it keeps hitting
#'   the old file, so vectors fetched under the previous condition are returned
#'   silently under a key that now names a different one. Nothing errors, and
#'   nothing in the returned matrix looks wrong.
#'
#'   A consequence worth knowing when you rerun someone's code: with
#'   \code{cache = TRUE} you get their cache, not the endpoint. Delete the
#'   file, or pass \code{refresh = TRUE}, to reach the API.
#' @param refresh Logical; whether to ignore cached entries and fetch again,
#'   overwriting them. Useful after a provider updates a model. Defaults to
#'   \code{FALSE}.
#' @param cache_dir Character; the directory holding the cache files. Defaults
#'   to \code{file.path("output", "embed_cache")}, which is relative to the
#'   working directory, so a session started elsewhere will not find an
#'   existing cache.
#' @param progress Logical; whether to show per-batch progress for large jobs.
#'   Defaults to \code{TRUE}.
#' @param rpm Numeric; a cap on requests per minute. Use on free tiers to stay
#'   under a rate limit instead of relying on retry-after-failure. Defaults to
#'   \code{NULL} (no cap).
#' @param dry_run Logical; whether to report how many texts would be fetched
#'   and return without calling the API. Defaults to \code{FALSE}.
#' @param ... Provider-specific options: \code{batch}, \code{dims}
#'   (output dimensionality), \code{task_type} (Gemini), \code{input_type}
#'   (Voyage). Options that change the returned vectors are given their own
#'   cache file automatically.
#'
#'   The defaults are set so that the three providers are as close to a common
#'   condition as their APIs allow, because every statistic in this package
#'   rests on a cosine similarity matrix and is therefore a symmetric task.
#'   They are not the providers' own defaults in every case.
#'
#'   For Gemini, \code{task_type} defaults to \code{"SEMANTIC_SIMILARITY"}.
#'   Sending no task type is not a neutral option: the API enum defines
#'   \code{TASK_TYPE_UNSPECIFIED} as "unset value, which will default to one
#'   of the other enum values", it contains no value meaning "no conditioning",
#'   and omitting the field returns vectors identical to
#'   \code{"RETRIEVAL_QUERY"} --- the query side of an asymmetric retrieval
#'   pair. The symmetric task types disagree with each other about as much as
#'   two providers do, so the choice has to be stated.
#'
#'   For Voyage, \code{input_type} defaults to \code{NULL}, which is Voyage's
#'   own default and sends no instruction. Setting it to \code{"document"} or
#'   \code{"query"} makes the endpoint prepend a retrieval instruction to the
#'   text before encoding, so what is embedded is the instruction plus the text
#'   rather than the text.
#'
#'   OpenAI exposes no equivalent parameter and always encodes the string as
#'   given. A common condition across all three is therefore not attainable;
#'   state the options you used when you report a cross-provider comparison.
#' @return A numeric matrix (texts x dimensions). Four attributes identify what
#'   was embedded and by what: \code{provider}, \code{model},
#'   \code{access_date}, and \code{texts} --- the exact strings that were sent
#'   to the API, in row order. Further attributes record the conditions of the
#'   call --- \code{request_options}, \code{fetch_dates}, \code{software}, and
#'   \code{request_batches}, a fingerprint of which texts travelled in the same
#'   request (the returned vector for a text depends on what accompanied it, and
#'   no provider exposes that as a named option). \code{\link{embedding_info}}
#'   prints the set. With \code{dry_run = TRUE}, an invisible list of counts.
#'
#' @section Why the matrix carries its own texts:
#' Row names are whatever you asked for. If you call
#' \code{embed(setNames(answers, participant_id))} the row names become the
#' participant IDs, which is usually what you want for analysis --- but it means
#' the matrix no longer records \emph{what was embedded}. Archive such a matrix
#' and the texts are gone, so the cache cannot be rebuilt from it and a reader
#' cannot verify what the numbers came from. The \code{texts} attribute keeps
#' that link no matter what the row names say:
#'
#' \preformatted{
#' emb <- embed(setNames(d$answer, d$id))
#' rownames(emb)[1]        # "P01"  -- your label
#' attr(emb, "texts")[1]   # "I felt calm all week." -- what was sent
#' attributes(emb)[c("provider", "model", "access_date")]
#' }
#'
#' \code{\link{save_embeddings}} warns if you archive a matrix without it, and
#' the archive-seeding script distributed with the paper uses it to rebuild a
#' cache from archived matrices alone --- so a reader who has your archive never needs an API key
#' to reproduce the analysis. Subsetting a matrix drops the attribute (this is
#' how R works); embed once and subset afterwards, or re-attach it yourself.
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
#' @seealso \code{\link{check_api}} to confirm a key before a long run;
#'   \code{\link{cos_sim_matrix}} and \code{\link{semantic_projection}} for
#'   what to do with the matrix; \code{\link{embedding_info}} to report what a
#'   matrix was measured with, \code{\link{save_embeddings}} to archive it,
#'   and \code{\link{cache_info}} and \code{\link{cache_path}} for the
#'   cache. What to record when you report a matrix:
#'   \url{https://github.com/PsycholoStudio/qualembed/blob/main/docs/provenance.md}.
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

  fn <- switch(provider,
    gemini = .embed_gemini,
    openai = .embed_openai,
    voyage = .embed_voyage
  )

  # ── 何を送ったかの記録 ────────────────────────────────────
  # 送ったオプションだけでなく、既定のまま送られたものも記録する。
  # 既定はクライアント間で異なり予告なく変わるので、「指定しなかった」
  # ことも仕様の一部であり、後から復元できない。
  .dots <- list(...)
  .opt_names <- intersect(names(formals(fn)), c("dims", "task_type", "input_type"))
  req_opts <- lapply(.opt_names, function(k)
    if (k %in% names(.dots)) .dots[[k]] else eval(formals(fn)[[k]]))
  names(req_opts) <- .opt_names

  # ── キャッシュ: 埋め込みは決定的なので、同一テキストの再送信は無駄 ──
  # provider×model ごとのRDS（テキスト→ベクトルの名前付きリスト）に保存し、
  # 未取得のテキストだけをAPIに送る。cache = FALSE で無効化できる。
  cache_file <- NULL
  cached <- list()
  fetch_dates <- character(0)
  fetch_batch <- character(0)
  if (isTRUE(cache)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    # オプションがベクトルを変える場合はキャッシュを分ける
    # （例: Gemini の taskType、出力次元数、Voyage の input_type）
    # 鍵は req_opts（解決済みの値）から作る。呼び出し側が明示した引数だけを
    # 見ると、既定値を変えたときに鍵が変わらず、旧条件のベクトルが黙って
    # 再利用される。NULL は「そのパラメータを送らない」を意味するので落とす。
    cache_file <- file.path(
      cache_dir, paste0(provider, "_",
                        gsub("[^A-Za-z0-9._-]", "_", model),
                        .cache_sig(req_opts), ".rds"))
    if (file.exists(cache_file)) cached <- readRDS(cache_file)
    # 取得日はキャッシュの属性として持つ（値の構造は変えない）。既存の
    # キャッシュにはこの属性が無く、その分の取得日は不明のまま NA になる。
    # 値に日付を抱かせると全キャッシュが無効になり取り直しに実費がかかる。
    fetch_dates <- attr(cached, "fetch_date", exact = TRUE)
    if (is.null(fetch_dates)) fetch_dates <- character(0)
    # 同じ理由で、そのベクトルがどの一括リクエストで返ってきたかも持つ。
    # プロバイダは一回の呼び出しで配列を受け取り、返るベクトルは配列に
    # 何が同居していたかに依存する（実測: 同一テキスト・同一オプションで
    # コサイン .9998–.9999）。これはオプションではないので request_options
    # には現れず、記録しなければ後から復元できない。
    fetch_batch <- attr(cached, "fetch_batch", exact = TRUE)
    if (is.null(fetch_batch)) fetch_batch <- character(0)
    # 初回だけ、キャッシュがどこに何を書くかを知らせる。参加者のテキストを
    # 扱う利用者が「平文の二つ目の写しが黙って作られた」状態にならないよう、
    # 論文の data-ethics 規則をライブラリ側からも一度は言う。
    if (!isTRUE(getOption("qualembed.cache_notice_shown"))) {
      message("qualembed: caching embeddings to ", normalizePath(cache_dir,
              mustWork = FALSE), "\n",
              "  The cache stores your texts verbatim and unencrypted. ",
              "For participant data,\n",
              "  pass cache = FALSE or set cache_dir to a controlled-access ",
              "location.\n",
              "  (Shown once per session; silence with ",
              "options(qualembed.cache_notice_shown = TRUE).)")
      options(qualembed.cache_notice_shown = TRUE)
    }
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
    # バッチごとにキャッシュへ書き出すコールバック（中断耐性）。
    # 一時ファイルに書いてから改名する。saveRDS を直接当てると、書き込みの
    # 最中に中断された場合にキャッシュが壊れ、以後 readRDS が失敗する。
    # 改名は同一ファイルシステム上で原子的なので、中断しても古い健全な
    # キャッシュが残る。
    today <- as.character(Sys.Date())
    on_chunk <- function(texts_chunk, mat_chunk) {
      for (i in seq_along(texts_chunk))
        cached[[texts_chunk[i]]] <<- mat_chunk[i, ]
      fetch_dates[texts_chunk] <<- today
      # この塊が「どの集合と一緒に送られたか」を、返ってきた各テキストに
      # 対して記録する。同じ塊に入っていたテキストは同じ指紋を持つ。
      fetch_batch[texts_chunk] <<- .request_fingerprint(texts_chunk)
      if (!is.null(cache_file)) {
        to_save <- cached
        attr(to_save, "fetch_date") <- fetch_dates
        attr(to_save, "fetch_batch") <- fetch_batch
        tmp <- paste0(cache_file, ".tmp", Sys.getpid())
        saveRDS(to_save, tmp)
        if (!file.rename(tmp, cache_file)) {
          unlink(tmp)
          warning("Could not replace the embedding cache at ", cache_file,
                  "; this batch was fetched but not cached.", call. = FALSE)
        }
      }
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
  # API に送った本文そのものを常に持たせる。行名は呼び出し側の都合で
  # 参加者IDなどに置き換わることがあり、そうなるとその行列からは
  # 「何を埋め込んだか」が二度と復元できない。属性で保つのは1行で済み、
  # アーカイブからキャッシュを再構成できる状態を恒久的に保証する。
  attr(mat, "texts")       <- texts_clean
  attr(mat, "provider")    <- provider
  attr(mat, "model")       <- model
  attr(mat, "access_date") <- as.character(Sys.Date())

  # ── 出所の記録 ────────────────────────────────────────────
  # ここに載せるものは、この行列を使った分析を報告するときに
  # method 節へ書き写すべき項目そのものにしてある。embedding_info() が
  # それを読み出す。access_date はこの行列を組んだ日であって API を
  # 叩いた日ではない——全件キャッシュから来た場合は誰にも触れていない
  # ので、取得日は別に持つ。
  attr(mat, "dim_embedding")   <- ncol(mat)
  attr(mat, "request_options") <- req_opts
  attr(mat, "n_fetched")       <- length(to_fetch)
  attr(mat, "n_from_cache")    <- length(texts_clean) - length(to_fetch)
  known <- if (length(fetch_dates))
    stats::na.omit(unname(fetch_dates[texts_clean])) else character(0)
  attr(mat, "fetch_dates") <-
    if (length(known)) range(known) else NA_character_
  attr(mat, "n_date_unknown") <- length(texts_clean) - length(known)
  kb <- if (length(fetch_batch))
    stats::na.omit(unname(fetch_batch[texts_clean])) else character(0)
  attr(mat, "request_batches") <- if (length(kb)) table(kb) else NULL
  attr(mat, "n_batch_unknown") <- length(texts_clean) - length(kb)
  attr(mat, "software") <- paste0("qualembed ", .qualembed_version(),
                                  "; ", R.version.string)
  mat
}


# パッケージとして読み込まれていれば版番号、script/ から source した
# 場合はそう分かる文字列を返す。アーカイブに「どの実装で作ったか」を
# 残すためだけの補助。
.qualembed_version <- function() {
  # インストール済みの版番号を無条件に名乗ってはいけない。script/ から
  # source して走らせている場合、走ったのはそのファイルであってパッケージ
  # ではなく、両者は同期しているとは限らない。実際にどちらで動いているかで
  # 分ける。
  ns <- environmentName(environment(.qualembed_version))
  if (identical(ns, "qualembed"))
    as.character(utils::packageVersion("qualembed"))
  else "(sourced from script/, not the installed package)"
}


#' Report the provenance of an embedding matrix
#'
#' Prints what a method section needs in order to identify the measurement:
#' the provider and model, the embedding dimensionality, every request option
#' including the ones left at their defaults, when the vectors were fetched,
#' and the software that fetched them. Returns those fields as a one-row
#' data frame, invisibly, so they can be written to a results file; the
#' per-request batch sizes are printed but not returned.
#'
#' Commercial embedding models are versioned products that are retired on the
#' provider's schedule. A matrix that records only its numbers cannot be
#' matched to the instrument that produced it once that instrument is gone,
#' which is why \code{\link{embed}} attaches these attributes and why archived
#' matrices carry them.
#'
#' @param x A matrix returned by \code{\link{embed}}, or a list of such
#'   matrices (as \code{\link{save_embeddings}} archives them).
#' @return A one-row data frame per matrix, invisibly, with the provider, the
#'   model, the embedding dimensionality, the request options, the access date
#'   and the software that fetched the vectors.
#' @seealso \code{\link{embed}}, which attaches these attributes, and
#'   \code{\link{save_embeddings}}, which archives them. What each field is
#'   for: \url{https://github.com/PsycholoStudio/qualembed/blob/main/docs/provenance.md}.
#' @export
#' @examples
#' \dontrun{
#' emb <- embed(c("first text", "second text"), provider = "openai")
#' embedding_info(emb)
#' embedding_info(readRDS("output/embeddings/demo1_bfi_openai.rds"))
#' }
embedding_info <- function(x) {
  if (is.list(x) && !is.matrix(x)) {
    out <- do.call(rbind, lapply(seq_along(x), function(i) {
      cat(if (i > 1) "\n" else "", "\u2500\u2500 ", names(x)[i] %||% i, "\n", sep = "")
      embedding_info(x[[i]])
    }))
    return(invisible(out))
  }
  g <- function(a, default = NA) {
    v <- attr(x, a, exact = TRUE)
    if (is.null(v)) default else v
  }
  # 属性が「無い」ことと「空である」ことを取り違えない。古い行列は
  # 記録していないだけで、オプションを受け付けないわけではない。
  opts <- attr(x, "request_options", exact = TRUE)
  opt_str <- if (is.null(opts)) "not recorded (matrix predates this field)" else
    if (!length(opts)) "none accepted by this provider" else
    paste(vapply(names(opts), function(k) sprintf(
      "%s = %s", k,
      if (is.null(opts[[k]])) "unset" else paste0("\"", opts[[k]], "\"")),
      character(1)), collapse = ", ")
  fd <- g("fetch_dates")
  fd_str <- if (all(is.na(fd))) "unknown (cached before dates were recorded)" else
    if (length(unique(fd)) == 1) fd[1] else paste(fd, collapse = " to ")
  unknown <- g("n_date_unknown", 0)

  cat(sprintf("Provider        : %s\n", g("provider")))
  cat(sprintf("Model           : %s\n", g("model")))
  cat(sprintf("Dimensions      : %s\n", g("dim_embedding", ncol(x))))
  cat(sprintf("Texts           : %d\n", nrow(x)))
  cat(sprintf("Request options : %s\n", opt_str))
  # 全件不明のときは「不明」だけでよい。件数を足すと同じことを二度言う。
  cat(sprintf("Fetched         : %s%s\n", fd_str,
              if (!is.na(unknown) && unknown > 0 && unknown < nrow(x))
                sprintf(" (%d of %d texts undated)", unknown, nrow(x)) else ""))
  nf <- attr(x, "n_fetched", exact = TRUE)
  cat(sprintf("Assembled       : %s%s\n", g("access_date"),
              if (is.null(nf)) "" else
                sprintf(" (%d fetched, %d from cache)", nf,
                        attr(x, "n_from_cache", exact = TRUE))))
  rb <- attr(x, "request_batches", exact = TRUE)
  ub <- attr(x, "n_batch_unknown", exact = TRUE)
  rb_str <- if (is.null(rb) || !length(rb))
    "not recorded (matrix predates this field)" else
    sprintf("%d request%s (%s)", length(rb), if (length(rb) == 1) "" else "s",
            paste(sprintf("%s x%d", names(rb), as.integer(rb)), collapse = ", "))
  # 返るベクトルは同じリクエストに何が同居したかに依存する。オプションでは
  # ないので上の行には出ない。ここに出しておかないと，報告のしようがない。
  cat(sprintf("Request batches : %s%s\n", rb_str,
              if (!is.null(ub) && ub > 0 && ub < nrow(x))
                sprintf(" (%d of %d texts unrecorded)", ub, nrow(x)) else ""))
  cat(sprintf("Software        : %s\n", g("software", "not recorded")))
  if (is.null(attr(x, "texts", exact = TRUE)))
    cat("Note            : no `texts` attribute -- what was embedded",
        "cannot be recovered from this matrix.\n")

  invisible(data.frame(
    provider = g("provider"), model = g("model"),
    dims = g("dim_embedding", ncol(x)), n_texts = nrow(x),
    request_options = opt_str, fetched = fd_str,
    assembled = g("access_date"), software = g("software"),
    stringsAsFactors = FALSE))
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
                           task_type = "SEMANTIC_SIMILARITY",
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
# モデル: voyage-4   (1024次元・多言語)
# 公式:   https://docs.voyageai.com/docs/embeddings
# キー取得: https://dash.voyageai.com
# 無料枠は 3 req/分 と厳しいが、429 は Retry-After に従い自動リトライする

.embed_voyage <- function(texts,
                           model      = "voyage-4",
                           api_key    = NULL,
                           batch      = 128,
                           input_type = NULL,
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
  if (is.null(model)) model <- "voyage-4"

  chunks <- split(texts, ceiling(seq_along(texts) / batch))
  rep_ <- .chunk_reporter("voyage", length(texts), length(chunks), progress, rpm)
  rows <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    body <- list(input = as.list(unname(chunks[[i]])), model = model)
    # input_type は省略可能。NULL のときにキーごと落とす必要がある——
    # list(input_type = NULL) は JSON では {} になり、指示文なしではなく
    # 不正値として送られてしまう。
    if (!is.null(input_type)) body$input_type <- input_type
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

#' Inspect the embedding cache
#'
#' @description
#' Lists what the on-disk embedding cache currently holds, one row per cache
#' file. A provider has more than one file when it was called with different
#' vector-changing request options, since those are part of the file name. Use it to confirm that a run will be served from
#' the cache rather than from the API, and to see how much disk the cache
#' occupies.
#'
#' @param cache_dir Character; the directory holding the cache files. Defaults
#'   to \code{file.path("output", "embed_cache")}, which is relative to the
#'   working directory, so a session started elsewhere will not find an
#'   existing cache.
#' @return A data frame with one row per cache file and columns
#'   \code{provider}, \code{model} (the model identifier, with any
#'   vector-changing request options appended as they appear in the file name),
#'   \code{n_texts}, \code{dims} (embedding dimensionality), \code{size_mb} and
#'   \code{file}. The directory that was read is printed as a message and
#'   attached as the attribute \code{cache_dir}, so that an empty result can
#'   be told apart from a session looking in the wrong place. When the
#'   directory holds no cache, an empty data frame is returned invisibly with
#'   the same attribute.
#' @seealso \code{\link{cache_clear}} to delete cache files, and
#'   \code{\link{cache_path}} for the name of a single cache file.
#' @export
cache_info <- function(cache_dir = file.path("output", "embed_cache")) {
  # 既定は作業ディレクトリからの相対パスなので、どこを見たのかを必ず示す。
  # そうしないと、セッションを別の場所で始めた利用者は、空の結果を
  # 「キャッシュが無い」と読んでしまう（実際には見る場所が違う）。
  where <- normalizePath(cache_dir, mustWork = FALSE)
  files <- list.files(cache_dir, pattern = "[.]rds$", full.names = TRUE)
  if (length(files) == 0) {
    message("No embedding cache in ", where)
    out <- data.frame()
    attr(out, "cache_dir") <- where
    return(invisible(out))
  }
  out <- do.call(rbind, lapply(files, function(f) {
    x <- readRDS(f)
    key <- sub("[.]rds$", "", basename(f))
    data.frame(provider = sub("_.*$", "", key),
               model    = sub("^[^_]+_", "", key),
               n_texts  = length(x),
               dims     = if (length(x)) length(x[[1]]) else NA_integer_,
               size_mb  = round(file.size(f) / 1024^2, 1),
               file     = basename(f),
               row.names = NULL)
  }))
  attr(out, "cache_dir") <- where
  message(nrow(out), " cache file", if (nrow(out) != 1) "s" else "",
          " in ", where)
  out
}

#' Delete cached embeddings
#'
#' @description
#' Removes files from the embedding cache directory. Texts already in the
#' cache are never re-fetched, so clear it when a provider has changed the
#' model behind a fixed name, or when the cache is to be rebuilt from
#' scratch. Clearing costs API calls on the next run.
#'
#' @param provider Character; the provider whose cache files are removed, for
#'   example \code{"gemini"}. Defaults to \code{NULL}, which removes every
#'   cache file in the directory.
#' @param cache_dir Character; the directory holding the cache files. Defaults
#'   to \code{file.path("output", "embed_cache")}, which is relative to the
#'   working directory, so a session started elsewhere will not find an
#'   existing cache.
#' @return Invisibly, the number of files removed. A message reports the
#'   count, or states that there was nothing to clear.
#' @seealso \code{\link{cache_info}} to inspect the cache before clearing it,
#'   and \code{\link{cache_path}} for the name of a single cache file.
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

#' Check that an embedding API is reachable
#'
#' Embeds a single test sentence and prints the provider, the model, the
#' embedding dimensionality and the first five components, so that a key and an
#' endpoint can be confirmed before a long run is started.
#'
#' @param provider Character; one of \code{"gemini"}, \code{"voyage"},
#'   \code{"openai"}. Defaults to \code{"gemini"}.
#' @param ... Passed to \code{\link{embed}}.
#' @return The one-row embedding matrix returned by \code{\link{embed}},
#'   invisibly, carrying the usual \code{provider}, \code{model} and
#'   \code{access_date} attributes.
#' @seealso \code{\link{embed}} for the call this wraps, and
#'   \code{\link{embedding_info}} for the full provenance of a matrix.
#' @export
check_api <- function(provider = "gemini", ...) {
  cat("API connection test:", provider, "\n")
  test <- embed("This is a test.", provider = provider, ...)
  cat("  model:", attr(test, "model"), "\n")
  cat("  dimensions:", ncol(test), "\n")
  cat("  first 5 components:", round(test[1, 1:5], 4), "\n")
  cat("  OK\n")
  invisible(test)
}
# check_api("gemini")  # GEMINI_API_KEY 要
# check_api("voyage")  # VOYAGE_API_KEY 要
# check_api("openai")  # OPENAI_API_KEY 要
