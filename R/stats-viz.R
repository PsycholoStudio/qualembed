# ============================================================
# utils.R
# 分析・可視化ユーティリティ
#
# 使い方: source("utils.R")
# 依存:   ggplot2, ggrepel, dplyr, vegan
#         （Rtsne / uwot はオプション: tsne_2d / umap_2d 使用時のみ）
#
# 構成:
#   1. 類似度・距離
#   2. 統計的推論（permutation / ARI / Mantel / Procrustes+PROTEST）
#   3. Semantic projection
#   4. 次元削減
#   5. 可視化
#   6. 結果の記録・保存
#
# Method セクションとの対応:
#   - すべての確証的検定は permutation 検定（既定 9,999 回・片側）
#     p = (1 + #{null >= observed}) / (1 + n_perm)
#   - Δ = within − between（test_delta）
#   - Ward法クラスタ → ARI（test_ari; Hubert & Arabie, 1985）
#   - 行列対応 → Mantel r_M（mantel_test; vegan::mantel）
#   - 空間比較 → Procrustes m² + PROTEST（procrustes_m2; vegan）
#   - 理論駆動の軸 → semantic projection（Grand et al., 2022）
# ============================================================

# 類似度行列と groups の対応を検証する共通チェック
.check_sim_groups <- function(sim_mat, groups, fn) {
  if (!is.matrix(sim_mat) || nrow(sim_mat) != ncol(sim_mat))
    stop(fn, "(): expected a square similarity matrix ",
         "(e.g., the output of cos_sim_matrix()).", call. = FALSE)
  if (length(groups) != nrow(sim_mat))
    stop(fn, "(): `groups` has length ", length(groups),
         " but the similarity matrix has ", nrow(sim_mat),
         " rows -- supply exactly one group label per row.", call. = FALSE)
  if (anyNA(groups))
    stop(fn, "(): `groups` contains NA; every row needs a label.",
         call. = FALSE)
  if (length(unique(groups)) < 2)
    stop(fn, "(): `groups` must contain at least two distinct groups.",
         call. = FALSE)
  if (!any(table(groups) >= 2))
    stop(fn, "(): every group has only one member, so no within-group ",
         "pairs exist and the statistic is undefined.", call. = FALSE)
  invisible(TRUE)
}

# ── 1. 類似度・距離 ─────────────────────────────────────────

#' コサイン類似度行列
#' @param mat embedding行列 (n × d)
#' @return n × n のコサイン類似度行列
#' @export
cos_sim_matrix <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat))
    stop("cos_sim_matrix(): expected a numeric matrix with one row per ",
         "text (the output of embed()); got ", class(mat)[1], ".",
         call. = FALSE)
  if (nrow(mat) < 2)
    stop("cos_sim_matrix(): need at least two texts to compare.",
         call. = FALSE)
  norms <- sqrt(rowSums(mat^2))
  zero  <- which(norms == 0 | !is.finite(norms))
  if (length(zero) > 0)
    stop("cos_sim_matrix(): row(s) ",
         paste(head(zero, 5), collapse = ", "),
         " have zero or non-finite length -- cosine similarity is ",
         "undefined for them. Check for empty or corrupted embeddings.",
         call. = FALSE)
  (mat %*% t(mat)) / (norms %o% norms)
}

#' ペアワイズユークリッド距離行列
#' @param mat embedding行列 (n × d)
#' @return dist オブジェクト
#' @export
euclidean_dist <- function(mat) {
  dist(mat, method = "euclidean")
}

#' グループ内・グループ間の平均コサイン類似度と差 Δ
#' @param sim_mat cos_sim_matrix() の出力
#' @param groups  各行に対応するグループラベル (character vector)
#' @return list(within, between, delta = within - between, ratio)
#' @export
within_between_sim <- function(sim_mat, groups) {
  ut   <- upper.tri(sim_mat)
  same <- outer(groups, groups, `==`)
  wi   <- mean(sim_mat[ut & same])
  bt   <- mean(sim_mat[ut & !same])
  list(within = wi, between = bt, delta = wi - bt, ratio = wi / bt)
}


# ── 2. 統計的推論 ───────────────────────────────────────────

# permutation p 値（片側・+1補正）
.perm_p <- function(observed, null) {
  (1 + sum(null >= observed)) / (1 + length(null))
}

#' Δ = within − between の permutation 検定
#' 帰無分布はグループラベルを項目上でシャッフルして生成する
#' @param sim_mat cos_sim_matrix() の出力
#' @param groups  グループラベル
#' @param n_perm  並べ替え回数
#' @return list(within, between, delta, p, n_perm)
#' @export
test_delta <- function(sim_mat, groups, n_perm = 9999) {
  .check_sim_groups(sim_mat, groups, "test_delta")
  ut  <- upper.tri(sim_mat)
  obs <- within_between_sim(sim_mat, groups)
  null <- replicate(n_perm, {
    g    <- sample(groups)
    same <- outer(g, g, `==`)
    mean(sim_mat[ut & same]) - mean(sim_mat[ut & !same])
  })
  list(within = obs$within, between = obs$between, delta = obs$delta,
       p = .perm_p(obs$delta, null), n_perm = n_perm)
}

#' Adjusted Rand Index（Hubert & Arabie, 1985）
#' 外部パッケージへの依存を避けるため直接実装
#' @param x, y 2つの分割（同じ長さのラベルベクトル）
#' @return ARI（チャンスレベル = 0、完全一致 = 1）
#' @export
adjusted_rand_index <- function(x, y) {
  tab <- table(x, y)
  n   <- sum(tab)
  a   <- sum(choose(tab, 2))
  b   <- sum(choose(rowSums(tab), 2))
  cc  <- sum(choose(colSums(tab), 2))
  expected <- b * cc / choose(n, 2)
  maximum  <- (b + cc) / 2
  if (isTRUE(all.equal(maximum, expected))) return(0)
  (a - expected) / (maximum - expected)
}

#' 類似度行列に対する Ward法クラスタリング → ARI + permutation 検定
#' 距離 (1 - 類似度) に ward.D2 を適用し、k 個に分割する。
#' 埋め込みのコサイン類似度だけでなく、実回答の|相関|行列など
#' 任意の類似度行列に同一パイプラインを適用できる（対称比較用）。
#' @param sim_mat 類似度行列（対角1・対称）
#' @param groups  理論的グループラベル
#' @param k       クラスタ数（既定 = グループ数）
#' @param n_perm  並べ替え回数
#' @return list(ari, p, clusters, table, hclust, n_perm)
#' @export
test_ari_sim <- function(sim_mat, groups, k = length(unique(groups)),
                         n_perm = 9999) {
  .check_sim_groups(sim_mat, groups, "test_ari_sim")
  if (k < 2 || k > nrow(sim_mat) - 1)
    stop("test_ari_sim(): `k` must lie between 2 and n - 1 (here ",
         nrow(sim_mat) - 1, "); got k = ", k, ".", call. = FALSE)
  d    <- as.dist(1 - sim_mat)
  hc   <- hclust(d, method = "ward.D2")
  cl   <- cutree(hc, k = k)
  obs  <- adjusted_rand_index(cl, groups)
  null <- replicate(n_perm, adjusted_rand_index(cl, sample(groups)))
  list(ari = obs, p = .perm_p(obs, null),
       clusters = cl, table = table(cluster = cl, group = groups),
       hclust = hc, n_perm = n_perm)
}

#' Ward法階層クラスタリング → 理論分類との ARI + permutation 検定
#' embedding 行列からコサイン類似度を計算して test_ari_sim() に委譲する
#' @param mat    embedding行列
#' @param groups 理論的グループラベル
#' @param k      クラスタ数（既定 = グループ数）
#' @param n_perm 並べ替え回数
#' @return list(ari, p, clusters, table, hclust, n_perm)
#' @export
test_ari <- function(mat, groups, k = length(unique(groups)), n_perm = 9999) {
  test_ari_sim(cos_sim_matrix(mat), groups, k = k, n_perm = n_perm)
}

#' Mantel 検定（vegan::mantel のラッパー; Mantel, 1967）
#' @param m1, m2  対応する n×n の距離（または類似度）行列。
#'                行・列の順序が2つの行列で一致していること。
#' @param n_perm  並べ替え回数
#' @return list(r, p, n_perm)
#' @export
mantel_test <- function(m1, m2, n_perm = 9999) {
  if (!is.matrix(m1) || !is.matrix(m2) ||
      nrow(m1) != ncol(m1) || nrow(m2) != ncol(m2))
    stop("mantel_test(): both arguments must be square matrices ",
         "(similarity or distance) over the same items.", call. = FALSE)
  if (!all(dim(m1) == dim(m2)))
    stop("mantel_test(): the two matrices differ in size (",
         nrow(m1), " vs. ", nrow(m2), " items) -- they must describe ",
         "the same items in the same order.", call. = FALSE)
  if (!is.null(rownames(m1)) && !is.null(rownames(m2)) &&
      !identical(rownames(m1), rownames(m2)))
    stop("mantel_test(): the two matrices have different row names -- ",
         "reorder one so the items match (e.g., m2[rownames(m1), ",
         "rownames(m1)]).", call. = FALSE)
  res <- vegan::mantel(as.dist(m1), as.dist(m2),
                       method = "pearson", permutations = n_perm)
  list(r = unname(res$statistic), p = res$signif, n_perm = n_perm)
}

#' Procrustes 一致度 m² + PROTEST（vegan; Gower, 1975; Jackson, 1995）
#' 次元数の異なる空間を比較するため、各空間の最初の k 主成分に
#' 射影してから対称 Procrustes を実行する（Method: 既定 k = 5）
#' @param X, Y    同じ n 項目の embedding 行列（行の対応が取れていること）
#' @param k       比較に使う主成分数
#' @param n_perm  PROTEST の並べ替え回数（0 で PROTEST をスキップ）
#' @return list(m2, corr = sqrt(1-m2), p, residuals（項目別・降順）, k, n_perm)
#' @export
procrustes_m2 <- function(X, Y, k = 5, n_perm = 9999) {
  stopifnot(nrow(X) == nrow(Y))
  if (!is.null(rownames(X)) && !is.null(rownames(Y)))
    stopifnot(identical(rownames(X), rownames(Y)))
  k  <- min(k, ncol(X), ncol(Y), nrow(X) - 1)
  Xk <- prcomp(X)$x[, seq_len(k), drop = FALSE]
  Yk <- prcomp(Y)$x[, seq_len(k), drop = FALSE]
  pro <- vegan::procrustes(Xk, Yk, symmetric = TRUE)
  p <- NA_real_
  if (n_perm > 0) {
    pt <- vegan::protest(Xk, Yk, permutations = n_perm)
    p  <- pt$signif
  }
  resid <- stats::residuals(pro)
  names(resid) <- rownames(X)
  list(m2 = pro$ss, corr = sqrt(1 - pro$ss), p = p,
       residuals = sort(resid, decreasing = TRUE), k = k, n_perm = n_perm)
}

#' Procrustes m² の k に対する感度分析（Method: k ∈ {2,...,10}）
#' @param X, Y  embedding 行列
#' @param ks    試す主成分数の範囲
#' @return data.frame(k, m2)
#' @export
procrustes_sensitivity <- function(X, Y, ks = 2:10) {
  ks <- ks[ks <= min(ncol(X), ncol(Y), nrow(X) - 1)]
  data.frame(
    k  = ks,
    m2 = vapply(ks, function(k) procrustes_m2(X, Y, k = k, n_perm = 0)$m2,
                numeric(1))
  )
}


# ── 3. Semantic projection（Grand et al., 2022）─────────────

#' アンカー重心の差ベクトルが定義する軸に項目を射影する
#' a = mean(high) - mean(low),  p_i = x_i・a / ||a||
#' @param item_mat 項目の embedding 行列 (n × d)
#' @param high_mat 高極アンカー句の embedding 行列
#' @param low_mat  低極アンカー句の embedding 行列
#' @return 項目ごとの射影スコア（名前付き数値ベクトル）
#' @export
semantic_projection <- function(item_mat, high_mat, low_mat) {
  if (!is.matrix(high_mat) || !is.matrix(low_mat) ||
      nrow(high_mat) == 0 || nrow(low_mat) == 0)
    stop("semantic_projection(): `high_mat` and `low_mat` must be ",
         "embedding matrices with at least one anchor row each ",
         "(subset the anchor embeddings, e.g., emb[anchors$high, , ",
         "drop = FALSE]).", call. = FALSE)
  if (ncol(item_mat) != ncol(high_mat) || ncol(item_mat) != ncol(low_mat))
    stop("semantic_projection(): items and anchors have different ",
         "embedding dimensionalities -- all must come from the same ",
         "provider and model.", call. = FALSE)
  a <- colMeans(high_mat) - colMeans(low_mat)
  if (sum(a^2) == 0)
    stop("semantic_projection(): the high- and low-anchor centroids ",
         "coincide, so the axis is undefined. Choose anchor phrases ",
         "that differ in meaning.", call. = FALSE)
  p <- as.numeric(item_mat %*% a) / sqrt(sum(a^2))
  names(p) <- rownames(item_mat)
  p
}


# ── 4. 次元削減 ─────────────────────────────────────────────

#' PCAで2次元に圧縮（Method の Z = X̃V₂ に対応: 中心化のみ・標準化なし）
#' @param mat    embedding行列 (n × d)
#' @param scale  TRUE で各次元を標準化（既定 FALSE = Method と一致）
#' @return list(df = 座標データフレーム, ve = 分散説明率[2], prcomp = prcomp結果)
#' @export
pca_2d <- function(mat, scale = FALSE) {
  pc <- prcomp(mat, scale. = scale)
  df <- as.data.frame(pc$x[, 1:2])
  colnames(df) <- c("PC1", "PC2")
  df$label <- rownames(mat)
  ve <- round(summary(pc)$importance[2, 1:2] * 100, 1)
  list(df = df, ve = ve, prcomp = pc)
}

#' 旧名（後方互換）。既定の標準化も pca_2d に合わせて FALSE に変更した。
#' @export
pca_coords <- pca_2d

#' t-SNE で2次元に圧縮（PCAマップの頑健性チェック用・Rtsne が必要）
#' 戻り値の列名は plot_embedding_2d() と互換のため PC1/PC2 とする
#' @export
tsne_2d <- function(mat, perplexity = NULL, seed = 2026) {
  if (!requireNamespace("Rtsne", quietly = TRUE))
    stop("tsne_2d には Rtsne が必要です: install.packages('Rtsne')")
  set.seed(seed)
  if (is.null(perplexity)) perplexity <- max(2, floor((nrow(mat) - 1) / 3))
  fit <- Rtsne::Rtsne(mat, dims = 2, perplexity = perplexity,
                      pca = FALSE, check_duplicates = FALSE)
  df <- as.data.frame(fit$Y)
  colnames(df) <- c("PC1", "PC2")
  df$label <- rownames(mat)
  list(df = df, ve = c(NA_real_, NA_real_))
}

#' UMAP で2次元に圧縮（PCAマップの頑健性チェック用・uwot が必要）
#' @export
umap_2d <- function(mat, n_neighbors = NULL, seed = 2026) {
  if (!requireNamespace("uwot", quietly = TRUE))
    stop("umap_2d には uwot が必要です: install.packages('uwot')")
  set.seed(seed)
  if (is.null(n_neighbors)) n_neighbors <- max(2, min(15, nrow(mat) - 1))
  fit <- uwot::umap(mat, n_components = 2, n_neighbors = n_neighbors)
  df <- as.data.frame(fit)
  colnames(df) <- c("PC1", "PC2")
  df$label <- rownames(mat)
  list(df = df, ve = c(NA_real_, NA_real_))
}


# ── 5. 可視化 ───────────────────────────────────────────────

#' 意味空間の2Dプロット（pca_2d / tsne_2d / umap_2d の出力を受け取る）
#' @param proj   pca_2d() などの戻り値（list）、または PC1/PC2/label 列を
#'               含む data.frame
#' @param labels 点ラベル（NULL なら proj 内の label 列を使用）
#' @param groups 色分け用グループラベル（NULL で単色）
#' @param title  図のタイトル
#' @param size   点のサイズ
#' @export
plot_embedding_2d <- function(proj, labels = NULL, groups = NULL,
                              title = NULL, size = 3.5) {
  df <- if (is.data.frame(proj)) proj else proj$df
  ve <- if (is.data.frame(proj)) c(NA_real_, NA_real_) else proj$ve
  if (!is.null(labels)) df$label <- labels
  if (!is.null(groups)) df$group <- groups
  subtitle <- if (!anyNA(ve))
    sprintf("PC1: %.1f%%  PC2: %.1f%%", ve[1], ve[2]) else NULL

  p <- ggplot(df, aes(PC1, PC2, label = label)) +
    {if (!is.null(groups))
       geom_point(aes(color = group), size = size, alpha = .85)
     else
       geom_point(size = size, alpha = .85, color = "#1D4ED8")} +
    geom_text_repel(
      size          = 3.2,
      box.padding   = 0.45,
      point.padding = 0.3,
      segment.size  = 0.3,
      segment.color = "grey65",
      max.overlaps  = 25
    ) +
    labs(title = title, subtitle = subtitle,
         x = "PC1", y = "PC2", color = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15),
      plot.subtitle    = element_text(color = "grey45", size = 11),
      panel.grid.minor = element_blank(),
      legend.position  = "right"
    )
  print(p)
  invisible(p)
}

#' 日英の意味空間を横並びで比較
#' 各言語で独立にPCAした座標なので直接比較はできない
#' （定量的な比較は procrustes_m2() で行う）
#' @param df_en, df_ja  pca_2d()$df（色分け列を追加したもの）
#' @param ve_en, ve_ja  分散説明率
#' @param title         図のタイトル
#' @param color_var     色分け列名
#' @export
plot_bilingual <- function(df_en, df_ja, ve_en, ve_ja,
                            title, color_var = NULL) {
  df_en$lang <- "English"
  df_ja$lang <- "Japanese"
  df_all <- bind_rows(df_en, df_ja)
  df_all$lang <- factor(df_all$lang, levels = c("English", "Japanese"))

  p <- ggplot(df_all, aes(PC1, PC2, label = label)) +
    {if (!is.null(color_var))
       geom_point(aes(color = .data[[color_var]]),
                  size = 3.5, alpha = .85)
     else
       geom_point(size = 3.5, alpha = .85, color = "#1D4ED8")} +
    geom_text_repel(size = 3.0, box.padding = .4,
                    segment.size = .3, segment.color = "grey65",
                    max.overlaps = 20) +
    facet_wrap(~ lang, scales = "free") +
    labs(title = title, color = NULL,
         x = "PC1 (independent per language)", y = "PC2") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(size = 13, face = "bold")
    )
  print(p)
  invisible(p)
}

#' 類似度行列のヒートマップ
#' @param sim_mat     類似度行列（行名・列名が必要）
#' @param order       表示順（既定は行名の順のまま）
#' @param title       図のタイトル
#' @param legend_name 凡例のタイトル
#' @export
plot_similarity_heatmap <- function(sim_mat, order = rownames(sim_mat),
                                    title = "Cosine similarity",
                                    legend_name = "Cosine\nsimilarity") {
  df <- expand.grid(x = order, y = order, stringsAsFactors = FALSE)
  df$sim <- mapply(function(a, b) sim_mat[a, b], df$x, df$y)
  df$x <- factor(df$x, levels = order)
  df$y <- factor(df$y, levels = rev(order))

  p <- ggplot(df, aes(x, y, fill = sim)) +
    geom_tile(color = "white", linewidth = .4) +
    scale_fill_gradient2(low = "#3B82F6", mid = "white", high = "#EF4444",
                         midpoint = mean(sim_mat[lower.tri(sim_mat)]),
                         name = legend_name) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title  = element_text(face = "bold"))
  print(p)
  invisible(p)
}


# ── 6. 結果の記録・保存 ─────────────────────────────────────

#' 図を指定フォルダに保存（figuresフォルダを自動作成）
#' @param plot   ggplotオブジェクト
#' @param name   ファイル名（拡張子なし）
#' @param dir    保存先ディレクトリ
#' @param w, h   幅・高さ（インチ）
#' @param dpi    解像度
#' @export
save_fig <- function(plot, name, dir = "figures",
                     w = 11, h = 7.5, dpi = 180) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, paste0(name, ".png"))
  ggsave(path, plot, width = w, height = h, dpi = dpi, bg = "white")
  message("保存: ", path)
  invisible(path)
}

#' Archive embedding matrices for reproduction
#'
#' Reproducibility rests on these matrices rather than on re-fetching from the
#' API, so the archive has to record what was embedded. It warns in the two
#' cases where it would not: a matrix that has lost the \code{texts} attribute
#' \code{embed()} attaches (subsetting drops it), and two matrices sharing a
#' name (name-based lookup would return only the first, leaving the other
#' unreachable). Both failures are silent otherwise, and both make the archive
#' impossible to trace back to its inputs.
#'
#' @param emb_list Named list of matrices from \code{embed()} (a bare matrix is
#'   accepted and named after \code{name}).
#' @param name     Analysis name; becomes part of the file name.
#' @param provider Provider name; becomes part of the file name.
#' @param dir      Output directory.
#' @return The path written, invisibly.
#' @seealso \code{embed()} for the \code{texts} attribute this relies on.
#' @export
save_embeddings <- function(emb_list, name, provider,
                            dir = file.path("output", "embeddings")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  if (is.matrix(emb_list)) emb_list <- stats::setNames(list(emb_list), name)

  # アーカイブは「本文が復元できる」状態でなければ意味がない。行名は
  # 呼び出し側の都合で参加者IDになりうるので、embed() が付ける texts 属性が
  # 唯一の確実な手がかりになる。無いまま保存すると、その行列からは
  # 何を埋め込んだのか二度と分からず、キャッシュも作り直せない。
  no_texts <- vapply(emb_list, function(m)
    is.matrix(m) && (is.null(attr(m, "texts")) ||
                     length(attr(m, "texts")) != nrow(m)), TRUE)
  if (any(no_texts))
    warning("save_embeddings(): ", sum(no_texts), " of ", length(emb_list),
            " matrices (", paste(names(emb_list)[no_texts], collapse = ", "),
            ") have no `texts` attribute, so the archive will not record what ",
            "was embedded and a cache cannot be rebuilt from it. Matrices from ",
            "embed() carry it automatically; if you subset or rebuild one, ",
            "re-attach it with attr(m, \"texts\") <- the_texts.", call. = FALSE)
  if (!is.null(names(emb_list)) && anyDuplicated(names(emb_list)))
    warning("save_embeddings(): duplicated names in the archive (",
            paste(unique(names(emb_list)[duplicated(names(emb_list))]),
                  collapse = ", "), "). Name-based lookup returns only the ",
            "first, so the others are unreachable.", call. = FALSE)

  path <- file.path(dir, sprintf("%s_%s.rds", name, provider))
  saveRDS(emb_list, path)
  message("埋め込みを保存: ", path)
  invisible(path)
}

#' 統計値を CSV に記録（原稿の数値との照合・再現性検証用）
#' @param stats    名前付きリスト（数値または文字列）
#' @param name     デモ名（ファイル名の一部になる）
#' @param provider プロバイダ名
#' @param dir      出力先ディレクトリ
#' @export
write_stats <- function(stats, name, provider, dir = "output") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  df <- data.frame(
    demo      = name,
    provider  = provider,
    statistic = names(stats),
    value     = vapply(stats, function(v) as.character(v)[1], character(1)),
    date      = as.character(Sys.Date()),
    row.names = NULL
  )
  path <- file.path(dir, sprintf("%s_%s.csv", name, provider))
  write.csv(df, path, row.names = FALSE)
  message("統計値を保存: ", path)
  invisible(df)
}


# ══════════════════════════════════════════════════════════════
# 7. 長文の分割と軌跡
#
# 設計方針: 分割は分析者の仕事であって、この道具の仕事ではない。
# segment_text() は便宜であり、義務ではない。既存の質的分析ソフトから
# 書き出したコード済みセグメント、逐語録の話者交替、自作の規則——
# どれで切っても、下の「契約」を満たす表にすれば同じ関数群が受け取る。
#
# 契約: 1セグメント1行の長形式データフレーム
#   doc_id  文書ID（参加者・インタビュー）。必須。
#   segid   文書内の順序（1から）。省略時は行順から導出。
#   text    セグメント本文。必須。
#   n_words / n_char / docname は自動計算。
#   その他の列（speaker, code, time …）はそのまま保持される。
#
# 列名は生態系の慣行に合わせた: doc_id は readtext の返り値列であり
# quanteda::corpus.data.frame() の既定 docid_field でもある。
# segid は quanteda::corpus_segment() 系の呼称。
# 旧称 (doc, index) は入力別名として恒久的に受け付ける。
# ══════════════════════════════════════════════════════════════

# 入力列名の別名表（小文字化・trim 後に照合）。推測はしない——
# 0件でも2件以上でも中断する。黙って間違った列を使うのが最悪の失敗。
.seg_aliases <- list(
  doc_id = c("doc_id", "docid", "doc", "document", "file", "participant",
             "parent", "id_doc", "interview", "case"),
  segid  = c("segid", "index", "segment", "seg_index", "seg_id", "order",
             "position", "turn"),
  text   = c("text", "content", "coded", "quotation content", "segment_text",
             "text_content", "utterance", "body")
)

.pick_col <- function(nms, kind, explicit = NULL) {
  if (!is.null(explicit)) {
    if (!explicit %in% nms)
      stop("as_segments(): column `", explicit, "` is not in the data ",
           "(columns are: ", paste(nms, collapse = ", "), ").", call. = FALSE)
    return(explicit)
  }
  hit <- nms[tolower(trimws(nms)) %in% .seg_aliases[[kind]]]
  if (length(hit) > 1)
    stop("as_segments(): more than one column could be the ", kind,
         " column (", paste(hit, collapse = ", "), "). Name it explicitly, ",
         "e.g. as_segments(x, ", kind, " = \"", hit[1], "\").", call. = FALSE)
  if (length(hit) == 0) return(NA_character_)
  hit
}

#' 分割済みテキストを軌跡関数が受け取れる形に整える
#'
#' @details
#' 分割の規則は分析者が決めるものである。この関数は「どう切ったか」には
#' 一切関与せず、「切った結果」を共通の形に揃えるだけの入口である。
#' segment_text() を使わずに自分で分割してよいし、そのほうが普通である。
#'
#' 現実的な出発点は三つある。
#' \enumerate{
#'   \item **質的分析ソフトの書き出し**（CSV/Excel）。Taguette は
#'     `id, document, tag, content`、QualCoder は `File, Coder, Coded, ...`
#'     の形で1行1セグメントを吐く。列名は自動照合されるので、
#'     `read.csv()` して渡すだけでよい。
#'   \item **逐語録ファイル**（1参加者1ファイル）。話者交替を空行区切りの
#'     段落とし、話者名を `名前:` の接頭辞で書く。read_segments() が
#'     .txt / .docx / .vtt / .srt を読む。
#'   \item **自作の分割**。data.frame(doc_id, text) を行順に並べるだけで
#'     よい。segid は行順から導出される。
#' }
#'
#' 注意: 「1行1セグメントのテキストファイル」は勧めない。その形を実際に
#' 吐くのは Whisper の txt 出力だが、あの行は2〜5秒の音声区間であって
#' 話者交替でも文でも意味単位でもない。正しく構造化されているように
#' 見えて、そうでない——設計上もっとも避けたい失敗である。
#'
#' @param x       data.frame / 名前付き文字ベクトル / 名前付きリスト /
#'                quanteda corpus
#' @param doc_id,segid,text 列名を明示する場合に指定（既定は自動照合）
#' @param renumber TRUE なら segid を文書内で振り直す
#' @param drop_empty 空文字・NA の行を落とす（既定 TRUE）
#' @param unit    分割単位の記録（"sentence" など）。図表の注に使う。
#' @param quiet   導出時のメッセージを抑制
#' @return class c("qe_segments", "data.frame")。列は
#'   doc_id, segid, text, n_words, n_char, docname と、元の追加列。
#' @export
as_segments <- function(x, ...) UseMethod("as_segments")

#' @export
as_segments.default <- function(x, ...) {
  stop("as_segments(): don't know how to build segments from an object of ",
       "class ", paste(class(x), collapse = "/"), ". Supply a data frame ",
       "with one row per segment (columns doc_id and text), a named ",
       "character vector, or a named list of character vectors.",
       call. = FALSE)
}

#' @export
as_segments.qe_segments <- function(x, ..., quiet = TRUE) {
  as_segments.data.frame(as.data.frame(x), ..., quiet = quiet)
}

#' @export
as_segments.data.frame <- function(x, doc_id = NULL, segid = NULL, text = NULL,
                                   ..., renumber = FALSE, drop_empty = TRUE,
                                   unit = NULL, quiet = FALSE) {
  nms <- names(x)
  ctext <- .pick_col(nms, "text", text)
  if (is.na(ctext))
    stop("as_segments(): no text column found. Expected one of: ",
         paste(.seg_aliases$text, collapse = ", "),
         ". Name it explicitly, e.g. as_segments(x, text = \"response\").",
         call. = FALSE)
  if (is.factor(x[[ctext]])) x[[ctext]] <- as.character(x[[ctext]])
  if (!is.character(x[[ctext]]))
    stop("as_segments(): column `", ctext, "` is ", class(x[[ctext]])[1],
         ", not character. Segment text must be character.", call. = FALSE)

  cdoc <- .pick_col(nms, "doc_id", doc_id)
  if (is.na(cdoc)) {
    if (!quiet) message("as_segments(): no document column found; ",
                        "treating all rows as one document (doc_id = \"doc1\").")
    x$.doc_id <- "doc1"; cdoc <- ".doc_id"
  }
  cseg <- .pick_col(nms, "segid", segid)

  out <- data.frame(doc_id = as.character(x[[cdoc]]),
                    text   = x[[ctext]], stringsAsFactors = FALSE)
  if (!is.na(cseg) && !renumber) {
    out$segid <- as.integer(x[[cseg]])
    if (anyNA(out$segid))
      stop("as_segments(): column `", cseg, "` could not be read as whole ",
           "numbers. Pass renumber = TRUE to derive the order from row ",
           "order instead.", call. = FALSE)
  } else {
    if (!quiet && is.na(cseg))
      message("as_segments(): no segment-order column found; using row order ",
              "within each document (this is the intended path for exports ",
              "that repeat the document name once per segment).")
    out$segid <- stats::ave(seq_len(nrow(out)), out$doc_id,
                            FUN = function(i) seq_along(i))
  }

  keep <- setdiff(names(x), c(cdoc, cseg, ctext, ".doc_id"))
  if (length(keep)) out <- cbind(out, x[, keep, drop = FALSE])

  if (isTRUE(drop_empty)) {
    bad <- is.na(out$text) | trimws(out$text) == ""
    if (any(bad)) {
      if (!quiet) message("as_segments(): dropped ", sum(bad),
                          " empty segment(s).")
      out <- out[!bad, , drop = FALSE]
    }
  }
  if (!nrow(out)) stop("as_segments(): no non-empty segments left.",
                       call. = FALSE)

  # doc_id は初出順（アルファベット順ではない）で並べ、文書内は segid 順
  out$doc_id <- factor(out$doc_id, levels = unique(out$doc_id))
  out <- out[order(out$doc_id, out$segid), , drop = FALSE]
  out$doc_id <- as.character(out$doc_id)

  out$n_words <- .n_words(out$text)
  out$n_char  <- nchar(out$text)
  out$docname <- paste0(out$doc_id, ".", out$segid)
  if (anyDuplicated(out$docname))
    stop("as_segments(): document/segment pairs are not unique (e.g. ",
         out$docname[anyDuplicated(out$docname)],
         " appears twice). Pass renumber = TRUE to renumber within document.",
         call. = FALSE)

  front <- c("doc_id", "segid", "text", "n_words", "n_char", "docname")
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  rownames(out) <- NULL
  attr(out, "unit") <- unit %||% attr(x, "unit")
  class(out) <- c("qe_segments", "data.frame")
  out
}

#' @export
as_segments.character <- function(x, doc_id = NULL, ..., unit = NULL,
                                  quiet = FALSE) {
  ids <- doc_id %||% names(x) %||% "doc1"
  if (length(ids) == 1) ids <- rep(ids, length(x))
  # ベクトルの並びがそのまま順序なので、segid は導出済みとして渡す
  d <- data.frame(doc_id = ids, text = unname(x), stringsAsFactors = FALSE)
  d$segid <- stats::ave(seq_len(nrow(d)), d$doc_id, FUN = seq_along)
  as_segments.data.frame(d, ..., unit = unit, quiet = quiet)
}

#' @export
as_segments.list <- function(x, ..., unit = NULL, quiet = FALSE) {
  if (is.null(names(x)))
    stop("as_segments(): a list input must be named -- the names become ",
         "doc_id. Use setNames(your_list, participant_ids).", call. = FALSE)
  if (!all(vapply(x, is.character, TRUE)))
    stop("as_segments(): every element of the list must be a character ",
         "vector of that document's segments, in order.", call. = FALSE)
  d <- data.frame(doc_id = rep(names(x), lengths(x)),
                  segid  = unlist(lapply(lengths(x), seq_len), use.names = FALSE),
                  text   = unlist(x, use.names = FALSE),
                  stringsAsFactors = FALSE)
  as_segments.data.frame(d, ..., unit = unit, quiet = quiet)
}

#' @export
as_segments.corpus <- function(x, ..., unit = NULL, quiet = FALSE) {
  if (!requireNamespace("quanteda", quietly = TRUE))
    stop("as_segments(): the input looks like a quanteda corpus but ",
         "quanteda is not installed.", call. = FALSE)
  d <- quanteda::convert(x, to = "data.frame")
  as_segments.data.frame(d, ..., unit = unit, quiet = quiet)
}

#' 契約を満たしているか
#' @param x 任意のオブジェクト
#' @export
is_segments <- function(x) inherits(x, "qe_segments")

#' @export
print.qe_segments <- function(x, n = 6, ...) {
  cat(sprintf("<qe_segments> %d segments from %d document(s)%s\n",
              nrow(x), length(unique(x$doc_id)),
              if (is.null(attr(x, "unit"))) "" else
                paste0(" [unit: ", attr(x, "unit"), "]")))
  print(utils::head(as.data.frame(x)[, 1:min(4, ncol(x))], n))
  if (nrow(x) > n) cat(sprintf("... %d more\n", nrow(x) - n))
  invisible(x)
}

#' @export
summary.qe_segments <- function(object, ...) {
  per <- table(object$doc_id)
  # セグメント長の分布は必ず見せる。長さは類似度に効く交絡である
  # (Palominos et al., 2024)。報告しないほうに手間がかかるようにする。
  cat(sprintf("Documents      : %d\n", length(per)))
  cat(sprintf("Segments       : %d (per document: median %g, range %d-%d)\n",
              nrow(object), stats::median(per), min(per), max(per)))
  cat(sprintf("Words/segment  : median %g, range %d-%d\n",
              stats::median(object$n_words), min(object$n_words),
              max(object$n_words)))
  cat(sprintf("Chars/segment  : median %g, range %d-%d\n",
              stats::median(object$n_char), min(object$n_char),
              max(object$n_char)))
  invisible(data.frame(doc_id = names(per), n_seg = as.integer(per)))
}


# ── 分割の便宜（義務ではない）────────────────────────────────

#' 語数の計数（ICU の語境界。空白を持たない言語でも動く）
#'
#' 空白分割は日本語で段落全体を1語と数える。ICU の語境界解析は CJK に
#' 辞書ベースの分割を持つので、同じ呼び出しで英語と日本語の両方が通る。
#' @keywords internal
.n_words <- function(x) {
  as.integer(stringi::stri_count_boundaries(
    x, opts_brkiter = stringi::stri_opts_brkiter(
      type = "word", skip_word_none = TRUE)))
}

# ICU に CJK 辞書が入っているかを初回だけ確かめる。無い環境では日本語の
# 語数が過小になるが、黙って別の指標に差し替えることはしない——無警告で
# 壊れるのは、この実装が取り除こうとしている失敗そのものである。
.qe_env <- new.env(parent = emptyenv())
.check_cjk_dict <- function() {
  if (!is.null(.qe_env$cjk)) return(invisible(.qe_env$cjk))
  .qe_env$cjk <- .n_words("昨日は朝から雨だった") > 3
  if (!.qe_env$cjk)
    warning("This build of ICU has no CJK word dictionary, so n_words will ",
            "undercount Japanese text (sentence splitting is unaffected -- ",
            "it is rule-based). Use by = \"chars\" for windows, and ",
            "min_words = 0 to disable the short-fragment merge.",
            call. = FALSE)
  invisible(.qe_env$cjk)
}

#' 文分割で保護する略語
#'
#' ICU の文境界は "Dr." のような略語で切ってしまう。分割前にこれらの
#' 終止符を私用領域文字へ退避し、分割後に戻す。
#' 拡張は c(qe_abbreviations(), "Univ")、無効化は character(0)。
#' "a.m." は実際に文末に来ることが多いので既定に含めない。
#' @export
qe_abbreviations <- function() {
  c("Dr", "Mr", "Mrs", "Ms", "Prof", "Sr", "Jr", "St", "Fig", "No", "Vol",
    "Ch", "e.g", "i.e", "cf", "vs", "approx", "Inc", "Ltd", "Ph.D",
    "M.A", "B.A")
}

.SENT_SENTINEL <- ""

#' 長いテキストを分割して、契約を満たす表を返す
#'
#' 長大な記述——インタビュー記録、日誌、複数段落の自由記述——は、そのまま
#' 埋め込むと一つの点になり、内部の推移が失われる。分割してから埋め込めば
#' 意味空間上の軌跡として扱える。ただし分割の単位は結果を変えるので、
#' 単位は分析者が明示的に選ぶべきものであり、既定値に委ねてはならない。
#' 自分の規則で切ったものを as_segments() に渡すほうが普通の使い方である。
#'
#' @details
#' 文分割と語数は ICU（stringi）の境界解析による。したがって英語の
#' `.!?` と日本語の `。！？` が同じ呼び出しで処理され、語数も日本語では
#' 形態素単位になる。言語を指定する引数はない——ICU が内部で書記系を見る
#' ので、判定すべき対象が存在しない。locale も指定しない（UAX#29 の
#' ロケール非依存部分しか使われず、実測で locale の効果はなかった）。
#'
#' 二言語を対照する分析では **`by = "sentence"` を使うこと**。ICU の
#' 日本語「語」は形態素相当なので、同一内容の対訳で日本語は英語の
#' およそ1.4倍の語数になる。`size = 50` は二つの言語で同じ窓ではない。
#' 対訳で一致したのは文の数だけだった。
#'
#' @param x        文字ベクトル。各要素が1つの文書。
#' @param by       "sentence"（文）、"words"（語数の窓）、"chars"（文字数の窓）、
#'   "paragraph"（空行区切り）。トークン上限が理由で切るなら "chars" が、
#'   言語をまたいで意味の揃う唯一の単位である。
#' @param size     窓の大きさ。NULL なら words = 50、chars = 200。
#' @param overlap  窓の重なり。既定 0。
#' @param min_words これ未満の語数の断片は直前の断片に併合する。既定 2、
#'   すなわち1語だけの断片（「はい。」のような相槌）だけを吸収する。
#'   3 にすると "It worked." のような正当な短文まで併合されるので勧めない。
#'   0 にすれば併合しない。
#' @param abbrev   文分割で保護する略語。qe_abbreviations() 参照。
#' @param ids      文書ID。既定は連番。
#' @return qe_segments（doc_id, segid, text, n_words, n_char, docname）
#' @examples
#' \dontrun{
#'   seg <- segment_text(interviews, by = "words", size = 80, overlap = 20)
#'   emb <- embed(setNames(seg$text, seg$docname))
#' }
#' @export
segment_text <- function(x, by = c("sentence", "words", "chars", "paragraph"),
                         size = NULL, overlap = 0, min_words = 2,
                         abbrev = qe_abbreviations(), ids = NULL) {
  by <- match.arg(by)
  if (!is.character(x)) stop("`x` must be a character vector.", call. = FALSE)
  if (is.null(ids)) ids <- seq_along(x)
  if (length(ids) != length(x))
    stop("`ids` must be the same length as `x`.", call. = FALSE)
  if (is.null(size)) size <- if (by == "chars") 200L else 50L
  if (overlap >= size && by %in% c("words", "chars"))
    stop("`overlap` must be smaller than `size`.", call. = FALSE)
  if (by %in% c("words", "sentence")) .check_cjk_dict()

  # ── 略語保護: 終止符を私用領域文字へ退避してから ICU に渡す ──
  protect <- function(s) {
    if (!length(abbrev)) return(s)
    for (a in abbrev)
      s <- gsub(paste0("(\\b", gsub("[.]", "\\\\.", a), ")\\."),
                paste0("\\1", .SENT_SENTINEL), s, perl = TRUE)
    s
  }
  unprotect <- function(s) gsub(.SENT_SENTINEL, ".", s, fixed = TRUE)

  # ICU は 彼は言った。「行きます。 で開き括弧を前文の末尾に残す。
  # 既知の挙動なので、未対応の開き括弧だけ次のセグメントへ送る。
  fix_open_bracket <- function(p) {
    if (length(p) < 2) return(p)
    for (i in seq_len(length(p) - 1)) {
      m <- regmatches(p[i], regexpr("[「『（(“\"']+$", p[i]))
      if (length(m) && nzchar(m)) {
        p[i]     <- sub("[「『（(“\"']+$", "", p[i])
        p[i + 1] <- paste0(m, p[i + 1])
      }
    }
    p
  }

  # ── 語・文字の窓: 境界の位置で原文から部分文字列を取る ────────
  # paste(collapse = " ") で組み直すと原文の空白と句読点が壊れる。
  # 「次の単位の開始の直前まで」を取れば overlap = 0 のとき再結合が
  # 原文と完全一致する（英日とも確認済み）。
  window_by <- function(s, type) {
    loc <- stringi::stri_locate_all_boundaries(
      s, opts_brkiter = stringi::stri_opts_brkiter(
        type = type, skip_word_none = (type == "word")))[[1]]
    if (all(is.na(loc))) return(trimws(s))
    starts <- loc[, "start"]
    n <- length(starts)
    step <- size - overlap
    from_i <- seq(1, n, by = step)
    from_i <- from_i[from_i <= n]
    vapply(from_i, function(i) {
      j <- min(i + size - 1L, n)
      to <- if (j >= n) nchar(s) else starts[j + 1L] - 1L
      substr(s, starts[i], to)
    }, "")
  }

  split_one <- function(s) {
    s <- trimws(s)
    if (is.na(s) || s == "") return(character(0))
    if (by == "paragraph") {
      p <- unlist(strsplit(s, "\n[[:space:]]*\n"))
    } else if (by == "sentence") {
      has_sent <- grepl(.SENT_SENTINEL, s, fixed = TRUE)
      if (has_sent)
        warning("Text contains U+E000, the character used internally to ",
                "protect abbreviations; abbreviation protection is skipped ",
                "for that document.", call. = FALSE)
      q <- if (has_sent) s else protect(s)
      p <- stringi::stri_split_boundaries(
        q, opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))[[1]]
      if (!has_sent) p <- unprotect(p)
      p <- fix_open_bracket(p)
    } else {
      p <- window_by(s, if (by == "words") "word" else "character")
    }
    p <- trimws(p); p <- p[nchar(p) > 0]
    # 短すぎる断片は直前に併合する（単独では意味が取れないため）。
    # 窓でも最後の窓は size に満たないことがある。同じ規則で吸収する。
    if (length(p) > 1 && min_words > 0) {
      keep <- character(0)
      for (q in p) {
        if (length(keep) && .n_words(q) < min_words) {
          keep[length(keep)] <- paste(keep[length(keep)], q)
        } else keep <- c(keep, q)
      }
      p <- keep
    }
    p
  }

  out <- do.call(rbind, lapply(seq_along(x), function(i) {
    p <- split_one(x[i])
    if (!length(p)) return(NULL)
    data.frame(doc_id = as.character(ids[i]), segid = seq_along(p), text = p,
               stringsAsFactors = FALSE)
  }))
  if (is.null(out))
    stop("segment_text(): every input produced zero segments.", call. = FALSE)
  as_segments(out, unit = by, quiet = TRUE)
}


# ── 逐語録の読み込み ─────────────────────────────────────────

.strip_speaker <- function(txt) {
  # Zoom / 文字起こしサービスの慣行: 行頭の「名前:」を話者とみなす。
  # WebVTT の <v Name>…</v> も同じ扱いにする。発見的手法であり、
  # 話者名にコロンが含まれる場合などは外れる。speaker = FALSE で無効。
  v <- regmatches(txt, regexpr("<v[^>]*>", txt))
  sp <- rep(NA_character_, length(txt))
  hasv <- grepl("<v\\s+[^>]+>", txt)
  if (any(hasv)) {
    sp[hasv] <- sub("^<v\\s+([^>]+?)\\s*>.*$", "\\1", txt[hasv])
    txt[hasv] <- gsub("</?v[^>]*>", "", txt[hasv])
  }
  pre <- grepl("^[[:space:]]*[^:\n]{1,40}[[:space:]]*:[[:space:]]", txt)
  sp[pre & is.na(sp)] <- trimws(sub("^[[:space:]]*([^:\n]{1,40})[[:space:]]*:.*$",
                                    "\\1", txt[pre & is.na(sp)]))
  txt[pre] <- trimws(sub("^[[:space:]]*[^:\n]{1,40}[[:space:]]*:[[:space:]]*",
                         "", txt[pre]))
  list(text = txt, speaker = sp)
}

.read_one <- function(f, format, unit, encoding) {
  ext <- tolower(tools::file_ext(f))
  if (format == "auto") format <- ext
  if (format %in% c("txt", "text", "md")) {
    raw <- paste(readLines(f, warn = FALSE, encoding = encoding),
                 collapse = "\n")
    u <- if (unit == "auto") "paragraph" else unit
    if (u == "line") {
      p <- unlist(strsplit(raw, "\n"))
    } else {
      p <- unlist(strsplit(raw, "\n[[:space:]]*\n"))
    }
    p <- trimws(p); p <- p[nchar(p) > 0]
    # Whisper 形状の検出。1行1セグメントの txt は構造化されて見えるが、
    # 実際は2〜5秒の音声区間であって話者交替でも文でもない。
    if (u == "paragraph" && length(p) == 1) {
      nl <- length(unlist(strsplit(raw, "\n")))
      if (nl > 5)
        warning("read_segments(): '", basename(f), "' has ", nl,
                " lines but no blank lines, so it became a single segment. ",
                "If those lines are automatic-transcription cues (Whisper, ",
                "Zoom), they are 2-5 second audio chunks, not speaker turns ",
                "-- prefer the .vtt/.srt file, which carries speakers and ",
                "timings. To split on every line anyway, pass unit = \"line\".",
                call. = FALSE)
    }
    return(data.frame(text = p, stringsAsFactors = FALSE))
  }
  if (format %in% c("vtt", "srt")) {
    ln <- readLines(f, warn = FALSE, encoding = encoding)
    ln <- ln[!grepl("^WEBVTT|^NOTE|^[0-9]+$", ln)]
    is_time <- grepl("-->", ln)
    cue <- cumsum(is_time)
    keep <- !is_time & cue > 0 & trimws(ln) != ""
    if (!any(keep)) stop("read_segments(): no cues found in ", basename(f),
                         call. = FALSE)
    p <- tapply(ln[keep], cue[keep], function(z) paste(trimws(z), collapse = " "))
    tm <- trimws(sub("-->.*$", "", ln[is_time]))
    return(data.frame(text = as.character(p),
                      time = tm[as.integer(names(p))],
                      stringsAsFactors = FALSE))
  }
  if (format %in% c("csv", "tsv")) {
    # 符号化の取り違えは "invalid input" 警告として現れ、行が黙って消える。
    # 警告をエラーに変えて、直し方を書いた上で止める。
    bad_enc <- FALSE
    d <- withCallingHandlers(
      utils::read.csv(f, sep = if (format == "tsv") "\t" else ",",
                      stringsAsFactors = FALSE, fileEncoding = encoding),
      warning = function(w) {
        if (grepl("invalid input|invalid multibyte|不正な入力", conditionMessage(w)))
          bad_enc <<- TRUE
        invokeRestart("muffleWarning")
      })
    if (bad_enc || !nrow(d))
      stop("read_segments(): '", basename(f), "' could not be read as ",
           encoding, ". This is what a character-encoding mismatch looks ",
           "like: rows vanish rather than turning into mojibake, so the ",
           "loss can be partial and silent. Re-save the file as UTF-8 (in ",
           "Excel: \"CSV UTF-8\"), or pass encoding = \"CP932\" for a ",
           "Japanese Windows export.", call. = FALSE)
    return(d)
  }
  if (format == "docx") {
    if (!requireNamespace("xml2", quietly = TRUE))
      stop("read_segments(): reading .docx needs the xml2 package ",
           "(install.packages(\"xml2\")). Or save the transcript as plain ",
           "text, which qualembed reads with no extra dependency.",
           call. = FALSE)
    tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(f, files = "word/document.xml", exdir = tmp)
    doc <- xml2::read_xml(file.path(tmp, "word", "document.xml"))
    ns  <- xml2::xml_ns(doc)
    ps  <- xml2::xml_find_all(doc, ".//w:p", ns)
    p   <- trimws(vapply(ps, function(z) paste(
      xml2::xml_text(xml2::xml_find_all(z, ".//w:t", ns)), collapse = ""), ""))
    p <- p[nchar(p) > 0]
    if (!length(p)) stop("read_segments(): no paragraphs found in ",
                         basename(f), call. = FALSE)
    return(data.frame(text = p, stringsAsFactors = FALSE))
  }
  if (format %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("read_segments(): reading .xlsx needs the readxl package ",
           "(install.packages(\"readxl\")). Or re-export from your ",
           "qualitative-analysis software as CSV, which qualembed reads ",
           "with no extra dependency.", call. = FALSE)
    return(as.data.frame(readxl::read_excel(f)))
  }
  stop("read_segments(): don't know how to read '", basename(f),
       "'. Supported: .txt .docx .vtt .srt .csv .tsv .xlsx.", call. = FALSE)
}

#' 逐語録・書き出しファイルからセグメント表を読む
#'
#' @details
#' 推奨する置き方は「1参加者1ファイル、話者交替を空行区切りの段落、
#' 話者名は `名前:` の接頭辞」である。.docx の逐語録はたいてい既にこの形で
#' あり、Zoom / Teams / Whisper の .vtt はさらに話者と時刻を持っている。
#'
#' 「1行1セグメント」の .txt は**勧めない**。その形を吐くのは Whisper の
#' txt 出力だが、あの行は2〜5秒の音声区間であって話者交替でも文でもない。
#' そのため .txt の既定単位は "line" ではなく "paragraph" であり、
#' 空行のない多行ファイルには警告が出る。
#'
#' 質的分析ソフトからの書き出し（CSV/Excel）は1行1セグメントなので、
#' そのまま読める。列名は as_segments() が自動照合する。
#'
#' @param path      ファイル、ファイルのベクトル、またはディレクトリ
#' @param format    "auto" または txt/docx/vtt/srt/csv/tsv/xlsx
#' @param unit      "auto"（txt/docx→paragraph, vtt/srt→cue, 表→row）
#' @param speaker   行頭の「名前:」を話者列に分離する（既定 TRUE）
#' @param doc_id    表形式のとき文書ID列名。ファイル群のときは無視
#'                  （ファイル名が doc_id になる）。
#' @param pattern   ディレクトリを渡したときのファイル絞り込み
#' @param recursive ディレクトリを再帰的に探す
#' @param encoding  ファイル符号化（既定 "UTF-8-BOM"）。R の文書で BOM 除去が
#'   保証されているのはこの指定だけで、BOM の無いファイルでも正しく動く。
#'   日本語の CSV を CP932 のまま既定で読むと、該当行が NA になって静かに
#'   落ちるため、読み込み後に NA があればエラーで止める。
#' @param ...       as_segments() に渡す
#' @return qe_segments
#' @export
read_segments <- function(path, format = "auto", unit = "auto",
                          speaker = TRUE, doc_id = NULL, pattern = NULL,
                          recursive = FALSE, encoding = "UTF-8-BOM", ...) {
  files <- if (length(path) == 1 && dir.exists(path)) {
    list.files(path, pattern = pattern %||%
                 "[.](txt|docx|vtt|srt|csv|tsv|xlsx)$",
               full.names = TRUE, recursive = recursive, ignore.case = TRUE)
  } else path
  if (!length(files)) stop("read_segments(): no files found at ", path,
                           call. = FALSE)
  miss <- files[!file.exists(files)]
  if (length(miss)) stop("read_segments(): file not found: ",
                         paste(miss, collapse = ", "), call. = FALSE)

  parts <- lapply(files, function(f) {
    d <- .read_one(f, format, unit, encoding)
    # ファイルごとにその場で正規名へ寄せる。形式の違うファイルを一つの
    # ディレクトリで混ぜたとき、content と text が並んで衝突するのを防ぐ。
    ct <- .pick_col(names(d), "text", NULL)
    if (!is.na(ct) && ct != "text") names(d)[names(d) == ct] <- "text"
    cd <- .pick_col(names(d), "doc_id", doc_id)
    if (!is.na(cd) && cd != "doc_id") names(d)[names(d) == cd] <- "doc_id"
    # 表形式の書き出しは自前の文書列を持っている。その場合はファイル名を
    # 足さない（足すと doc_id 候補が二つになって照合が止まる）。
    if (!"doc_id" %in% names(d))
      d$doc_id <- tools::file_path_sans_ext(basename(f))
    d
  })
  d <- do.call(rbind, lapply(parts, function(z) {
    miss <- setdiff(unique(unlist(lapply(parts, names))), names(z))
    for (m in miss) z[[m]] <- NA
    z[, sort(names(z)), drop = FALSE]
  }))

  # 符号化の取り違えは「文字化け」ではなく NA として現れる。空欄 ("") とは
  # 区別できるので、NA は静かに落とさず止める。英日混在のファイルでは
  # 一部の行だけが消えるので、黙って進むと最悪の壊れ方をする。
  if ("text" %in% names(d) && anyNA(d$text)) {
    bad <- which(is.na(d$text))
    stop("read_segments(): ", length(bad), " row(s) came back as NA (first at ",
         bad[1], "). This is the signature of a character-encoding mismatch, ",
         "not an empty cell -- a Shift_JIS/CP932 file read as UTF-8 turns the ",
         "affected rows into NA. Re-save the file as UTF-8 (in Excel: ",
         "\"CSV UTF-8\"), or pass encoding = \"CP932\".", call. = FALSE)
  }

  if (isTRUE(speaker) && "text" %in% names(d)) {
    sp <- .strip_speaker(d$text)
    d$text <- sp$text
    if (any(!is.na(sp$speaker))) d$speaker <- sp$speaker
  }
  # 読み込み順がそのまま文書内順序なので segid は確定している
  if (!any(tolower(names(d)) %in% .seg_aliases$segid)) {
    key <- if ("doc_id" %in% names(d)) d$doc_id else
      d[[names(d)[tolower(names(d)) %in% .seg_aliases$doc_id][1]]]
    if (!is.null(key)) d$segid <- stats::ave(seq_len(nrow(d)),
                                             as.character(key), FUN = seq_along)
  }
  as_segments(d, ...)
}


# ── 軌跡の統計 ───────────────────────────────────────────────

# emb の行と seg の行が対応していることを確かめる。行順に依存する設計は
# 黙って壊れるので、docname が rownames にあれば名前で並べ替える。
.align_emb <- function(emb, seg) {
  seg <- as_segments(seg, quiet = TRUE)
  rn <- rownames(emb)
  if (!is.null(rn) && all(seg$docname %in% rn)) {
    return(list(emb = emb[seg$docname, , drop = FALSE], seg = seg))
  }
  if (nrow(emb) != nrow(seg))
    stop("trajectory functions: the embedding matrix has ", nrow(emb),
         " rows but the segment table has ", nrow(seg), ". Embed the ",
         "segments with names, e.g. embed(setNames(seg$text, seg$docname)), ",
         "so the two can be matched by name rather than by position.",
         call. = FALSE)
  list(emb = emb, seg = seg)
}

#' 分割された文書の軌跡統計
#'
#' 統計量そのものは新しくない。段数で割った経路長（Toubia らの speed）、
#' 経路長に対する正味変位の比（circuitousness の逆数）、逐次類似度は、
#' Toubia ら (2021)、Palominos ら (2024)、Bedi ら (2015) がすでに定義し
#' 使っている。ここでの寄与は R で同じ検定の作法に載せたことだけである。
#'
#' @param emb   セグメントの埋め込み行列（rownames が docname なら名前で照合）
#' @param seg   as_segments() の契約を満たす表
#' @param axis  投影軸（semantic_projection の返り値と同じ長さ）。任意。
#' @return 文書ごとの data.frame。
#'   path_length はセグメント間コサイン距離の総和だが、**セグメント数と
#'   強く相関する**ため単独で解釈してはならない（デモ15では r = .92–.96）。
#'   step_mean はそれを段数で割ったもの。straightness は正味変位÷経路長で、
#'   1 に近いほど一方向に進み、0 に近いほど行きつ戻りつしている。
#'   異なる軌跡が同じ集約値を与えうる（Palominos et al., 2024）ため、
#'   スカラーと図は必ず併せて報告すること。
#' @seealso [trajectory_null()], [plot_recurrence()], [plot_arc()]
#' @export
trajectory_stats <- function(emb, seg, axis = NULL) {
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  docs <- unique(seg$doc_id)
  do.call(rbind, lapply(docs, function(dd) {
    k <- which(seg$doc_id == dd)
    k <- k[order(seg$segid[k])]
    if (length(k) < 2) return(NULL)
    M <- emb[k, , drop = FALSE]
    S <- cos_sim_matrix(M)
    steps <- 1 - S[cbind(seq_len(nrow(M) - 1), 2:nrow(M))]
    net   <- 1 - S[1, nrow(M)]
    r <- data.frame(doc_id = dd, n_seg = nrow(M),
                    n_words = sum(seg$n_words[k]),
                    path_length = sum(steps), step_mean = mean(steps),
                    net_displacement = net,
                    straightness = if (sum(steps) > 0) net / sum(steps) else NA_real_,
                    stringsAsFactors = FALSE)
    if (!is.null(axis)) {
      av <- as.numeric(axis)[k]
      r$axis_mean  <- mean(av); r$axis_sd <- stats::sd(av)
      r$axis_range <- diff(range(av))
      r$axis_start <- av[1]; r$axis_end <- av[length(av)]
    }
    r
  }))
}

#' 軌跡統計の帰無分布（文書内で段の順序を入れ替える）
#'
#' 経路長や直進度は、それ単独では大きいのか小さいのか判定できない。
#' 同じ段を無作為な順序で並べ替えれば、同じ段数・同じ語数・同じ内容を
#' 保ったまま「順序だけを壊した」比較対象が得られる。観測値がこの分布の
#' どこに落ちるかが、順序が担っている情報の量である。局所的に一貫した
#' 語りは、並べ替えた自分より短い距離しか動かないはずである。
#'
#' @param emb    セグメントの埋め込み行列
#' @param seg    as_segments() の契約を満たす表
#' @param n_perm 並べ替え回数（既定 999）
#' @param stat   評価する統計量（"path_length" / "step_mean" / "straightness"）
#' @return 文書ごとの data.frame(doc_id, n_seg, observed, null_mean, z, p)。
#'   p は両側で、(#{|null - mean| >= |obs - mean|} + 1) / (n_perm + 1)。
#' @export
trajectory_null <- function(emb, seg, n_perm = 999, stat = "path_length") {
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  docs <- unique(seg$doc_id)
  do.call(rbind, lapply(docs, function(dd) {
    k <- which(seg$doc_id == dd)
    k <- k[order(seg$segid[k])]
    if (length(k) < 3) return(NULL)          # 3段未満は並べ替えの意味がない
    S <- cos_sim_matrix(emb[k, , drop = FALSE])
    path <- function(o) {
      st <- 1 - S[cbind(o[-length(o)], o[-1])]
      switch(stat,
             path_length = sum(st),
             step_mean   = mean(st),
             straightness = { d <- 1 - S[o[1], o[length(o)]]
                              if (sum(st) > 0) d / sum(st) else NA_real_ },
             stop("unknown stat: ", stat))
    }
    obs  <- path(seq_along(k))
    null <- replicate(n_perm, path(sample.int(length(k))))
    mu   <- mean(null, na.rm = TRUE)
    sdv  <- stats::sd(null, na.rm = TRUE)
    data.frame(doc_id = dd, n_seg = length(k), observed = obs,
               null_mean = mu,
               z = if (is.finite(sdv) && sdv > 0) (obs - mu) / sdv else NA_real_,
               p = (sum(abs(null - mu) >= abs(obs - mu), na.rm = TRUE) + 1) /
                   (n_perm + 1),
               stringsAsFactors = FALSE)
  }))
}

#' 再帰定量化（RQA）: 類似度行列を固定再帰率で二値化して要約する
#'
#' 埋め込みベクトルはそれ自体が状態なので、遅延埋め込み再構成は不要で
#' ある（m = 1, τ = 1）。これは文章に RQA を当てるときの最大の人工物源を
#' 取り除く。閾値は固定再帰率で決める——プロバイダごとにコサインの
#' 尺度が違い、文書ごとに長さが違うため、絶対閾値では比較できない。
#'
#' @param emb  セグメントの埋め込み行列
#' @param seg  as_segments() の契約を満たす表
#' @param rr   目標再帰率（既定 .05）
#' @param lmin 線とみなす最小長（既定 2）
#' @return 文書ごとの data.frame(doc_id, n_seg, threshold, RR, DET, LAM, L,
#'   L_max, TT, ENTR)。RR は再帰率、DET は対角線に乗る再帰の割合（同じ話題を
#'   同じ順序で辿り直す度合い）、LAM は垂直線の割合（一つの話題に留まる
#'   度合い）、L は反復挿話の平均長、TT は留まる長さの平均、ENTR は
#'   対角線長分布のエントロピー。
#' @export
recurrence_stats <- function(emb, seg, rr = 0.05, lmin = 2) {
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  runs <- function(v) { r <- rle(v); r$lengths[r$values] }
  do.call(rbind, lapply(unique(seg$doc_id), function(dd) {
    k <- which(seg$doc_id == dd); k <- k[order(seg$segid[k])]
    n <- length(k)
    if (n < 4) return(NULL)
    S <- cos_sim_matrix(emb[k, , drop = FALSE])
    off <- S[upper.tri(S)]
    thr <- stats::quantile(off, 1 - rr, names = FALSE)
    R <- S >= thr; diag(R) <- FALSE
    dl <- unlist(lapply(seq(-(n - 2), n - 2), function(d)
      runs(R[cbind(pmax(1, 1 - d):pmin(n, n - d),
                   pmax(1, 1 + d):pmin(n, n + d))])))
    vl <- unlist(lapply(seq_len(n), function(j) runs(R[, j])))
    dl <- dl[dl >= lmin]; vl <- vl[vl >= lmin]
    nrec <- sum(R)
    ph <- if (length(dl)) table(dl) / length(dl) else numeric(0)
    data.frame(doc_id = dd, n_seg = n, threshold = round(thr, 4),
               RR   = round(nrec / (n * (n - 1)), 4),
               DET  = round(if (nrec > 0) sum(dl) / nrec else NA_real_, 4),
               LAM  = round(if (nrec > 0) sum(vl) / nrec else NA_real_, 4),
               L    = round(if (length(dl)) mean(dl) else NA_real_, 3),
               L_max = if (length(dl)) max(dl) else NA_integer_,
               TT   = round(if (length(vl)) mean(vl) else NA_real_, 3),
               ENTR = round(if (length(ph)) -sum(ph * log(ph)) else NA_real_, 4),
               stringsAsFactors = FALSE)
  }))
}


# ── 軌跡の図 ─────────────────────────────────────────────────

#' 概念再帰プロット（Angus et al., 2012）
#'
#' セル (i, j) は**全次元**の埋め込み同士のコサイン類似度である。次元削減は
#' 一切なく、両軸はセグメントの位置にすぎない。2次元投影が距離を歪める
#' という問題 (Chari & Pachter, 2023) をそもそも踏まない唯一の表示である。
#' 閾値は掛けない（Angus に倣い、Eckmann/Marwan の二値化はしない）。
#'
#' 読み方: 対角付近のまとまり = 一つの話題が続く区間。対角から離れた
#' まとまり = 前の話題への回帰。明るい縦筋 = その後ずっと参照され続ける
#' 早い時点の一節。暗い領域 = 新しい話。
#'
#' @param emb    セグメントの埋め込み行列
#' @param seg    as_segments() の契約を満たす表
#' @param doc    描く文書。複数ある場合は必須。
#' @param anchor 色の基準。"quantile"（既定）は非対角セルの分位で
#'   両端を決める。商用APIのコサインは0.6〜0.95の狭い帯に集まるため、
#'   0〜1 で塗ると図が一色になる。"range" は実測の最小最大。
#' @param probs  anchor = "quantile" のときの下側・上側確率
#' @param title  図題
#' @return ggplot（表示もする）
#' @export
plot_recurrence <- function(emb, seg, doc = NULL,
                            anchor = c("quantile", "range"),
                            probs = c(.02, .98), title = NULL) {
  anchor <- match.arg(anchor)
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  docs <- unique(seg$doc_id)
  if (is.null(doc)) {
    if (length(docs) > 1)
      stop("plot_recurrence(): a recurrence plot is a within-document ",
           "object, but the table holds ", length(docs), " documents (",
           paste(utils::head(docs, 4), collapse = ", "),
           if (length(docs) > 4) ", ..." else "",
           "). Name one with doc = .", call. = FALSE)
    doc <- docs[1]
  }
  k <- which(seg$doc_id == doc); k <- k[order(seg$segid[k])]
  if (length(k) < 3)
    stop("plot_recurrence(): document '", doc, "' has only ", length(k),
         " segment(s).", call. = FALSE)
  S <- cos_sim_matrix(emb[k, , drop = FALSE])
  n <- nrow(S)
  df <- expand.grid(i = seq_len(n), j = seq_len(n))
  df$sim <- S[cbind(df$i, df$j)]
  off <- S[upper.tri(S)]
  lim <- if (anchor == "quantile")
    stats::quantile(off, probs, names = FALSE) else range(off)

  p <- ggplot(df, aes(.data$i, .data$j, fill = .data$sim)) +
    geom_raster() +
    scale_fill_gradient2(low = "#3B82F6", mid = "white", high = "#EF4444",
                         midpoint = stats::median(off), limits = lim,
                         oob = scales::squish, name = "Cosine\nsimilarity") +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    coord_fixed() +
    labs(title = title %||% paste0("Conceptual recurrence: ", doc),
         x = "Segment", y = "Segment") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank())
  print(p)
  invisible(p)
}

#' 意味の弧: セグメントごとの量を語りの位置に対して描く
#'
#' 縦軸はいずれも**全次元**で計算される。横軸はセグメントの位置しか
#' 担わないので、2次元投影の歪みを踏まない。
#'
#' @param emb  セグメントの埋め込み行列
#' @param seg  as_segments() の契約を満たす表
#' @param y    "projection"（high/low アンカーへの投影）、"step"（直前との
#'   コサイン距離＝Toubia らの speed を段ごとに解いたもの）、
#'   "forward_flow"（先行する全セグメントとの平均距離＝Gray et al., 2019）
#' @param high,low  y = "projection" のときのアンカー埋め込み行列
#' @param x    "auto"（1文書なら position、複数なら relative）/ "position" /
#'   "relative"（(t-0.5)/T）
#' @param smooth   loess の平滑線。"auto"（既定）は最短文書が10段以上の
#'   ときだけ引く——数点に loess を当てても形は読めない。
#' @param null_band 段の順序を入れ替えた帰無分布（0 なら計算しない。999 が目安）。
#'   y = "step" / "forward_flow" では位置ごとの 2.5–97.5% 帯を描く。
#'   y = "projection" では**帯を描かない**——投影値は順序に依存しないので、
#'   並べ替えても値の集合は変わらず、位置ごとの帯は定義上まったいらになり、
#'   何も検定しない。代わりに位置と投影の順位相関に対する並べ替え p を
#'   副題に出す（弧に傾きがあるかを問う、意味のある帰無仮説である）。
#'   複数文書に帯を描くときは文書ごとに面を分ける（重ねると読めない）。
#' @param center  "auto"（1文書は生値、複数は文書内平均中心化）/ "none" / "mean"
#' @param title  図題
#' @return ggplot（表示もする）
#' @export
plot_arc <- function(emb, seg, y = c("projection", "step", "forward_flow"),
                     high = NULL, low = NULL,
                     x = c("auto", "position", "relative"),
                     smooth = "auto", null_band = 0,
                     center = c("auto", "none", "mean"), title = NULL) {
  y <- match.arg(y); x <- match.arg(x); center <- match.arg(center)
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  docs <- unique(seg$doc_id)
  if (x == "auto")      x      <- if (length(docs) > 1) "relative" else "position"
  if (center == "auto") center <- if (length(docs) > 1) "mean" else "none"
  if (y == "projection" && (is.null(high) || is.null(low)))
    stop("plot_arc(): y = \"projection\" needs anchor matrices. Pass ",
         "high = and low = (embeddings of the phrases that define the two ",
         "poles), as in semantic_projection().", call. = FALSE)

  # 縦軸の値を1文書ぶん計算する（すべて全次元）
  yval <- function(k, order_idx = seq_along(k)) {
    kk <- k[order_idx]
    if (y == "projection")
      return(as.numeric(semantic_projection(emb[kk, , drop = FALSE], high, low)))
    S <- cos_sim_matrix(emb[kk, , drop = FALSE])
    if (y == "step") return(c(NA_real_, 1 - S[cbind(seq_len(length(kk) - 1),
                                                    2:length(kk))]))
    vapply(seq_along(kk), function(t) if (t == 1) NA_real_ else
      mean(1 - S[t, seq_len(t - 1)]), 0)
  }

  df <- do.call(rbind, lapply(docs, function(dd) {
    k <- which(seg$doc_id == dd); k <- k[order(seg$segid[k])]
    v <- yval(k)
    if (center == "mean") v <- v - mean(v, na.rm = TRUE)
    data.frame(doc_id = dd, segid = seq_along(k), y = v,
               xx = if (x == "relative") (seq_along(k) - .5) / length(k)
                    else seq_along(k), stringsAsFactors = FALSE)
  }))

  if (identical(smooth, "auto"))
    smooth <- min(table(df$doc_id)) >= 10

  # y = "projection" は順序に依存しないので、位置ごとの帯は定義上
  # まったいらになる。帯の代わりに傾きの並べ替え検定を出す。
  trend <- NULL
  if (null_band > 0 && y == "projection") {
    trend <- do.call(rbind, lapply(docs, function(dd) {
      v <- df$y[df$doc_id == dd]
      if (length(v) < 4) return(NULL)
      rho  <- suppressWarnings(stats::cor(seq_along(v), v, method = "spearman"))
      nullr <- replicate(null_band, suppressWarnings(
        stats::cor(seq_along(v), sample(v), method = "spearman")))
      data.frame(doc_id = dd, rho = rho,
                 p = (sum(abs(nullr) >= abs(rho)) + 1) / (null_band + 1),
                 stringsAsFactors = FALSE)
    }))
  }

  band <- NULL
  if (null_band > 0 && y != "projection") {
    band <- do.call(rbind, lapply(docs, function(dd) {
      k <- which(seg$doc_id == dd); k <- k[order(seg$segid[k])]
      if (length(k) < 3) return(NULL)
      M <- replicate(null_band, { v <- yval(k, sample.int(length(k)))
                                  if (center == "mean")
                                    v - mean(v, na.rm = TRUE) else v })
      q <- t(apply(M, 1, stats::quantile, c(.025, .975), na.rm = TRUE))
      data.frame(doc_id = dd,
                 xx = if (x == "relative") (seq_along(k) - .5) / length(k)
                      else seq_along(k),
                 lo = q[, 1], hi = q[, 2], stringsAsFactors = FALSE)
    }))
  }

  ylab <- switch(y,
    projection   = "Projection on the anchor axis",
    step         = "Cosine distance from previous segment",
    forward_flow = "Mean distance from all preceding segments")
  p <- ggplot(df, aes(.data$xx, .data$y))
  if (!is.null(band))
    p <- p + geom_ribbon(data = band, inherit.aes = FALSE,
                         aes(x = .data$xx, ymin = .data$lo, ymax = .data$hi,
                             group = .data$doc_id),
                         fill = "grey80", alpha = .6)
  p <- p + geom_line(aes(color = .data$doc_id, group = .data$doc_id),
                     linewidth = .7, na.rm = TRUE) +
    geom_point(aes(color = .data$doc_id), size = 1.6, na.rm = TRUE)
  if (isTRUE(smooth) && nrow(df) > 6)
    p <- p + geom_smooth(aes(group = .data$doc_id), method = "loess",
                         se = FALSE, linewidth = .5, linetype = "22",
                         color = "grey30", na.rm = TRUE, formula = y ~ x)
  sub <- if (is.null(trend)) NULL else
    paste(strwrap(paste0(
      "Trend against position (", null_band,
      " permutations of segment order): ",
      paste(sprintf("%s rho = %+.2f, p = %.3f", trend$doc_id,
                    trend$rho, trend$p), collapse = "; ")),
      width = 78), collapse = "\n")
  p <- p + labs(title = title %||% "Semantic arc", subtitle = sub,
                x = if (x == "relative") "Relative position in document"
                    else "Segment", y = ylab, color = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = if (length(docs) > 1 && is.null(band))
            "right" else "none")
  # 帯は文書ごとに違う。重ねると鋸歯状の意味のない形になるので面を分ける。
  if (!is.null(band) && length(docs) > 1)
    p <- p + facet_wrap(~ doc_id, scales = "free_x")
  print(p)
  invisible(p)
}

#' 射影の忠実さを測る（矢印の図を信じてよいかの診断）
#'
#' 2次元への射影は距離を歪める。どれだけ歪むかは場合による——測れば済む。
#' 全次元でのペア距離と、平面上でのペア距離の順位相関（Shepard 相関）を
#' 文書ごとに返す。相関が高ければ、その平面図の「近い／遠い」は空間の
#' 「近い／遠い」を概ね反映している。
#'
#' 実測の目安（デモ15、5-8セグメントの逐語）: 1文書ずつ射影すれば
#' PC1+PC2 が分散の 56%、順位相関の中央値 .87。40名を一つの共通射影に
#' 載せると 12% と .16 まで落ちる。**軌跡の地図は文書ごとに描くこと。**
#'
#' @param emb   セグメントの埋め込み行列
#' @param seg   as_segments() の契約を満たす表
#' @param scope "document"（文書ごとに射影）/ "shared"（全体で一つの射影）
#' @return 文書ごとの data.frame(doc_id, n_seg, var_2d, shepard)
#' @export
trajectory_fidelity <- function(emb, seg, scope = c("document", "shared")) {
  scope <- match.arg(scope)
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  shared_pc <- if (scope == "shared") stats::prcomp(emb) else NULL
  do.call(rbind, lapply(unique(seg$doc_id), function(dd) {
    k <- which(seg$doc_id == dd); k <- k[order(seg$segid[k])]
    if (length(k) < 3) return(NULL)
    M <- emb[k, , drop = FALSE]
    if (is.null(shared_pc)) {
      pc <- stats::prcomp(M); xy <- pc$x[, 1:2, drop = FALSE]
      ve <- sum(pc$sdev[1:2]^2) / sum(pc$sdev^2)
    } else {
      xy <- shared_pc$x[k, 1:2, drop = FALSE]
      ve <- sum(shared_pc$sdev[1:2]^2) / sum(shared_pc$sdev^2)
    }
    d_full <- stats::as.dist(1 - cos_sim_matrix(M))
    data.frame(doc_id = dd, n_seg = length(k), var_2d = round(ve, 3),
               shepard = round(suppressWarnings(
                 stats::cor(d_full, stats::dist(xy), method = "spearman")), 3),
               stringsAsFactors = FALSE)
  }))
}

#' 意味空間上の軌跡を矢印で描く
#'
#' 発言の順に点を結び、矢印で向きを示す。これは「どの順に、どちらへ動いたか」
#' を見るための図である。
#'
#' **矢印が担う順序は射影で歪まない。歪むのは距離と角度である。**
#' したがって「3番目でいったん戻って、そこから一方向に進んだ」という読みは
#' この図から取ってよい。「Aさんのほうが長く動いた」という読みは取っては
#' ならない——それは全次元で測る量で、trajectory_stats() が返す。
#' 図の副題に、その射影がどれだけ距離を保っているか（Shepard 順位相関）を
#' 出すので、目盛りとして使ってよいかは毎回そこで判断できる。
#'
#' 射影は既定で**文書ごと**に計算する。複数の逐語を一つの共通平面に載せると
#' 忠実さが激しく落ちるため（実測で順位相関 .87 → .16）、scope = "shared"
#' は明示的に選んだときだけ使われ、忠実さが低ければ警告する。
#'
#' @param emb    セグメントの埋め込み行列
#' @param seg    as_segments() の契約を満たす表
#' @param doc    描く文書。NULL なら全文書（scope に従う）。
#' @param scope  "document"（既定・文書ごとの射影）/ "shared"（共通の射影）
#' @param arrows 進行方向の矢印を描く（既定 TRUE）
#' @param label  セグメント番号を書く（既定 TRUE）
#' @param title  図題
#' @return ggplot（表示もする）
#' @seealso [trajectory_fidelity()], [trajectory_stats()], [plot_recurrence()]
#' @export
plot_trajectory <- function(emb, seg, doc = NULL,
                            scope = c("document", "shared"),
                            arrows = TRUE, label = TRUE, title = NULL) {
  scope <- match.arg(scope)
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  if (!is.null(doc)) {
    keep <- seg$doc_id %in% doc
    if (!any(keep)) stop("plot_trajectory(): no document named '",
                         paste(doc, collapse = "', '"), "'.", call. = FALSE)
    emb <- emb[keep, , drop = FALSE]; seg <- seg[keep, , drop = FALSE]
  }
  docs <- unique(seg$doc_id)
  if (length(docs) > 1 && scope == "document") scope <- "shared"

  fid <- trajectory_fidelity(emb, seg, scope = scope)
  if (scope == "shared" && length(docs) > 1) {
    med <- stats::median(fid$shepard, na.rm = TRUE)
    if (!is.na(med) && med < .5)
      warning("plot_trajectory(): on a shared projection these documents ",
              "keep only a rank correlation of ", sprintf("%.2f", med),
              " with their full-space distances -- the map's near/far is ",
              "close to meaningless. Draw one document at a time (doc = ), ",
              "which in our checks recovers about .87.", call. = FALSE)
  }

  # 射影: 文書ごと（既定）か、全体で一つか
  d <- do.call(rbind, lapply(docs, function(dd) {
    k <- which(seg$doc_id == dd); k <- k[order(seg$segid[k])]
    xy <- if (scope == "document") stats::prcomp(emb[k, , drop = FALSE])$x[, 1:2]
          else stats::prcomp(emb)$x[k, 1:2, drop = FALSE]
    data.frame(doc_id = dd, segid = seg$segid[k],
               PC1 = xy[, 1], PC2 = xy[, 2], stringsAsFactors = FALSE)
  }))

  sub <- paste(strwrap(sprintf(
    "%s projection. PC1 + PC2 hold %.0f%% of the variance, and distances on the page keep a rank correlation of %s with the distances actually measured. The arrows show the order, which projection preserves exactly; their lengths are not the distances travelled.",
    if (scope == "document") "Per-document" else "Shared",
    100 * stats::median(fid$var_2d, na.rm = TRUE),
    sub("^0[.]", ".", sprintf("%.2f", stats::median(fid$shepard, na.rm = TRUE)))),
    width = 72), collapse = "\n")

  # 1段ごとに矢印を引く。geom_path の arrow は経路の最後にしか付かない。
  seg_df <- do.call(rbind, lapply(docs, function(dd) {
    z <- d[d$doc_id == dd, ]; z <- z[order(z$segid), ]
    if (nrow(z) < 2) return(NULL)
    data.frame(doc_id = dd,
               x = z$PC1[-nrow(z)], y = z$PC2[-nrow(z)],
               xend = z$PC1[-1],    yend = z$PC2[-1],
               stringsAsFactors = FALSE)
  }))

  # 始点は白抜き、終点は四角。どちらが語りの入口かが一目でわかるように。
  d$role <- "mid"
  for (dd in docs) {
    k <- which(d$doc_id == dd); k <- k[order(d$segid[k])]
    d$role[k[1]] <- "start"; d$role[k[length(k)]] <- "end"
  }

  p <- ggplot(d, aes(.data$PC1, .data$PC2))
  if (!is.null(seg_df)) {
    p <- p + geom_segment(
      data = seg_df, inherit.aes = FALSE,
      aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
          colour = .data$doc_id),
      linewidth = .5, alpha = .8,
      arrow = if (isTRUE(arrows))
        arrow(length = unit(.22, "cm"), type = "closed", angle = 22) else NULL,
      show.legend = FALSE)
  }
  p <- p + geom_point(aes(colour = .data$doc_id, shape = .data$role,
                          fill = .data$role), size = 2.4, stroke = .7) +
    scale_shape_manual(values = c(start = 21, mid = 19, end = 22),
                       breaks = c("start", "end"),
                       labels = c("first segment", "last segment"),
                       name = NULL) +
    scale_fill_manual(values = c(start = "white", mid = NA, end = "white"),
                      guide = "none") +
    labs(title = title %||% "Trajectory through semantic space",
         subtitle = sub, x = "PC1", y = "PC2", colour = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 7.5, colour = "grey35",
                                       lineheight = 1.15),
          panel.grid.minor = element_blank(),
          legend.position = "bottom", legend.box = "horizontal") +
    guides(colour = if (length(docs) > 1) guide_legend(order = 2) else "none")
  if (isTRUE(label))
    p <- p + geom_text_repel(aes(label = .data$segid), size = 3,
                             box.padding = .35, segment.color = "grey70",
                             max.overlaps = 20, seed = 1)
  if (length(docs) > 1) p <- p + facet_wrap(~ doc_id, scales = "free")
  print(p)
  invisible(p)
}

#' 2次元投影上のセグメント配置（非推奨。plot_trajectory() を使うこと）
#'
#' plot_trajectory() に置き換えられた。新関数は矢印で順序を示し、射影が
#' どれだけ距離を保っているかを副題に出す。
#' @param emb,seg,line,label,title 旧引数
#' @seealso [plot_trajectory()]
#' @export
plot_trajectory_2d <- function(emb, seg, line = TRUE, label = TRUE,
                               title = NULL) {
  plot_trajectory(emb, seg, arrows = line, label = label, title = title)
}


# ── 旧 API（非推奨・後方互換のためだけに残す）─────────────────

#' 2次元投影上の軌跡長（非推奨）
#'
#' **PC1・PC2 の平面上で距離を測る。すなわち埋め込み空間ではなく「図」を
#' 測っている。** 2次元への射影は距離構造を保存しない (Chari & Pachter,
#' 2023)。距離には trajectory_stats() を使うこと。
#'
#' @param df        pca_2d()$df にperson列とtime列を追加したもの
#' @param person_col 個人ID列名
#' @param time_col   時点列名
#' @return 個人ごとの2次元経路長（total_dist_2d）
#' @seealso [trajectory_stats()]
#' @export
trajectory_length <- function(df, person_col = "person", time_col = "time") {
  warning("trajectory_length() measures distance in the 2-D PCA projection, ",
          "not in the embedding space; 2-D projections do not preserve ",
          "distances. Use trajectory_stats() for a distance you can ",
          "interpret.", call. = FALSE)
  df |>
    arrange(.data[[person_col]], .data[[time_col]]) |>
    group_by(.data[[person_col]]) |>
    summarise(n_timepoints  = n(),
              total_dist_2d = sum(sqrt(diff(PC1)^2 + diff(PC2)^2), na.rm = TRUE),
              .groups = "drop")
}

#' 縦断軌跡の可視化（非推奨）
#'
#' plot_trajectory() に置き換えられた。新関数は埋め込み行列とセグメント表を
#' 直接受け取り、1段ごとの矢印を描き、その射影が距離をどれだけ保っているかを
#' 図に印字する。本関数は v0.2.0 との後方互換のためだけに残す。
#'
#' @param df        person・time・PC1・PC2・label を含むデータフレーム
#' @param person_col 個人ID列名
#' @param title     図のタイトル
#' @seealso [plot_trajectory()]
#' @export
plot_trajectories <- function(df, person_col = "person",
                              title = "Trajectories in semantic space") {
  warning("plot_trajectories() is superseded by plot_trajectory(), which takes ",
          "the embedding matrix and a segment table directly, draws a per-step ",
          "arrow, and prints how well the projection preserves the measured ",
          "distances.", call. = FALSE)
  p <- ggplot(df, aes(PC1, PC2, color = .data[[person_col]],
                      group = .data[[person_col]])) +
    geom_path(arrow = arrow(length = unit(.25, "cm"), type = "closed"),
              linewidth = 0.9, alpha = .8) +
    geom_point(alpha = .85) +
    geom_text_repel(aes(label = label), size = 3.2, box.padding = .4,
                    segment.size = .3, segment.color = "grey65") +
    labs(title = title, color = person_col, x = "PC1", y = "PC2") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  print(p)
  invisible(p)
}
