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

#' 埋め込み行列をアーカイブ（再現性は API 再取得ではなくこの行列に付随する。
#' OSF にはこの output/embeddings/ ごと公開する）
#' @param emb_list 名前付きリスト（各要素は embed() の戻り値の行列）
#' @param name     デモ名
#' @param provider プロバイダ名
#' @param dir      出力先ディレクトリ
#' @export
save_embeddings <- function(emb_list, name, provider,
                            dir = file.path("output", "embeddings")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
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


# ── 7. 縦断分析（追加的応用・デモでは未使用） ────────────────

#' 個人の意味空間上の軌跡を計算
#' @param df        pca_2d()$df にperson列とtime列を追加したもの
#' @param person_col 個人ID列名
#' @param time_col   時点列名
#' @return 個人ごとの軌跡長（total_dist）を含むデータフレーム
#' @export
trajectory_length <- function(df, person_col = "person", time_col = "time") {
  df |>
    arrange(.data[[person_col]], .data[[time_col]]) |>
    group_by(.data[[person_col]]) |>
    summarise(
      n_timepoints = n(),
      total_dist   = sum(sqrt(diff(PC1)^2 + diff(PC2)^2), na.rm = TRUE),
      .groups = "drop"
    )
}

#' 縦断軌跡の可視化
#' @param df        person・time・PC1・PC2・label を含むデータフレーム
#' @param person_col 個人ID列名
#' @param title     図のタイトル
#' @export
plot_trajectories <- function(df, person_col = "person",
                              title = "Trajectories in semantic space") {
  p <- ggplot(df, aes(PC1, PC2,
                       color  = .data[[person_col]],
                       group  = .data[[person_col]])) +
    geom_path(arrow = arrow(length = unit(.25, "cm"), type = "closed"),
              linewidth = 0.9, alpha = .8) +
    geom_point(aes(size = if ("time" %in% names(df)) time else 1),
               alpha = .85, show.legend = "time" %in% names(df)) +
    geom_text_repel(aes(label = label), size = 3.2,
                    box.padding = .4, segment.size = .3,
                    segment.color = "grey65") +
    labs(title = title, color = person_col,
         x = "PC1", y = "PC2", size = "time") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  print(p)
  invisible(p)
}

