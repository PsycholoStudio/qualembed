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

#' Compute the cosine similarity matrix of an embedding
#'
#' Computes the cosine similarity between every pair of rows of an embedding
#' matrix. Cosine ignores vector length and compares directions only, which is
#' the quantity embedding APIs are trained to make meaningful; it is the
#' default similarity used throughout this package.
#'
#' @param mat A numeric matrix with one row per text and one column per
#'   embedding dimension, as returned by \code{\link{embed}}. At least two
#'   rows are required, and no row may have zero or non-finite length.
#' @return An n by n symmetric matrix of cosine similarities with ones on the
#'   diagonal, where n is the number of rows of \code{mat}. Row and column
#'   names are inherited from \code{rownames(mat)}, so the texts themselves
#'   label the result.
#' @details
#' The values in this matrix will look uniformly large, and that is the normal
#' case rather than a sign that the texts are alike: the vectors share a common
#' component that puts a floor under every pair. A contrast between groups is
#' therefore small in absolute terms even
#' when it is strong; read it through \code{\link{within_between_sim}}, which
#' standardises it, and \code{\link{test_delta}}, which tests it against a
#' permutation null, rather than from the size of the difference itself.
#' @examples
#' # The floor is easiest to see on vectors that share a component, as
#' # embeddings do. Two groups are present, but every cosine is high.
#' set.seed(1)
#' centres <- matrix(rnorm(2 * 64), 2, 64)
#' m <- centres[rep(1:2, each = 6), ] +
#'   matrix(rnorm(12 * 64, sd = 1.2), 12, 64) + 3
#' rownames(m) <- paste0("item", 1:12)
#' groups <- rep(c("a", "b"), each = 6)
#'
#' s <- cos_sim_matrix(m)
#' range(s[upper.tri(s)])            # nothing is dissimilar
#' within_between_sim(s, groups)     # delta is small, delta_std is not
#' test_delta(s, groups, n_perm = 999)$p
#' @seealso \code{\link{within_between_sim}} and \code{\link{test_delta}} to
#'   compare groups, \code{\link{test_ari}} to test a partition,
#'   \code{\link{centered_sim_matrix}} for the mean-centered variant
#'   and \code{\link{euclidean_dist}} for a distance-based alternative.
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

#' Compute cosine similarities after mean-centering the embedding
#'
#' Subtracts the column means of the embedding matrix before measuring angles,
#' which allows negative similarities to appear. Embeddings are not spread
#' around the origin: every vector shares a large common component, so raw
#' cosines are almost never negative even between texts that mean opposite
#' things. For questions about opposition
#' (reverse-keyed items, opposing constructs, agreement versus disagreement)
#' this means that no coordinates capable of expressing opposition are in use,
#' rather than that no opposition exists. Intended for exploration and for
#' sign-related checks; confirmatory analyses in the accompanying paper use
#' \code{\link{cos_sim_matrix}} instead.
#'
#' @param mat A numeric matrix with one row per text, as returned by
#'   \code{\link{embed}}. At least three rows are required, and no row may sit
#'   exactly at the mean of the pool.
#' @return An n by n similarity matrix, carrying the attribute
#'   \code{centering = "mean"} so that centered matrices cannot be mistaken
#'   for raw ones.
#' @details
#' Centering restores negative values, but at two costs. First, the
#' similarities become pool-dependent: adding or removing one text moves every
#' pair, so a centered value cannot be read as a property of that pair alone
#' the way a raw cosine can. Second, whether it helps recover a partition is
#' not something a rule can settle in advance: it wins on some pools and loses
#' on others, and the pools where it wins are not the ones where whitening or
#' eigencomponent removal win.
#'
#' Centering is not a repair of the sign. After centering, the vectors sum to
#' zero, so the entries of the centered Gram matrix sum to zero identically
#' and negative values must occur whether or not any opposition is present.
#' The appearance of negative similarities is therefore not evidence of
#' anything on its own. The answerable question is whether the negatives fall
#' on the pairs theory says are opposed, which needs a known key to check
#' against and a baseline of calling every pair positive to beat.
#'
#' With only two rows the two centered vectors necessarily point in opposite
#' directions and the similarity is fixed at -1 by construction, so the
#' function stops rather than return a meaningless number.
#' @seealso \code{\link{cos_sim_matrix}} (no centering) and
#'   \code{\link{double_center}}, a different operation that centers the
#'   similarity matrix itself.
#' @export
centered_sim_matrix <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat))
    stop("centered_sim_matrix(): expected a numeric matrix with one row per ",
         "text (the output of embed()); got ", class(mat)[1], ".",
         call. = FALSE)
  # n = 2 では中心化した二行が必ず正反対を向き、類似度が定義上 -1 に
  # 固定される。数値は返るが意味がないので、ここで止める。
  if (nrow(mat) < 3)
    stop("centered_sim_matrix(): need at least three texts. With two, ",
         "centering forces the similarity to -1 by construction.",
         call. = FALSE)
  centered <- sweep(mat, 2, colMeans(mat), "-")
  flat <- which(sqrt(rowSums(centered^2)) == 0)
  if (length(flat) > 0)
    stop("centered_sim_matrix(): row(s) ",
         paste(head(flat, 5), collapse = ", "),
         " sit exactly at the pool mean, so their centered direction is ",
         "undefined. This usually means duplicate texts.", call. = FALSE)
  sim <- cos_sim_matrix(centered)
  attr(sim, "centering") <- "mean"
  sim
}

#' Double-center a similarity matrix
#'
#' Removes the row means, the column means and the grand mean from a
#' similarity matrix. Whereas \code{\link{centered_sim_matrix}} centers the
#' embedding, this centers the finished similarity matrix; use it to control
#' hub structure, that is, the tendency of a few items to be similar to
#' everything.
#'
#' @param sim_mat A square n by n similarity matrix, for example the output of
#'   \code{\link{cos_sim_matrix}}.
#' @return An n by n matrix of the same dimensions and dimnames whose rows and
#'   columns each sum to zero. The diagonal is no longer 1 and the entries are
#'   no longer bounded by -1 and 1, so the result is a contrast structure
#'   rather than a similarity.
#' @seealso \code{\link{cos_sim_matrix}} for the matrix this transforms, and
#'   \code{\link{centered_sim_matrix}}, which centers the embedding instead.
#' @export
double_center <- function(sim_mat) {
  if (!is.matrix(sim_mat) || nrow(sim_mat) != ncol(sim_mat))
    stop("double_center(): expected a square similarity matrix.",
         call. = FALSE)
  sim_mat - rowMeans(sim_mat)[row(sim_mat)] -
    colMeans(sim_mat)[col(sim_mat)] + mean(sim_mat)
}

#' Compute pairwise Euclidean distances between embedding rows
#'
#' A thin wrapper around \code{\link[stats]{dist}} for embedding matrices,
#' provided for the procedures that expect a distance object rather than a
#' similarity matrix, such as \code{\link{mantel_test}} or multidimensional
#' scaling.
#'
#' @param mat A numeric matrix with one row per text, as returned by
#'   \code{\link{embed}}.
#' @return An object of class \code{dist} holding the n * (n - 1) / 2 pairwise
#'   Euclidean distances, labelled by \code{rownames(mat)}.
#' @seealso \code{\link{cos_sim_matrix}} for the similarity alternative, and
#'   \code{\link{mantel_test}}, which takes square matrices and so needs
#'   \code{as.matrix()} around the result.
#' @export
euclidean_dist <- function(mat) {
  dist(mat, method = "euclidean")
}

#' Summarise within-group and between-group similarity
#'
#' Splits the pairs in the upper triangle of a similarity matrix into those
#' that share a group label and those that do not, and reports the two means
#' with their difference \eqn{\Delta}. This is the descriptive core of the
#' within-between contrast; \code{\link{test_delta}} adds a permutation test
#' around it.
#'
#' @param sim_mat A square similarity matrix, typically the output of
#'   \code{\link{cos_sim_matrix}}.
#' @param groups A vector of group labels, one per row of \code{sim_mat} and
#'   in the same order.
#' @return A list with components \code{within} (mean similarity of same-group
#'   pairs), \code{between} (mean similarity of different-group pairs),
#'   \code{delta} (\code{within - between}), \code{ratio}
#'   (\code{within / between}), \code{sd_between} (standard deviation of the
#'   between-group similarities) and \code{delta_std}
#'   (\code{delta / sd_between}).
#' @details
#' The raw \code{delta} depends on how densely the space is packed. For the
#' same texts, the mean between-group cosine can differ by a large factor
#' from one provider to another, so raw differences are not comparable across
#' models.
#' \code{delta_std} rescales the difference by the spread of the
#' between-group similarities and is the quantity to report when spaces are
#' being compared.
#' @examples
#' \dontrun{
#' s <- cos_sim_matrix(emb)
#' wb <- within_between_sim(s, groups)
#' wb$delta_std   # this is the value that is comparable across providers
#' }
#' @seealso \code{\link{test_delta}}, which adds a permutation test to these
#'   descriptives.
#' @export
within_between_sim <- function(sim_mat, groups) {
  ut   <- upper.tri(sim_mat)
  same <- outer(groups, groups, `==`)
  wi   <- mean(sim_mat[ut & same])
  bt   <- mean(sim_mat[ut & !same])
  sdb  <- stats::sd(sim_mat[ut & !same])
  list(within = wi, between = bt, delta = wi - bt, ratio = wi / bt,
       sd_between = sdb, delta_std = (wi - bt) / sdb)
}


# ── 1b. 進捗表示 ─────────────────────────────────────────────

#' Report the progress of a long-running loop
#'
#' Builds a small progress reporter for permutation tests, jackknives and
#' other loops that can run for several minutes, so that whoever reproduces an
#' analysis can tell a slow run from a stalled one. On a terminal a single
#' line is overwritten in place; when output is redirected to a file, one line
#' is appended at fixed intervals instead. No extra packages are needed.
#'
#' Work projected to finish sooner than \code{min_secs} prints nothing. The
#' projection is formed from the first few iterations, so short runs never
#' fill a log.
#'
#' @param label Character; a short name shown in brackets, for example
#'   \code{"delta perm"}.
#' @param n_total Integer; the total number of iterations expected.
#' @param min_secs Numeric; the projected running time, in seconds, below which
#'   the reporter stays silent. Defaults to 10.
#' @param every Numeric; the shortest interval, in seconds, between appended
#'   lines when the stream is not a terminal. Defaults to 15.
#' @param stream A connection to write to. Defaults to \code{stderr()}.
#' @return A list of two functions. \code{tick(k = 1L)} records that \code{k}
#'   further iterations are complete and refreshes the display; \code{done()}
#'   prints the closing line with the elapsed time, and should be called once
#'   after the loop.
#' @examples
#' \dontrun{
#' pb <- progress_ticker("jackknife", n)
#' for (i in seq_len(n)) { ...; pb$tick() }
#' pb$done()
#' }
#' @export
progress_ticker <- function(label, n_total, min_secs = 10, every = 15,
                            stream = stderr()) {
  t0 <- Sys.time(); done <- 0L; shown <- FALSE; last <- t0
  tty <- isatty(stream)
  list(
    tick = function(k = 1L) {
      done <<- done + k
      now <- Sys.time()
      el  <- as.numeric(difftime(now, t0, units = "secs"))
      if (done < 1 || el <= 0) return(invisible(NULL))
      eta <- el / done * (n_total - done)
      if (!shown && (el + eta) < min_secs) return(invisible(NULL))
      if (!tty && shown && as.numeric(difftime(now, last, units = "secs")) < every &&
          done < n_total) return(invisible(NULL))
      shown <<- TRUE; last <<- now
      cat(sprintf("%s  [%s] %d/%d (%.0f%%)%s%s",
                  if (tty) "\r" else "", label, done, n_total, 100 * done / n_total,
                  if (eta > 5) sprintf("  ETA ~%.0fs", eta) else "        ",
                  if (tty) "" else "\n"),
          file = stream)
      flush(stream)
    },
    done = function() {
      if (shown) {
        cat(sprintf("%s  [%s] %d/%d (100%%)  %.0fs elapsed        \n",
                    if (tty) "\r" else "", label, n_total, n_total,
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))), file = stream)
        flush(stream)
      }
    }
  )
}

# ── 2. 統計的推論 ───────────────────────────────────────────

# permutation p 値（片側・+1補正）
.perm_p <- function(observed, null) {
  (1 + sum(null >= observed)) / (1 + length(null))
}

#' Test the within-between contrast Delta by permutation
#'
#' Tests whether items that share a theoretical group label are more similar
#' to one another than to items of other groups. The observed
#' \eqn{\Delta = within - between} is referred to a null distribution obtained
#' by shuffling the group labels across items while the similarity matrix is
#' held fixed, so no distributional assumption about the similarities is
#' needed.
#'
#' @param sim_mat A square similarity matrix, typically the output of
#'   \code{\link{cos_sim_matrix}}.
#' @param groups A vector of group labels, one per row of \code{sim_mat} and
#'   in the same order. At least two distinct groups are required, and at
#'   least one group must have two members.
#' @param n_perm Integer; the number of label permutations. Defaults to 9999,
#'   for which the smallest attainable p value is 1e-04.
#' @return A list with the descriptive quantities of
#'   \code{\link{within_between_sim}} (\code{within}, \code{between},
#'   \code{delta}, \code{sd_between}, \code{delta_std}), plus \code{null} (the
#'   vector of \code{n_perm} permuted values of Delta), \code{p} (the
#'   one-sided permutation p value, computed as one plus the number of null
#'   values at least as large as the observed one, divided by
#'   \code{1 + n_perm}) and \code{n_perm}.
#' @seealso \code{\link{within_between_sim}} for the descriptives alone, and
#'   \code{\link{test_ari}} for the clustering counterpart.
#' @export
test_delta <- function(sim_mat, groups, n_perm = 9999) {
  .check_sim_groups(sim_mat, groups, "test_delta")
  obs <- within_between_sim(sim_mat, groups)
  # 帰無分布は上三角のペアだけを見る。n が千を超えると n x n の論理行列を
  # 毎反復で組んで添字するのが支配的になるので、ペアの行・列番号と類似度を
  # 一度だけ取り出し、以後はベクトルで回す。算術も乱数列も同じで、結果は
  # 変わらない。
  n   <- nrow(sim_mat)
  idx <- which(upper.tri(sim_mat))
  ri  <- ((idx - 1L) %% n) + 1L
  ci  <- ((idx - 1L) %/% n) + 1L
  sv  <- sim_mat[idx]
  gi  <- as.integer(factor(groups))
  pb <- progress_ticker("delta perm", n_perm)
  null <- vapply(seq_len(n_perm), function(.i) {
    g    <- sample(gi)
    same <- g[ri] == g[ci]
    v <- mean(sv[same]) - mean(sv[!same])
    pb$tick(); v
  }, numeric(1))
  pb$done()
  list(within = obs$within, between = obs$between, delta = obs$delta,
       sd_between = obs$sd_between, delta_std = obs$delta_std,
       null = null, p = .perm_p(obs$delta, null), n_perm = n_perm)
}


#' Compute the adjusted Rand index between two partitions
#'
#' Chance-corrected agreement between two partitions of the same objects
#' (Hubert and Arabie, 1985). Implemented directly rather than taken from
#' another package, so that the reproduction code carries no extra dependency
#' for a single formula.
#'
#' @param x,y Two label vectors of equal length describing the same objects in
#'   the same order. Any type accepted by \code{table} may be used, and the
#'   two partitions need not have the same number of classes.
#' @return A single number: 0 at chance level and 1 for identical partitions.
#'   Values below 0 indicate agreement worse than chance. If either partition
#'   is degenerate, so that the index is undefined, 0 is returned.
#' @seealso \code{\link{test_ari_sim}} and \code{\link{test_ari}}, which refer
#'   this index to a permutation null.
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

#' Cluster a similarity matrix and test its agreement with theory
#'
#' Turns a similarity matrix into distances (\code{1 - similarity}), clusters
#' them with Ward's method (\code{ward.D2}), cuts the tree into \code{k}
#' groups, and scores the cut against a theoretical grouping with the adjusted
#' Rand index. The null distribution is generated by shuffling the theoretical
#' labels.
#'
#' Any similarity matrix may be supplied, not only embedding cosines: the
#' matrix of absolute correlations among observed responses can be sent
#' through exactly the same pipeline, which is what makes the comparison
#' between an embedding space and a response space symmetric.
#'
#' @param sim_mat A square, symmetric similarity matrix with ones on the
#'   diagonal.
#' @param groups A vector of theoretical group labels, one per row of
#'   \code{sim_mat} and in the same order.
#' @param k Integer; the number of clusters to cut. Defaults to the number of
#'   distinct labels in \code{groups} and must lie between 2 and
#'   \code{nrow(sim_mat) - 1}.
#' @param n_perm Integer; the number of label permutations. Defaults to 9999.
#' @return A list with \code{ari} (the observed adjusted Rand index),
#'   \code{p} (the one-sided permutation p value), \code{null} (the permuted
#'   indices, returned so that the observed value can be plotted against
#'   them), \code{clusters} (the cluster membership of each item),
#'   \code{table} (the cluster by group cross-tabulation), \code{hclust} (the
#'   fitted \code{\link[stats]{hclust}} object) and \code{n_perm}.
#' @seealso \code{\link{test_ari}} to start from an embedding matrix, and
#'   \code{\link{adjusted_rand_index}} for the index itself.
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
  pb <- progress_ticker("ARI perm", n_perm)
  null <- vapply(seq_len(n_perm), function(.i) {
    v <- adjusted_rand_index(cl, sample(groups)); pb$tick(); v }, numeric(1))
  pb$done()
  # 帰無分布そのものも返す。観測値と並べて図に示せるようにするため。
  list(ari = obs, p = .perm_p(obs, null), null = null,
       clusters = cl, table = table(cluster = cl, group = groups),
       hclust = hc, n_perm = n_perm)
}

#' Cluster an embedding and test its agreement with theory
#'
#' Computes the cosine similarity matrix of an embedding with
#' \code{\link{cos_sim_matrix}} and passes it to \code{\link{test_ari_sim}}.
#' Use this function when starting from an embedding matrix, and
#' \code{\link{test_ari_sim}} when starting from a similarity matrix of some
#' other kind, such as correlations among observed responses.
#'
#' @param mat A numeric matrix with one row per text, as returned by
#'   \code{\link{embed}}.
#' @param groups A vector of theoretical group labels, one per row of
#'   \code{mat} and in the same order.
#' @param k Integer; the number of clusters to cut. Defaults to the number of
#'   distinct labels in \code{groups}.
#' @param n_perm Integer; the number of label permutations. Defaults to 9999.
#' @return The list returned by \code{\link{test_ari_sim}}, with components
#'   \code{ari}, \code{p}, \code{null}, \code{clusters}, \code{table},
#'   \code{hclust} and \code{n_perm}.
#' @seealso \code{\link{test_ari_sim}} to start from a similarity matrix, and
#'   \code{\link{adjusted_rand_index}} for the index itself.
#' @export
test_ari <- function(mat, groups, k = length(unique(groups)), n_perm = 9999) {
  # 類似度行列を渡されても cos_sim_matrix() は素通しし、類似度の類似度を
  # 取って別の答えを返してしまう。エラーにならず値が変わるのが最悪なので、
  # 対称・対角 1・正方という類似度行列の特徴で捕まえて test_ari_sim() へ送る。
  if (is.matrix(mat) && nrow(mat) == ncol(mat) && nrow(mat) > 1 &&
      isTRUE(all.equal(unname(diag(mat)), rep(1, nrow(mat)),
                       tolerance = 1e-8)) &&
      isTRUE(all.equal(mat, t(mat), check.attributes = FALSE)))
    stop("test_ari(): `mat` looks like a similarity matrix (square, ",
         "symmetric, ones on the diagonal), not an embedding. Use ",
         "test_ari_sim() for a similarity matrix, or pass the embedding ",
         "matrix here.", call. = FALSE)
  test_ari_sim(cos_sim_matrix(mat), groups, k = k, n_perm = n_perm)
}

#' Test the agreement of two similarity structures by Mantel correlation
#'
#' Correlates the off-diagonal entries of two distance or similarity matrices
#' defined over the same items, and tests that correlation by permuting the
#' rows and columns of one matrix (Mantel, 1967). A wrapper around
#' \code{\link[vegan]{mantel}} with Pearson correlation. This is the primary
#' statistic for cross-space agreement in the accompanying paper, because it
#' compares the structures directly and requires no choice of
#' dimensionality.
#'
#' @param m1 A square n by n distance or similarity matrix.
#' @param m2 A second square matrix of the same size, describing the same
#'   items in the same row and column order. When both matrices carry row
#'   names, the names must be identical; reorder one of them if they are not,
#'   for example with \code{m2[rownames(m1), rownames(m1)]}.
#' @param n_perm Integer; the number of permutations. Defaults to 9999.
#' @return A list with \code{r} (the Mantel correlation, denoted r_M in the
#'   paper), \code{p} (the permutation p value) and \code{n_perm}.
#' @seealso \code{\link{procrustes_m2}} for the supplementary, descriptive
#'   comparison, and \code{\link{euclidean_dist}}, whose \code{dist} object
#'   has to be passed through \code{as.matrix()} first.
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

#' Compare two spaces by Procrustes fit with a PROTEST permutation test
#'
#' Compares the configuration of the same items in two embedding spaces. The
#' spaces may have different dimensionalities, so each is first reduced to
#' \code{k} coordinates; a symmetric Procrustes fit then returns the residual
#' sum of squares \eqn{m^2} (Gower, 1975), and PROTEST supplies a permutation
#' p value (Jackson, 1995). Reported as supplementary, descriptive evidence
#' alongside \code{\link{mantel_test}}, without any pass-or-fail threshold.
#'
#' @param X A numeric embedding matrix for n items.
#' @param Y A second embedding matrix for the same n items in the same row
#'   order; its number of columns may differ from that of \code{X}.
#' @param k Integer; the number of dimensions retained before the fit.
#'   Defaults to 5 and is capped at \code{min(ncol(X), ncol(Y), nrow(X) - 1)}.
#'   The default is a convenience, not a recommendation: \code{m2} is computed
#'   on however many dimensions you retain, and that number is a choice the
#'   similarity matrix does not require. Report how far the value moves across
#'   it with \code{\link{procrustes_sensitivity}}, and let
#'   \code{\link{mantel_test}} carry the verdict.
#' @param n_perm Integer; the number of PROTEST permutations. Defaults to
#'   9999; set to 0 to skip the test, in which case \code{p} is \code{NA}.
#' @param layout Character; how the k coordinates are formed, either
#'   \code{"pca"} (the metric reduction fixed in the Method of the accompanying
#'   paper) or \code{"mds"}. Defaults to \code{"pca"}.
#' @return A list with \code{m2} (the Procrustes residual sum of squares, 0
#'   for perfect agreement and 1 for none), \code{corr}
#'   (\code{sqrt(1 - m2)}), \code{p} (the PROTEST p value, or \code{NA} when
#'   \code{n_perm = 0}), \code{residuals} (the per-item residuals in
#'   decreasing order, named after the rows of \code{X}, which identify the
#'   items that fit worst), \code{k}, \code{n_perm} and \code{layout}.
#' @seealso \code{\link{mantel_test}}, the primary statistic for cross-space
#'   agreement, and \code{\link{procrustes_sensitivity}} for the fit across a
#'   range of \code{k}.
#' @export
procrustes_m2 <- function(X, Y, k = 5, n_perm = 9999,
                          layout = c("pca", "mds")) {
  stopifnot(nrow(X) == nrow(Y))
  if (!is.null(rownames(X)) && !is.null(rownames(Y)))
    stopifnot(identical(rownames(X), rownames(Y)))
  layout <- match.arg(layout)
  k  <- min(k, ncol(X), ncol(Y), nrow(X) - 1)
  Xk <- .layout_kd(X, k, layout)
  Yk <- .layout_kd(Y, k, layout)
  pro <- vegan::procrustes(Xk, Yk, symmetric = TRUE)
  p <- NA_real_
  if (n_perm > 0) {
    pt <- vegan::protest(Xk, Yk, permutations = n_perm)
    p  <- pt$signif
  }
  resid <- stats::residuals(pro)
  names(resid) <- rownames(X)
  list(m2 = pro$ss, corr = sqrt(1 - pro$ss), p = p,
       residuals = sort(resid, decreasing = TRUE), k = k, n_perm = n_perm,
       layout = layout)
}

#' Trace the sensitivity of the Procrustes fit to the number of dimensions
#'
#' Repeats \code{\link{procrustes_m2}} over a range of \code{k} so that the
#' agreement between two spaces can be read as a curve rather than as a single
#' number that depends on an arbitrary choice of dimensionality. The
#' permutation test is skipped throughout, since only the shape of the curve
#' is at issue.
#'
#' @param X A numeric embedding matrix for n items.
#' @param Y A second embedding matrix for the same n items in the same row
#'   order.
#' @param ks An integer vector of dimensionalities to try. Defaults to
#'   \code{2:10} and is silently truncated at
#'   \code{min(ncol(X), ncol(Y), nrow(X) - 1)}.
#' @param layout Character; how the coordinates are formed, either \code{"pca"}
#'   or \code{"mds"}; passed to \code{\link{procrustes_m2}}. Defaults to
#'   \code{"pca"}.
#' @return A data frame with one row per retained value of \code{k} and
#'   columns \code{k}, \code{m2} and \code{layout}.
#' @seealso \code{\link{procrustes_m2}}, the single-\code{k} fit this repeats,
#'   and \code{\link{mantel_test}}, which needs no choice of dimensionality.
#' @export
procrustes_sensitivity <- function(X, Y, ks = 2:10,
                                   layout = c("pca", "mds")) {
  layout <- match.arg(layout)
  ks <- ks[ks <= min(ncol(X), ncol(Y), nrow(X) - 1)]
  data.frame(
    k  = ks,
    m2 = vapply(ks, function(k)
      procrustes_m2(X, Y, k = k, n_perm = 0, layout = layout)$m2, numeric(1)),
    layout = layout
  )
}


# ── 2b. 効果量の不確かさ ─────────────────────────────────────
#
# 区間推定には標本モデルが要る。研究を繰り返したとき何が変わるのかが
# 材料によって違うので、同じ手続きを全部に当てると、標本でないものを
# 標本として扱うことになる。本稿は「埋め込みは決定論的写像の像であって
# 確率変数の実現ではない」という立場を取るので、区間の出し方も材料で
# 分ける。人が単位（回答者・評定者）なら jackknife_ci()、項目が単位
# （器具そのもの）なら loo_range()。どちらも統計量を関数 theta として
# 受けるので、Delta でも ARI でも Mantel r_M でも射影相関でも使える。

#' Compute the leave-one-out range of a statistic (not a confidence interval)
#'
#' Recomputes a statistic n times, each time omitting one unit, and reports
#' the smallest and largest value obtained. Intended for materials whose unit
#' is the item: the items of an instrument are not drawn from a population, so
#' a confidence interval would not mean anything, and the answerable question
#' is instead whether any single item is carrying the result.
#'
#' @param n Integer; the number of units (items).
#' @param theta A function that takes an integer vector of retained indices
#'   and returns a single number, such as a Delta, an adjusted Rand index, a
#'   Mantel correlation or a projection correlation.
#' @param label Character; the name shown by the progress reporter. Defaults
#'   to \code{"leave-one-out"}.
#' @return A named numeric vector with \code{obs} (the statistic computed on
#'   all units), \code{lo} and \code{hi} (the minimum and maximum over the n
#'   leave-one-out recomputations) and \code{n_unit} (equal to \code{n}).
#'   \code{lo} and \code{hi} bound the observed swing and must not be read as
#'   a confidence interval.
#' @seealso \code{\link{jackknife_ci}} for materials whose unit is a person.
#' @export
loo_range <- function(n, theta, label = "leave-one-out") {
  obs <- theta(seq_len(n))
  pb  <- progress_ticker(label, n)
  v   <- vapply(seq_len(n), function(k) {
    x <- theta(setdiff(seq_len(n), k)); pb$tick(); x }, numeric(1))
  pb$done()
  c(obs = obs, lo = min(v), hi = max(v), n_unit = n)
}

#' Compute a jackknife confidence interval for materials whose unit is a person
#'
#' Delete-one jackknife interval for statistics computed over respondents or
#' raters. Severiano et al. (2011) report better coverage than the bootstrap
#' for pairwise agreement indices, because resampling with replacement draws
#' the same individual twice and inflates agreement. Supply \code{blocks} to
#' delete groups of observations together, for example several narratives
#' written by the same respondent.
#'
#' @param n Integer; the number of observations.
#' @param theta A function that takes an integer vector of retained indices
#'   and returns a single number.
#' @param blocks A vector of length \code{n} giving the deletion unit of each
#'   observation; observations sharing a value are dropped together. Defaults
#'   to \code{seq_len(n)}, that is, one observation per block.
#' @param level Numeric; the confidence level. Defaults to 0.95.
#' @param cores Integer; the number of forked worker processes across which the
#'   blocks are evaluated. Unix-alikes only: on Windows, and with
#'   \code{cores = 1}, the run falls back to a sequential loop. Worth setting
#'   when \code{theta} is expensive; progress is not reported from the
#'   workers, only the start and the end. Defaults to 1.
#' @param label Character; the name shown by the progress reporter. Defaults
#'   to \code{"jackknife"}.
#' @return A named numeric vector with \code{obs} (the statistic computed on
#'   all observations), \code{lo} and \code{hi} (the normal-theory interval
#'   formed from the jackknife pseudo-values) and \code{n_unit} (the number of
#'   blocks, which is the effective sample size of the interval).
#' @seealso \code{\link{loo_range}} for materials whose unit is the item, and
#'   \code{\link{cor_jackknife}} for statistics of a correlation matrix.
#' @export
jackknife_ci <- function(n, theta, blocks = seq_len(n), level = .95,
                         cores = 1L, label = "jackknife") {
  obs <- theta(seq_len(n))
  bl  <- unname(split(seq_len(n), blocks)); nb <- length(bl)
  par_ok <- cores > 1L && .Platform$OS.type == "unix"
  if (par_ok) {
    # 子プロセスからは進捗を更新できないので、開始と終了だけ知らせる
    cat(sprintf("  %s: %d blocks on %d cores...", label, nb, cores))
    jk <- unlist(parallel::mclapply(seq_along(bl),
      function(k) theta(unlist(bl[-k])), mc.cores = cores), use.names = FALSE)
    cat(" done\n")
    if (length(jk) != nb || anyNA(jk))
      stop("the parallel run returned nothing; re-run with cores = 1.")
  } else {
    pb <- progress_ticker(label, nb)
    jk <- vapply(seq_along(bl), function(k) {
      v <- theta(unlist(bl[-k])); pb$tick(); v }, numeric(1))
    pb$done()
  }
  ps  <- nb * obs - (nb - 1) * jk
  se  <- stats::sd(ps) / sqrt(nb); z <- stats::qnorm(1 - (1 - level) / 2)
  c(obs = obs, lo = mean(ps) - z * se, hi = mean(ps) + z * se, n_unit = nb)
}

#' Jackknife a statistic of a correlation matrix
#'
#' Delete-one jackknife over observations for any statistic computed from a
#' correlation matrix, without recomputing the correlations. Passing a
#' correlation matrix to \code{\link{jackknife_ci}} would call \code{cor} once
#' per deleted observation; because a correlation can be rebuilt from four
#' running sums (pairwise counts, cross products, sums and sums of squares),
#' deleting an observation only requires subtracting its contribution. The
#' point estimate matches full recomputation to within a bit or two of
#' double precision, the interval bounds to within about 1e-14, which the
#' package's own tests hold fixed.
#'
#' @param X A numeric matrix of observations by variables, or anything
#'   \code{as.matrix} accepts. Missing values are handled pairwise.
#' @param stat A function that takes a correlation matrix and returns a single
#'   number.
#' @param level Numeric; the confidence level. Defaults to 0.95.
#' @return A named numeric vector with \code{obs}, \code{lo}, \code{hi} and
#'   \code{n_unit} (the number of observations), as returned by
#'   \code{\link{jackknife_ci}}.
#' @seealso \code{\link{jackknife_ci}} for the general case, and
#'   \code{\link{loo_range}} for materials whose unit is the item.
#' @details
#' Downdating subtracts from large running sums, so precision can be lost when
#' a column spans an extreme range of values or has a variance close to
#' machine epsilon. Bounded integer scales such as psychological ratings are
#' unaffected. When in doubt, check the result against
#' \code{\link{jackknife_ci}}.
#' @export
cor_jackknife <- function(X, stat, level = .95) {
  X <- as.matrix(X)
  M <- !is.na(X); Z <- X; Z[!M] <- 0; Mn <- M + 0
  N <- crossprod(Mn); Sxy <- crossprod(Z)
  Sx <- crossprod(Z, Mn); Sxx <- crossprod(Z^2, Mn)
  rebuild <- function(N, Sxy, Sx, Sxx) {
    v <- N * Sxx - Sx^2
    r <- (N * Sxy - Sx * t(Sx)) / sqrt(v * t(v))
    diag(r) <- 1; r
  }
  n   <- nrow(X)
  obs <- stat(rebuild(N, Sxy, Sx, Sxx))
  pb  <- progress_ticker("correlation jackknife", n)
  jk  <- vapply(seq_len(n), function(k) {
    m <- Mn[k, ]; z <- Z[k, ]
    v <- stat(rebuild(N - outer(m, m), Sxy - outer(z, z),
                      Sx - outer(z, m), Sxx - outer(z^2, m)))
    pb$tick(); v }, numeric(1))
  pb$done()
  ps <- n * obs - (n - 1) * jk
  se <- stats::sd(ps) / sqrt(n); zq <- stats::qnorm(1 - (1 - level) / 2)
  c(obs = obs, lo = mean(ps) - zq * se, hi = mean(ps) + zq * se, n_unit = n)
}

# ── 3. Semantic projection（Grand et al., 2022）─────────────

#' Project items onto an axis defined by two sets of anchors
#'
#' Implements the semantic projection of Grand et al. (2022). The axis is the
#' difference between the centroids of a high-pole and a low-pole set of
#' anchor texts, and each item is scored by the length of its projection onto
#' that axis. Use it when theory names the dimension to be measured, instead
#' of letting a component analysis choose one.
#'
#' The axis is \code{a = colMeans(high_mat) - colMeans(low_mat)} and the score
#' of an item \code{x} is \code{sum(x * a) / sqrt(sum(a^2))}.
#'
#' @param item_mat A numeric embedding matrix of the items to be scored, one
#'   row per item.
#' @param high_mat An embedding matrix of the anchor texts marking the high
#'   pole, with at least one row.
#' @param low_mat An embedding matrix of the anchor texts marking the low
#'   pole, with at least one row. All three matrices must come from the same
#'   provider and model, so that they share the same number of columns, and
#'   the two anchor centroids must not coincide.
#' @return A named numeric vector of projection scores, one per row of
#'   \code{item_mat} and named by \code{rownames(item_mat)}. The scores are
#'   neither centered nor normalised, so their ordering and relative spacing
#'   are interpretable but their absolute level is not.
#' @examples
#' # Choose the two poles yourself: a handful of words or phrases that differ
#' # on the axis you mean and on as little else as possible. Embed them with
#' # the items, then subset the matrix. Keep drop = FALSE, or a one-word pole
#' # would stop being a matrix.
#' high <- c("happy", "pleased", "delighted")
#' low  <- c("sad", "unhappy", "miserable")
#' \dontrun{
#' items <- c("cheerful", "downcast", "content")
#' e <- embed(c(items, high, low), provider = "gemini")
#' semantic_projection(e[items, ],
#'                     e[high, , drop = FALSE],
#'                     e[low, , drop = FALSE])
#' }
#' @seealso \code{\link{coords_2d}}, whose \code{axis} argument takes the same
#'   anchor contrast, and \code{\link{plot_arc}}, which projects segments onto
#'   it.
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

#' Compute two-dimensional display coordinates for an embedding matrix
#'
#' @description
#' Reduces an embedding matrix to a plane for plotting. The default layout is
#' non-metric multidimensional scaling (MDS), which takes the rank order of
#' distances as its loss function; \code{layout = "pca"} gives the metric
#' alternative. Use this to produce the coordinates that
#' \code{\link{plot_embedding_2d}} and \code{\link{plot_bilingual}} draw.
#'
#' @details
#' The choice is not "PCA or MDS". For Euclidean distances, classical (metric)
#' MDS and PCA return the same configuration, agreeing to numerical precision.
#' What is being chosen is metric versus non-metric, and a single rule settles
#' it: match the loss function of the reduction to what the downstream reader
#' uses. A scatter plot is read as the rank order of distances, so displays use
#' non-metric MDS; Procrustes m2 is a metric criterion (squared distance after
#' a similarity transform), which is why \code{\link{procrustes_m2}} keeps PCA
#' as its default.
#'
#' PCA maximises variance rather than preserving distances, so when the
#' embedding dimensionality far exceeds the number of points, little variance
#' reaches the first two components and the plane flattens the configuration.
#' \code{\link{trajectory_fidelity}} reports, for your own data, how well the
#' plotted distances track the measured ones.
#'
#' @param mat A numeric matrix with one row per text (n items) and one column
#'   per embedding dimension (d), as returned by \code{\link{embed}}. Row names
#'   are carried through as point labels.
#' @param layout Character; either \code{"mds"} (non-metric MDS via
#'   \code{MASS::isoMDS}) or \code{"pca"}. MDS falls back to PCA when there are
#'   fewer than four rows, when \pkg{MASS} is not installed, or when the
#'   distances are degenerate; the layout actually used is reported in the
#'   result. Defaults to \code{"mds"}.
#' @param axis An optional numeric vector of length d giving a pre-specified
#'   direction in the embedding space, for instance the difference between the
#'   centroids of two anchor sets (the axis \code{\link{semantic_projection}}
#'   scores texts on, not the scores it returns). When supplied, the
#'   configuration is
#'   rotated so that the x axis lies as close as possible to that direction.
#'   Rotation leaves distances unchanged, so fidelity is unaffected and only
#'   the axis becomes interpretable. Defaults to \code{NULL} (no rotation).
#' @param scale Logical; whether to standardise each embedding dimension before
#'   the decomposition, which applies only when \code{layout = "pca"}. Defaults
#'   to \code{FALSE}: standardising gives every embedding dimension the same
#'   weight, which alters the space rather than the display of it.
#' @return A list with four elements: \code{df}, a data frame of coordinates
#'   with columns \code{PC1}, \code{PC2} and \code{label} (the column names are
#'   kept for backward compatibility whichever layout is used); \code{ve}, the
#'   percentage of variance explained by each of the two components, or
#'   \code{c(NA, NA)} under MDS; \code{layout}, the layout actually used
#'   (\code{"mds"} or \code{"pca"}); and \code{lab}, a length-2 character
#'   vector of axis labels for the plot.
#' @seealso \code{\link{plot_embedding_2d}} to draw the result, and
#'   \code{\link{pca_2d}}, \code{\link{tsne_2d}} and \code{\link{umap_2d}} for
#'   the other reductions.
#' @export
coords_2d <- function(mat, layout = c("mds", "pca"), axis = NULL,
                      scale = FALSE) {
  layout <- match.arg(layout)
  if (layout == "pca" && isTRUE(scale)) {
    pc <- stats::prcomp(mat, scale. = TRUE)
    xy <- pc$x[, 1:2, drop = FALSE]
    ve <- round(summary(pc)$importance[2, 1:2] * 100, 1)
    used <- "pca"
  } else {
    xy   <- .layout_2d(mat, layout, axis)
    used <- attr(xy, "layout")
    v    <- attr(xy, "ve")
    ve   <- if (is.na(v)) c(NA_real_, NA_real_) else NULL
    if (is.null(ve)) {
      pc <- stats::prcomp(mat)
      ve <- round(pc$sdev[1:2]^2 / sum(pc$sdev^2) * 100, 1)
    }
  }
  df <- as.data.frame(xy[, 1:2, drop = FALSE])
  colnames(df) <- c("PC1", "PC2")     # 列名は後方互換のため据え置き
  df$label <- rownames(mat)
  lab <- if (!is.null(axis)) c("Anchor axis", .ax_lab(used, 2))
         else c(.ax_lab(used, 1), .ax_lab(used, 2))
  list(df = df, ve = unname(ve), layout = used, lab = lab)
}

#' Compute two-dimensional display coordinates by PCA
#'
#' @description
#' Wrapper for \code{coords_2d(mat, layout = "pca")}, kept so that earlier
#' scripts keep running. New code should call \code{\link{coords_2d}} directly:
#' its default non-metric MDS layout preserves the original distances better in
#' high-dimensional embedding spaces.
#'
#' @param mat A numeric matrix with one row per text (n items) and one column
#'   per embedding dimension (d), as returned by \code{\link{embed}}.
#' @param scale Logical; whether to standardise each embedding dimension before
#'   the decomposition. Defaults to \code{FALSE}.
#' @return The same list as \code{coords_2d(layout = "pca")}, with elements
#'   \code{df}, \code{ve}, \code{layout} and \code{lab}.
#' @seealso \code{\link{coords_2d}}, which this wraps.
#' @export
pca_2d <- function(mat, scale = FALSE) coords_2d(mat, "pca", scale = scale)

#' Compute two-dimensional display coordinates by PCA (former name of pca_2d)
#'
#' @description
#' Alias for \code{\link{pca_2d}}, retained for scripts written against the
#' earlier API. Note that \code{scale} now defaults to \code{FALSE} here too, so
#' this name no longer standardises the embedding dimensions by default.
#'
#' @param mat A numeric matrix with one row per text, as returned by
#'   \code{\link{embed}}.
#' @param scale Logical; whether to standardise each embedding dimension before
#'   the decomposition. Defaults to \code{FALSE}.
#' @return The same list as \code{\link{pca_2d}}.
#' @seealso \code{\link{pca_2d}}, which this aliases, and
#'   \code{\link{coords_2d}} for new code.
#' @export
pca_coords <- pca_2d

#' Compute two-dimensional display coordinates by t-SNE
#'
#' @description
#' Re-derives the map with t-SNE as a robustness check on the layout returned
#' by \code{\link{coords_2d}}: structure that survives a second, very different
#' reduction is less likely to be an artefact of the first. Requires the
#' \pkg{Rtsne} package.
#'
#' @param mat A numeric matrix with one row per text, as returned by
#'   \code{\link{embed}}. Row names are used as point labels.
#' @param perplexity Numeric; the t-SNE perplexity. Defaults to \code{NULL},
#'   which uses \code{max(2, floor((nrow(mat) - 1) / 3))}.
#' @param seed Integer; the random seed. t-SNE is stochastic, so fixing the
#'   seed is what makes the map reproducible. Defaults to 2026.
#' @return A list with \code{df}, a data frame with columns \code{PC1},
#'   \code{PC2} and \code{label}, and \code{ve}, which is \code{c(NA, NA)}
#'   because t-SNE has no variance-explained quantity. The coordinate columns
#'   are named \code{PC1}/\code{PC2} so that the result can be passed directly
#'   to \code{\link{plot_embedding_2d}}.
#' @seealso \code{\link{coords_2d}} for the default layout, and
#'   \code{\link{umap_2d}} for the other robustness check.
#' @export
tsne_2d <- function(mat, perplexity = NULL, seed = 2026) {
  if (!requireNamespace("Rtsne", quietly = TRUE))
    stop("tsne_2d requires Rtsne: install.packages('Rtsne')")
  set.seed(seed)
  if (is.null(perplexity)) perplexity <- max(2, floor((nrow(mat) - 1) / 3))
  fit <- Rtsne::Rtsne(mat, dims = 2, perplexity = perplexity,
                      pca = FALSE, check_duplicates = FALSE)
  df <- as.data.frame(fit$Y)
  colnames(df) <- c("PC1", "PC2")
  df$label <- rownames(mat)
  list(df = df, ve = c(NA_real_, NA_real_))
}

#' Compute two-dimensional display coordinates by UMAP
#'
#' @description
#' Re-derives the map with UMAP as a robustness check on the layout returned by
#' \code{\link{coords_2d}}, in the same role as \code{\link{tsne_2d}}. Requires
#' the \pkg{uwot} package.
#'
#' @param mat A numeric matrix with one row per text, as returned by
#'   \code{\link{embed}}. Row names are used as point labels.
#' @param n_neighbors Integer; the size of the local neighbourhood UMAP uses.
#'   Defaults to \code{NULL}, which uses \code{max(2, min(15, nrow(mat) - 1))}.
#' @param seed Integer; the random seed. UMAP is stochastic, so fixing the
#'   seed is what makes the map reproducible. Defaults to 2026.
#' @return A list with \code{df}, a data frame with columns \code{PC1},
#'   \code{PC2} and \code{label}, and \code{ve}, which is \code{c(NA, NA)}
#'   because UMAP has no variance-explained quantity. The coordinate columns
#'   are named \code{PC1}/\code{PC2} so that the result can be passed directly
#'   to \code{\link{plot_embedding_2d}}.
#' @seealso \code{\link{coords_2d}} for the default layout, and
#'   \code{\link{tsne_2d}} for the other robustness check.
#' @export
umap_2d <- function(mat, n_neighbors = NULL, seed = 2026) {
  if (!requireNamespace("uwot", quietly = TRUE))
    stop("umap_2d requires uwot: install.packages('uwot')")
  set.seed(seed)
  if (is.null(n_neighbors)) n_neighbors <- max(2, min(15, nrow(mat) - 1))
  fit <- uwot::umap(mat, n_components = 2, n_neighbors = n_neighbors)
  df <- as.data.frame(fit)
  colnames(df) <- c("PC1", "PC2")
  df$label <- rownames(mat)
  list(df = df, ve = c(NA_real_, NA_real_))
}


# ── 5. 可視化 ───────────────────────────────────────────────

#' Plot items as points in a two-dimensional semantic map
#'
#' @description
#' Draws the coordinates produced by \code{\link{coords_2d}},
#' \code{\link{pca_2d}}, \code{\link{tsne_2d}} or \code{\link{umap_2d}} as a
#' labelled scatter plot, optionally coloured by a grouping variable. Point
#' labels are placed with \pkg{ggrepel}, so items that fall close together stay
#' readable.
#'
#' @details
#' Variance explained is a property of PCA, so the subtitle reporting it is
#' printed only when the coordinates came from a PCA layout; under MDS it is
#' omitted rather than shown as a misleading number. Axis labels follow the
#' layout as well (\code{PC1}/\code{PC2} for PCA, \code{Dimension 1}/
#' \code{Dimension 2} for MDS).
#'
#' @param proj Either the list returned by \code{\link{coords_2d}} and its
#'   relatives, or a plain data frame with columns \code{PC1}, \code{PC2} and
#'   \code{label}. A data frame is treated as a PCA layout with unknown
#'   variance explained.
#' @param labels An optional character vector of point labels, overriding the
#'   \code{label} column in \code{proj}. Defaults to \code{NULL}.
#' @param groups An optional vector of group labels, one per point, used to
#'   colour the points. Defaults to \code{NULL} (a single colour).
#' @param title Character; the plot title. Defaults to \code{NULL}.
#' @param size Numeric; the point size passed to \code{geom_point()}. Defaults
#'   to 3.5.
#' @return A \pkg{ggplot2} object (printed when called at the top level): one
#'   labelled point per item, coloured by \code{groups} where supplied, with
#'   axis labels following the layout and, under PCA, a subtitle reporting the
#'   variance explained. It can be modified further before printing or saving
#'   with \code{\link{save_fig}}.
#' @seealso \code{\link{coords_2d}} for the coordinates it draws, and
#'   \code{\link{plot_bilingual}} for the two-language display.
#' @export
plot_embedding_2d <- function(proj, labels = NULL, groups = NULL,
                              title = NULL, size = 3.5) {
  df  <- if (is.data.frame(proj)) proj else proj$df
  ve  <- if (is.data.frame(proj)) c(NA_real_, NA_real_) else proj$ve
  lab <- if (is.data.frame(proj) || is.null(proj$lab)) c("PC1", "PC2")
         else proj$lab
  lay <- if (is.data.frame(proj)) "pca" else (proj$layout %||% "pca")
  if (!is.null(labels)) df$label <- labels
  if (!is.null(groups)) df$group <- groups
  # 分散説明率は PCA の量である。MDS の軸に対して刷ると読者を誤らせるので、
  # 配置が MDS のときは出さない。
  subtitle <- if (identical(lay, "pca") && !anyNA(ve))
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
         x = lab[1], y = lab[2], color = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15),
      plot.subtitle    = element_text(color = "grey45", size = 11),
      panel.grid.minor = element_blank(),
      legend.position  = "right"
    )
  p
}

#' Plot English and Japanese semantic maps side by side
#'
#' @description
#' Draws two faceted panels, one per language, from coordinates computed
#' separately in each language. Use it to show readers whether the same
#' theoretical structure appears in both languages.
#'
#' @details
#' Because each language is reduced independently, the two panels do not share
#' a coordinate system: positions cannot be compared point by point across
#' panels, and neither can the axes. The panel scales are therefore free, and
#' the quantitative cross-language comparison belongs to
#' \code{\link{procrustes_m2}} and \code{\link{mantel_test}} instead.
#'
#' @param df_en,df_ja Data frames of coordinates for the English and Japanese
#'   panels, each the \code{df} element of a \code{\link{coords_2d}} result
#'   (columns \code{PC1}, \code{PC2} and \code{label}), with any column named
#'   by \code{color_var} added by the caller. Both must contain the same
#'   columns. The axes are labelled \code{PC1} and \code{PC2} whatever layout
#'   produced them.
#' @param ve_en,ve_ja Numeric vectors of the variance explained in each panel,
#'   as returned in the \code{ve} element of \code{\link{coords_2d}}. Accepted
#'   for API compatibility; the panels do not print these values, because the
#'   two reductions are not on a common scale. Default to \code{NULL}.
#' @param color_var Character; the name of the column in \code{df_en} and
#'   \code{df_ja} used to colour the points, as a length-one string. Defaults
#'   to \code{NULL} (a single colour).
#' @param title Character; the plot title. Defaults to \code{NULL}.
#' @return A \pkg{ggplot2} object (printed when called at the top level): one
#'   facet per language, each holding the labelled points of that language on
#'   free scales, coloured by \code{color_var} where supplied.
#' @seealso \code{\link{plot_embedding_2d}} for a single panel, and
#'   \code{\link{coords_2d}} for the coordinates it draws.
#' @export
plot_bilingual <- function(df_en, df_ja, ve_en = NULL, ve_ja = NULL,
                            title = NULL, color_var = NULL) {
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
  p
}

#' Plot a similarity matrix as a heatmap
#'
#' @description
#' Renders a square similarity matrix, typically from
#' \code{\link{cos_sim_matrix}}, as a tiled heatmap. Reordering the rows and
#' columns by theoretical grouping is what makes block structure visible, so
#' \code{order} is usually worth supplying.
#'
#' @details
#' The diverging colour scale is centered on the mean of the off-diagonal
#' similarities rather than on zero. Embedding cosines are usually positive and
#' occupy a narrow band, so a zero-centered scale would render every cell the
#' same colour.
#'
#' @param sim_mat A square similarity matrix, typically the output of
#'   \code{\link{cos_sim_matrix}}, with matching row and column names; the
#'   names are used to label the axes.
#' @param order A character vector of row and column names giving the display
#'   order. May be a subset, in which case only those rows and columns are
#'   drawn. Defaults to \code{rownames(sim_mat)}, that is, the order already in
#'   the matrix.
#' @param title Character; the plot title. Defaults to
#'   \code{"Cosine similarity"}.
#' @param legend_name Character; the title of the colour legend. Defaults to
#'   \code{"Cosine\\nsimilarity"}.
#' @return A \pkg{ggplot2} object (printed when called at the top level): one
#'   tile per pair on a blue-white-red diverging scale centered at the mean of
#'   the off-diagonal similarities, with the row and column names on both axes
#'   in the order given by \code{order}.
#' @seealso \code{\link{cos_sim_matrix}} for the matrix this displays, and
#'   \code{\link{save_fig}} to write the result to a file.
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
  p
}


# ── 6. 結果の記録・保存 ─────────────────────────────────────

#' Save a plot to a PNG file
#'
#' @description
#' Writes a \pkg{ggplot2} object to \code{dir/name.png} on a white background,
#' creating the directory if it does not yet exist, and reports the path it
#' wrote.
#'
#' @param plot A \pkg{ggplot2} object.
#' @param name Character; the file name without the extension, to which
#'   \code{.png} is appended.
#' @param dir Character; the output directory, created recursively if missing.
#'   Defaults to \code{"figures"}.
#' @param w Numeric; the width in inches. Defaults to 11.
#' @param h Numeric; the height in inches. Defaults to 7.5.
#' @param dpi Numeric; the resolution in dots per inch. Defaults to 180.
#' @return The path written, invisibly.
#' @seealso \code{\link{plot_embedding_2d}} and the other plotting functions,
#'   whose results this writes to disk.
#' @export
save_fig <- function(plot, name, dir = "figures",
                     w = 11, h = 7.5, dpi = 180) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, paste0(name, ".png"))
  ggsave(path, plot, width = w, height = h, dpi = dpi, bg = "white")
  message("saved: ", path)
  invisible(path)
}

#' Archive embedding matrices for reproduction
#'
#' Reproducibility rests on these matrices rather than on re-fetching from the
#' API, so the archive has to record what was embedded. It warns in the two
#' cases where it would not: a matrix that has lost the \code{texts} attribute
#' \code{\link{embed}} attaches (subsetting drops it), and two matrices sharing
#' a name (name-based lookup would return only the first, leaving the other
#' unreachable). Both failures are silent otherwise, and both make the archive
#' impossible to trace back to its inputs.
#'
#' @param emb_list A named list of matrices from \code{\link{embed}} (a bare
#'   matrix is accepted and named after \code{name}).
#' @param name Character; the analysis name, which becomes part of the file
#'   name.
#' @param provider Character; the embedding provider name, which becomes part
#'   of the file name.
#' @param dir Character; the output directory, created recursively if missing.
#'   Defaults to \code{file.path("output", "embeddings")}.
#' @return The path written, invisibly.
#' @seealso \code{\link{embed}} for the \code{texts} attribute this relies on,
#'   \code{\link{embedding_info}} to read the provenance back, and
#'   \code{\link{write_stats}} for the accompanying statistics.
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
    is.matrix(m) && (is.null(attr(m, "texts", exact = TRUE)) ||
                     length(attr(m, "texts", exact = TRUE)) != nrow(m)), TRUE)
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
  message("embeddings saved: ", path)
  invisible(path)
}

#' Record a set of statistics to a CSV file
#'
#' @description
#' Writes the statistics an analysis produced to
#' \code{dir/name_provider.csv}, one row per statistic. Keeping every reported
#' number in a machine-readable file is what allows the values in a manuscript
#' to be checked against the code that produced them, and allows a re-run to be
#' compared with an earlier one.
#'
#' @param stats A named list or named vector of statistics. Each element is
#'   coerced with \code{as.character()} and only its first value is kept, so
#'   pass scalars.
#' @param name Character; the analysis or demonstration name, which becomes
#'   part of the file name and fills the \code{demo} column.
#' @param provider Character; the embedding provider name, which becomes part
#'   of the file name and fills the \code{provider} column.
#' @param dir Character; the output directory, created recursively if missing.
#'   Defaults to \code{"output"}.
#' @return The data frame that was written, invisibly, with columns
#'   \code{demo}, \code{provider}, \code{statistic}, \code{value} and
#'   \code{date}.
#' @seealso \code{\link{save_embeddings}} for archiving the matrices these
#'   statistics were computed from.
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
  message("statistics saved: ", path)
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

#' Convert already-segmented text into the segment table the package expects
#'
#' @description
#' Standardises text that has already been split into segments so that the
#' trajectory and similarity functions can consume it. This is the entry point
#' for the package's text side: how the text was split is the analyst's
#' decision, and this function takes no part in it. Segmenting the text
#' yourself and passing the result here is the ordinary path;
#' \code{\link{segment_text}} is a convenience, not a requirement.
#'
#' @details
#' The result satisfies a simple contract: one row per segment, with
#' \code{doc_id} identifying the document (a participant, an interview) and
#' \code{text} holding the segment. \code{segid} gives the order within a
#' document and is derived from row order when absent. Column names follow
#' ecosystem convention: \code{doc_id} is what \pkg{readtext} returns and the
#' default \code{docid_field} of \code{quanteda::corpus()}, and \code{segid} is
#' the \code{quanteda::corpus_segment()} name. The older names \code{doc} and
#' \code{index} are accepted as input aliases and always will be. Column
#' matching is by alias table, never by guessing. A missing \code{text}
#' column, or two columns matching the same role, stops the function and asks
#' you to name the column, because silently using the wrong column is the
#' worst outcome of all. A missing \code{doc_id} or \code{segid} is filled in
#' instead, with a message saying so: one document named \code{"doc1"}, and
#' segment order taken from the order of the rows.
#'
#' Three practical starting points:
#' \enumerate{
#'   \item \strong{An export from qualitative analysis software} (CSV or
#'     Excel). Taguette writes \code{id, document, tag, content} and QualCoder
#'     writes \code{File, Coder, Coded, ...}, both one row per coded segment.
#'     Those column names are matched automatically, so you can read the
#'     export with \code{read.csv()} and pass the result straight in.
#'   \item \strong{Transcript files}, one file per participant. Write speaker
#'     turns as blank-line-separated paragraphs with the speaker as a
#'     \code{Name:} prefix; \code{\link{read_segments}} reads \code{.txt},
#'     \code{.docx}, \code{.vtt} and \code{.srt}.
#'   \item \strong{Your own segmentation.} A \code{data.frame(doc_id, text)} in
#'     the intended order is enough; \code{segid} is derived from row order.
#' }
#'
#' One format to avoid: a plain text file with one segment per line. The tool
#' that actually produces that shape is Whisper's \code{.txt} output, whose
#' lines are two- to five-second audio spans rather than speaker turns,
#' sentences, or units of meaning. It looks correctly structured and is not.
#'
#' @param x The segmented text: a data frame with one row per segment, a
#'   named character vector (names become \code{doc_id}), a named list of
#'   character vectors (one element per document, each in order), a
#'   \pkg{quanteda} corpus, or an existing \code{qe_segments} object.
#' @param ... Arguments passed to the method for \code{x}. The data frame
#'   method accepts \code{doc_id}, \code{segid} and \code{text} to name those
#'   columns explicitly when the automatic alias matching is ambiguous or
#'   wrong; \code{renumber} (logical, default \code{FALSE}) to renumber
#'   \code{segid} from row order within each document; \code{drop_empty}
#'   (logical, default \code{TRUE}) to discard empty and \code{NA} segments;
#'   \code{unit}, a string recording the unit of segmentation (for example
#'   \code{"sentence"}) that is stored as an attribute for figure and table
#'   notes; and \code{quiet} (logical) to suppress the messages reporting what
#'   was derived.
#' @return An object of class \code{c("qe_segments", "data.frame")} with
#'   columns \code{doc_id}, \code{segid}, \code{text}, \code{n_words},
#'   \code{n_char} and \code{docname} (\code{doc_id.segid}, unique by
#'   construction), followed by any other columns of the input, which are
#'   carried through unchanged. Rows are ordered by document in order of first
#'   appearance, then by \code{segid}.
#' @seealso \code{\link{segment_text}} to split long texts, and
#'   \code{\link{is_segments}} to test the class.
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
  out$text    <- .declare_utf8(out$text)
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

#' Test whether an object is a segment table
#'
#' @description
#' Returns \code{TRUE} when \code{x} is a \code{qe_segments} object, that is,
#' the standardised segment table returned by \code{\link{as_segments}},
#' \code{\link{segment_text}} or \code{\link{read_segments}}. Use it to guard
#' code that assumes the \code{doc_id}/\code{segid}/\code{text} columns are
#' present.
#'
#' @param x Any object.
#' @return A length-one logical.
#' @seealso \code{\link{as_segments}}, \code{\link{segment_text}} and
#'   \code{\link{read_segments}}, which produce the tested class.
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
# UTF-8 でないロケール（サーバでは LC_ALL=C が既定であることが多い）では、
# ファイルや他パッケージから来たテキストが "unknown" のまま入り、R が
# 1 バイトを 1 文字として扱う。正規表現も nchar() も壊れるので、入口で
# 一度だけ宣言する。すでに UTF-8 なら何も起きない。
.declare_utf8 <- function(x) {
  if (!is.character(x)) return(x)
  u <- Encoding(x) == "unknown" & !is.na(x)
  if (any(u)) {
    ok <- u & validUTF8(x)
    if (any(ok)) Encoding(x)[ok] <- "UTF-8"
  }
  x
}


#' Count words using ICU word boundaries
#'
#' @description
#' Counts words with the ICU boundary analysis exposed by \pkg{stringi}, so the
#' count is defined for languages that do not delimit words with spaces.
#' Splitting on whitespace would count an entire Japanese paragraph as one
#' word; ICU applies dictionary-based segmentation to CJK text, so English and
#' Japanese both work through the same call.
#'
#' @param x A character vector.
#' @return An integer vector of word counts, one per element of \code{x}.
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
  .qe_env$cjk <- .n_words("\u6628\u65e5\u306f\u671d\u304b\u3089\u96e8\u3060\u3063\u305f") > 3
  if (!.qe_env$cjk)
    warning("This build of ICU has no CJK word dictionary, so n_words will ",
            "undercount Japanese text (sentence splitting is unaffected -- ",
            "it is rule-based). Use by = \"chars\" for windows, and ",
            "min_words = 0 to disable the short-fragment merge.",
            call. = FALSE)
  invisible(.qe_env$cjk)
}

#' List the abbreviations protected during sentence splitting
#'
#' @description
#' Returns the default set of abbreviations that \code{\link{segment_text}}
#' protects when splitting text into sentences. ICU treats the period in
#' \code{"Dr."} as a sentence boundary; before splitting, the periods of these
#' abbreviations are swapped for a private-use character and restored
#' afterwards, so the sentence is not broken in the middle.
#'
#' @details
#' Extend the set by passing \code{c(qe_abbreviations(), "Univ")} to the
#' \code{abbrev} argument of \code{\link{segment_text}}, or disable protection
#' entirely with \code{character(0)}. Matching is case-sensitive and on whole
#' words. \code{"a.m."} is deliberately excluded: it genuinely ends sentences
#' often enough that protecting it would merge more sentences than it saves.
#'
#' @return A character vector of abbreviations, written without the trailing
#'   period.
#' @seealso \code{\link{segment_text}}, whose \code{abbrev} argument defaults to
#'   this set.
#' @export
qe_abbreviations <- function() {
  c("Dr", "Mr", "Mrs", "Ms", "Prof", "Sr", "Jr", "St", "Fig", "No", "Vol",
    "Ch", "e.g", "i.e", "cf", "vs", "approx", "Inc", "Ltd", "Ph.D",
    "M.A", "B.A")
}

.SENT_SENTINEL <- "\ue000"

#' Split long texts into segments
#'
#' @description
#' Splits each element of a character vector into segments and returns them as
#' a \code{qe_segments} table. A long response -- an interview transcript, a
#' diary entry, a multi-paragraph open-ended answer -- becomes a single point
#' when embedded whole, and everything that moves inside it is lost. Embedding
#' its segments instead turns the response into a trajectory through semantic
#' space. The unit of segmentation changes the result, so choose \code{by}
#' deliberately rather than accepting a default; splitting the text by your own
#' rule and passing it to \code{\link{as_segments}} is the more usual path.
#'
#' @details
#' Sentence boundaries and word counts come from the ICU boundary analysis in
#' \pkg{stringi}. English \code{.!?} and their full-width Japanese
#' counterparts are therefore handled by the same call, and word counts in
#' Japanese are morpheme-based rather than whitespace-based. There is no
#' language argument, because ICU inspects the writing system itself and leaves
#' nothing for the caller to declare. No locale is set either: only the
#' locale-independent part of UAX #29 is used, so the result does not depend
#' on the locale the session happens to run in.
#'
#' For analyses that compare two languages, use \code{by = "sentence"}. Because
#' an ICU "word" in Japanese is roughly a morpheme, a translation yields more
#' Japanese words than English ones for the same content, so \code{size = 50}
#' is not the same window in the two languages. Sentence
#' counts were the only unit that matched across translations.
#'
#' If the ICU build in use has no CJK word dictionary, a warning is issued the
#' first time a word count is needed: word counts for Japanese will be too low,
#' though sentence splitting is unaffected because it is rule-based. In that
#' situation, use \code{by = "chars"} for windows and \code{min_words = 0} to
#' disable the short-fragment merge.
#'
#' @param x A character vector; each element is one document.
#' @param by Character; the unit of segmentation, either \code{"sentence"} (ICU
#'   sentence boundaries), \code{"words"} (fixed-width window in words),
#'   \code{"chars"} (fixed-width window in characters), or \code{"paragraph"}
#'   (blank-line separated blocks). If the reason for splitting is a token
#'   limit, \code{"chars"} is the only unit that means the same thing across
#'   languages. Defaults to \code{"sentence"}.
#' @param size Integer; the window size for \code{by = "words"} or
#'   \code{"chars"}, ignored otherwise. Defaults to \code{NULL}, which uses 50
#'   words or 200 characters.
#' @param overlap Integer; the number of words or characters shared between
#'   consecutive windows. Must be smaller than \code{size}. Defaults to 0.
#' @param min_words Integer; the length in words below which a fragment is
#'   merged into the preceding segment. A value of 2 absorbs only one-word
#'   fragments, such as a bare backchannel ("Right."); raising it to 3 also
#'   swallows legitimate short sentences such as "It worked.", which is why it
#'   is not recommended, and 0 merges nothing. Defaults to 2.
#' @param abbrev A character vector of abbreviations whose periods are
#'   protected from sentence splitting, written without the trailing period;
#'   pass \code{character(0)} to disable. Defaults to
#'   \code{qe_abbreviations()}.
#' @param ids A vector of document identifiers, one per element of \code{x},
#'   used as \code{doc_id}. Defaults to \code{NULL}, which numbers the
#'   documents sequentially.
#' @return A \code{qe_segments} object (see \code{\link{as_segments}}) with
#'   columns \code{doc_id}, \code{segid}, \code{text}, \code{n_words},
#'   \code{n_char} and \code{docname}, and the unit of segmentation recorded as
#'   an attribute.
#' @seealso \code{\link{as_segments}} for text you have segmented yourself, and
#'   \code{\link{read_segments}} for reading transcript files.
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
  # In a non-UTF-8 locale (LC_ALL=C, common on servers), text read from a
  # file or passed from another package arrives tagged "unknown", and R then
  # treats each byte as a character: the regex below fails on any non-ASCII
  # input. Declaring the encoding costs nothing when it is already UTF-8.
  x <- .declare_utf8(x)
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
      m <- regmatches(p[i], regexpr("[\u300c\u300e\uff08(\u201c\"']+$", p[i]))
      if (length(m) && nzchar(m)) {
        p[i]     <- sub("[\u300c\u300e\uff08(\u201c\"']+$", "", p[i])
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

# 符号化の取り違えは、経路によって現れ方が違う。LC_ALL=C ではバイトがその
# まま通って黙って壊れ、UTF-8 ロケールでは R が生のエラーを投げる。どの
# 経路からも同じ説明に落とすための共有部品。
.enc_mismatch_stop <- function(f, encoding) {
  stop("read_segments(): '", basename(f), "' could not be read as ",
       encoding, ". This is what a character-encoding mismatch looks ",
       "like: rows vanish rather than turning into mojibake, so the ",
       "loss can be partial and silent. Re-save the file as UTF-8 (in ",
       "Excel: \"CSV UTF-8\"), or pass encoding = \"CP932\" for a ",
       "Japanese Windows export.", call. = FALSE)
}

# 行として読む。readLines(encoding=) は「そう書かれている」と宣言する
# だけで、BOM も落とさず内容も検査しない。UTF-8 は自分でバイト列を扱い、
# 他の符号化は接続に変換させる。
.read_text_lines <- function(f, encoding) {
  if (grepl("^UTF-8", encoding, ignore.case = TRUE)) {
    raw <- readBin(f, "raw", file.size(f))
    if (length(raw) >= 3 &&
        identical(as.integer(raw[1:3]), c(239L, 187L, 191L)))
      raw <- raw[-(1:3)]
    txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
    if (!validUTF8(txt)) .enc_mismatch_stop(f, encoding)
    strsplit(txt, "\r\n|\n|\r")[[1]]
  } else {
    con <- file(f, encoding = encoding)
    on.exit(close(con), add = TRUE)
    ln <- tryCatch(readLines(con, warn = FALSE),
                   error = function(e) .enc_mismatch_stop(f, encoding))
    if (any(is.na(ln)) || (length(ln) && !all(validUTF8(ln[!is.na(ln)]))))
      .enc_mismatch_stop(f, encoding)
    ln
  }
}

.read_one <- function(f, format, unit, encoding) {
  ext <- tolower(tools::file_ext(f))
  if (format == "auto") format <- ext
  if (format %in% c("txt", "text", "md")) {
    raw <- paste(.read_text_lines(f, encoding), collapse = "\n")
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
    ln <- .read_text_lines(f, encoding)
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
    # UTF-8 のファイルは encoding = で「そう書かれている」と宣言するだけに
    # する。fileEncoding = はロケールの符号化へ変換するので、LC_ALL=C では
    # その変換が日本語で失敗し、正しいファイルまで誤診していた。
    # 一方 CP932 のような他の符号化は変換しなければ読めないので、そちらは
    # fileEncoding = を使う。
    # UTF-8 のファイルは自分でバイト列として読み、BOM を落としてから
    # 渡す。read.csv(fileEncoding=) はロケールの符号化へ変換するので
    # LC_ALL=C では日本語が読めず、read.csv(encoding=) は宣言するだけで
    # BOM を落とさない（Excel の「CSV UTF-8」は BOM を書く）。どちらの
    # 経路にも寄らず、妥当性は自分で検査する。
    # 取り違えの現れ方はロケールによって違う。LC_ALL=C ではバイトがそのまま
    # 通って行が黙って消え、UTF-8 ロケールでは read.csv が生のエラーを投げる。
    # どちらの経路からも同じ説明に落とす。
    .enc_stop <- function() .enc_mismatch_stop(f, encoding)
    .enc_pat <- "invalid input|invalid multibyte|\u4e0d\u6b63\u306a\u5165\u529b"

    # 行として読むところまでを .read_text_lines に任せる。read.csv の
    # fileEncoding= はロケールの符号化へ変換するので LC_ALL=C では CP932 が
    # 読めず、encoding= は宣言するだけで BOM を落とさない。どちらにも寄らない。
    d <- withCallingHandlers(
      tryCatch({
        utils::read.csv(text = .read_text_lines(f, encoding),
                        sep = if (format == "tsv") "\t" else ",",
                        stringsAsFactors = FALSE, encoding = "UTF-8")
      }, error = function(e) {
        if (grepl(.enc_pat, conditionMessage(e))) .enc_stop() else stop(e)
      }),
      warning = function(w) {
        if (grepl(.enc_pat, conditionMessage(w))) bad_enc <<- TRUE
        invokeRestart("muffleWarning")
      })
    if (bad_enc || !nrow(d)) .enc_stop()
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

#' Read a segment table from transcripts or exported text files
#'
#' @description
#' Reads one file, a vector of files, or a whole directory of transcripts and
#' returns a segment table ready for the trajectory functions. Text and Word
#' files are split into paragraphs, subtitle files into cues, and spreadsheet
#' or delimited exports are taken one row per segment. Use this whenever the
#' material already lives in files, rather than assembling a segment table by
#' hand.
#'
#' @details
#' The recommended layout is one file per participant, one paragraph per
#' speaker turn separated by a blank line, and a speaker prefix of the form
#' \code{Name:}. Word transcripts usually arrive in that form already, and
#' the .vtt files produced by Zoom, Teams, or Whisper additionally carry
#' speaker labels and time stamps.
#'
#' One segment per line in a .txt file is \strong{not} recommended. Whisper's
#' plain-text output has that shape, but its lines are two- to five-second
#' audio spans rather than speaker turns or sentences. The default unit for
#' .txt is therefore "paragraph" and not "line", and a multi-line file with
#' no blank lines raises a warning.
#'
#' Exports from qualitative-analysis software (CSV or Excel) are already one
#' row per segment and can be read as they stand; \code{\link{as_segments}}
#' matches their column names automatically.
#'
#' Reading .docx requires the \pkg{xml2} package and .xlsx requires
#' \pkg{readxl}; the other formats need no extra dependency.
#'
#' @param path Character; a file, a vector of files, or a directory.
#' @param format Character; the file format, either \code{"auto"} (taken from
#'   the file extension) or one of \code{"txt"}, \code{"docx"}, \code{"vtt"},
#'   \code{"srt"}, \code{"csv"}, \code{"tsv"}, \code{"xlsx"}. Defaults to
#'   \code{"auto"}.
#' @param unit Character; the segmentation unit. \code{"auto"} gives paragraphs
#'   for txt and docx, cues for vtt and srt, and rows for tabular files.
#'   Defaults to \code{"auto"}.
#' @param speaker Logical; whether to split a leading \code{"Name:"} off each
#'   segment into a \code{speaker} column. Defaults to \code{TRUE}.
#' @param doc_id Character; the name of the document-identifier column in a
#'   tabular file. Ignored when files are read from disk, where the file name
#'   becomes the \code{doc_id}. Defaults to \code{NULL}.
#' @param pattern A regular expression restricting which files are read when
#'   \code{path} is a directory. Defaults to \code{NULL}, which matches all
#'   supported extensions.
#' @param recursive Logical; whether to search the directory recursively.
#'   Defaults to \code{FALSE}.
#' @param encoding Character; the file encoding. \code{"UTF-8-BOM"} is the
#'   only setting R
#'   documents as guaranteed to strip a byte-order mark, and it
#'   reads files without one correctly too. A character-encoding mismatch is
#'   silent rather than obvious: a Shift_JIS/CP932 file read as UTF-8 turns
#'   the affected rows into NA instead of mojibake, so rows vanish rather than
#'   look wrong. Any NA surviving the read therefore stops the function with
#'   an error. \code{"CP932"}, for a Japanese Windows export, works for the
#'   delimited formats and needs a session whose locale can represent the
#'   characters, since R converts as it reads; a plain \code{.txt} in that
#'   encoding has to be converted before it reaches this function. A UTF-8
#'   file is read without conversion and so does not depend on the locale.
#'   Defaults to \code{"UTF-8-BOM"}.
#' @param ... Passed to \code{\link{as_segments}}.
#' @return A \code{qe_segments} data frame, one row per segment, with columns
#'   \code{doc_id}, \code{segid}, \code{text}, \code{n_words}, \code{n_char},
#'   \code{docname} (\code{doc_id.segid}, the name to embed the text under),
#'   and \code{speaker} where a speaker prefix was found.
#' @seealso \code{\link{as_segments}}, \code{\link{trajectory_stats}},
#'   \code{\link{plot_recurrence}}
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
    d$text <- .declare_utf8(d$text)   # 話者名の切り出しも文字単位で行う
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

#' Summarise the path each segmented document takes
#'
#' @description
#' Returns one row of path statistics per document: how far a text travels
#' through the semantic space, how far it ends up from where it started, and
#' how directly it gets there. Use it once long responses or transcripts have
#' been segmented, and read it together with \code{\link{trajectory_null}},
#' which says whether an observed value differs from what the same segments in
#' any other order would give.
#'
#' The statistics themselves are not new. Path length divided by the number of
#' steps is the speed of Toubia et al. (2021), the ratio of net displacement
#' to path length is the reciprocal of their circuitousness, and consecutive
#' similarity is used by Palominos et al. (2024) and Bedi et al. (2015). What
#' this function adds is an R implementation on the same testing footing as
#' the rest of the package.
#'
#' @param emb A numeric matrix of segment embeddings, one row per segment. When
#'   its row names are the segment table's \code{docname} values the rows are
#'   matched by name; otherwise they are matched by position and must already
#'   be in the table's order.
#' @param seg A segment table satisfying the \code{\link{as_segments}}
#'   contract.
#' @param axis An optional numeric vector of per-segment scores on a projection
#'   axis, as returned by \code{\link{semantic_projection}}, with one value
#'   per row of \code{emb}. Adds axis summaries to the output. Defaults to
#'   \code{NULL}.
#' @return A data frame with one row per document that has at least two
#'   segments:
#'   \describe{
#'     \item{\code{doc_id}, \code{n_seg}, \code{n_words}}{document identifier,
#'       number of segments, total words.}
#'     \item{\code{path_length}}{sum of the cosine distances between
#'       consecutive segments. Longer documents give longer paths whatever
#'       the text does, so this correlates with \code{n_seg} and must not be
#'       interpreted on its own; use \code{step_mean} or
#'       \code{straightness} instead.}
#'     \item{\code{step_mean}}{path length divided by the number of steps.}
#'     \item{\code{net_displacement}}{cosine distance from the first segment
#'       to the last.}
#'     \item{\code{straightness}}{net displacement divided by path length.
#'       Near 1 the document moves in one direction; near 0 it doubles back.}
#'   }
#'   When \code{axis} is supplied, the columns \code{axis_mean},
#'   \code{axis_sd}, \code{axis_range}, \code{axis_start} and
#'   \code{axis_end} are appended. Documents with a single segment are
#'   dropped.
#' @details
#' Different trajectories can yield identical aggregates (Palominos et al.,
#' 2024), so report these scalars alongside a display rather than in place of
#' one: \code{\link{plot_arc}} and \code{\link{plot_recurrence}} show the shape
#' they summarise. Note also that no statistic here resolves an individual
#' transition. See \code{\link{trajectory_null}} for the arithmetic bound that
#' makes a single step untestable in short documents.
#' @seealso \code{\link{trajectory_null}}, \code{\link{plot_recurrence}},
#'   \code{\link{plot_arc}}
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

#' Test a trajectory statistic against reorderings of the same document
#'
#' @description
#' A path length or a straightness value cannot be called large or small on
#' its own. Reordering a document's own segments holds the number of
#' segments, the word count and the content fixed while destroying the order,
#' which gives the value the statistic would take if the sequence carried
#' nothing. Where the observed value falls in that distribution is how much
#' the ordering is doing: a locally coherent narrative should travel less
#' than its own reorderings.
#'
#' \code{stat = "far_mean"} tests what a recurrence plot displays. It takes
#' the mean cosine between segments at least \code{lag_min} positions apart
#' and compares it with the same reorderings. Reordering leaves the set of
#' similarities untouched and changes only which pairs are near one another
#' in the narrative, so this null asks precisely whether the ordering creates
#' the structure on the plot. An observed value \emph{above} the null is a
#' return to earlier material and one \emph{below} it a topic shift; use it
#' to decide whether a block that caught the eye in
#' \code{\link{plot_recurrence}} is more than chance.
#'
#' @param emb A numeric matrix of segment embeddings, one row per segment;
#'   rows are matched to the table by \code{docname} where possible, otherwise
#'   by position (see \code{\link{trajectory_stats}}).
#' @param seg A segment table satisfying the \code{\link{as_segments}}
#'   contract.
#' @param n_perm Integer; the number of reorderings. Defaults to 999.
#' @param stat Character; the statistic to evaluate, either
#'   \code{"path_length"}, \code{"step_mean"}, \code{"straightness"} or
#'   \code{"far_mean"}. Defaults to \code{"path_length"}.
#' @param lag_min Integer; for \code{stat = "far_mean"}, the smallest
#'   separation in segment positions that counts as distant. Defaults to 3.
#' @return A data frame with one row per document of at least three segments
#'   and columns \code{doc_id}, \code{n_seg}, \code{observed},
#'   \code{null_mean}, \code{z} (the observed value in standard deviations of
#'   the null) and \code{p}. The p value is two-sided: the number of
#'   reorderings with |null - null_mean| >= |observed - null_mean|, plus one,
#'   divided by \code{n_perm + 1}. Shorter documents are dropped, as are
#'   documents with no pair at least \code{lag_min} apart when
#'   \code{stat = "far_mean"}.
#' @details
#' \strong{The resolution of a single transition is fixed by arithmetic, so
#' this function tests at the document level only.} Reordering makes each
#' adjacent pair a random pair drawn from that document's own n(n - 1)/2
#' pairwise distances, and that set is the whole null distribution available
#' for one step. The smallest attainable p for a document of n segments is
#' therefore 2/(n^2 - n + 2): .14 at four segments, .045 at seven and .03 at
#' eight, while a six-segment document cannot reach .05 at all. The ceiling
#' is known before the data are seen. Do not read a non-significant result
#' from a short document as evidence that its ordering carries nothing, and
#' do not attempt a verdict on an individual transition at the lengths a
#' survey returns, where the arithmetic above puts the answer out of reach
#' before any text is embedded.
#'
#' For \code{stat = "far_mean"} the count of distant pairs falls away quickly
#' in short documents: at the default \code{lag_min} a five-segment document
#' has three of them and a four-segment one has a single pair, so the mean is
#' not worth reading below about six segments.
#' Permutation is within document: documents are tested independently and the
#' p values are not corrected across them. If you then read the smallest p
#' among several documents, that smallest value is a search result and not a
#' single test; pass \code{keep_null = TRUE} and build its null from the draws
#' rather than moving the threshold.
#' @param keep_null attach the permutation draws (one numeric vector per
#'   returned row, in row order) as the \code{"null"} attribute of the result.
#' @param alternative direction of the test: \code{"two.sided"} (the default)
#'   where the statistic predicts no sign, \code{"less"} where the observed
#'   value is predicted to fall below its shuffles, \code{"greater"} for the
#'   opposite prediction.
#' @seealso \code{\link{trajectory_stats}}, \code{\link{plot_recurrence}},
#'   \code{\link{plot_arc}}
#' @export
trajectory_null <- function(emb, seg, n_perm = 999, stat = "path_length",
                            lag_min = 3, keep_null = FALSE,
                            alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  docs <- unique(seg$doc_id)
  .draws <- list()
  out <- do.call(rbind, lapply(docs, function(dd) {
    k <- which(seg$doc_id == dd)
    k <- k[order(seg$segid[k])]
    if (length(k) < 3) return(NULL)          # 3段未満は並べ替えの意味がない
    S <- cos_sim_matrix(emb[k, , drop = FALSE])
    # far_mean は「離れた対」を語りの位置で定義するので、行列の外に
    # 一度だけ作っておく。並べ替えるのは行列のほうである。
    .up  <- upper.tri(S)
    .far <- .up & abs(row(S) - col(S)) >= lag_min
    if (identical(stat, "far_mean") && !any(.far)) return(NULL)
    path <- function(o) {
      st <- 1 - S[cbind(o[-length(o)], o[-1])]
      switch(stat,
             path_length = sum(st),
             step_mean   = mean(st),
             straightness = { d <- 1 - S[o[1], o[length(o)]]
                              if (sum(st) > 0) d / sum(st) else NA_real_ },
             far_mean    = { Q <- S[o, o]; mean(Q[.far]) },
             stop("unknown stat: ", stat))
    }
    obs  <- path(seq_along(k))
    null <- replicate(n_perm, path(sample.int(length(k))))
    if (keep_null) .draws[[length(.draws) + 1L]] <<- null
    mu   <- mean(null, na.rm = TRUE)
    sdv  <- stats::sd(null, na.rm = TRUE)
    data.frame(doc_id = dd, n_seg = length(k), observed = obs,
               null_mean = mu,
               z = if (is.finite(sdv) && sdv > 0) (obs - mu) / sdv else NA_real_,
               p = (switch(alternative,
                           two.sided = sum(abs(null - mu) >= abs(obs - mu), na.rm = TRUE),
                           less      = sum(null <= obs, na.rm = TRUE),
                           greater   = sum(null >= obs, na.rm = TRUE)) + 1) /
                   (n_perm + 1),
               stringsAsFactors = FALSE)
  }))
  if (keep_null) attr(out, "null") <- .draws
  out
}

#' Quantify recurrence in a segmented document
#'
#' @description
#' Recurrence quantification analysis (RQA) for embedded text. The segment
#' similarity matrix is binarised at a fixed recurrence rate and the
#' resulting pattern of recurrent points is summarised by its diagonal and
#' vertical lines. Use it on documents long enough to contain repeated
#' passages; at the lengths a survey returns the line-based columns are
#' undefined (see Details).
#'
#' No delay embedding is required here. An embedding vector is already the
#' state of the system, so m = 1 and tau = 1, which removes the largest
#' source of artefact in applying RQA to text. The threshold is set from a
#' fixed recurrence rate rather than an absolute cosine, because the cosine
#' scale differs between providers and the number of segments differs between
#' documents, so no absolute cut-off is comparable across either.
#'
#' @param emb A numeric matrix of segment embeddings, one row per segment;
#'   rows are matched to the table by \code{docname} where possible, otherwise
#'   by position.
#' @param seg A segment table satisfying the \code{\link{as_segments}}
#'   contract.
#' @param rr Numeric; the target recurrence rate. The threshold is the
#'   corresponding quantile of that document's off-diagonal similarities.
#'   Defaults to .05.
#' @param lmin Integer; the shortest run of recurrent points counted as a line.
#'   Defaults to 2.
#' @return A data frame with one row per document of at least four segments:
#'   \describe{
#'     \item{\code{doc_id}, \code{n_seg}, \code{threshold}}{document
#'       identifier, number of segments, cosine cut-off used.}
#'     \item{\code{RR}}{recurrence rate actually attained.}
#'     \item{\code{DET}}{determinism: share of recurrent points lying on
#'       diagonal lines, that is, how far the document retraces the same
#'       topics in the same order.}
#'     \item{\code{LAM}}{laminarity: share lying on vertical lines, that is,
#'       how far it stays on one topic.}
#'     \item{\code{L}, \code{L_max}}{mean and longest diagonal line, the
#'       average and maximum length of a repeated episode.}
#'     \item{\code{TT}}{trapping time: mean vertical line length.}
#'     \item{\code{ENTR}}{Shannon entropy of the diagonal line-length
#'       distribution.}
#'   }
#'   \code{DET} and \code{LAM} are 0 and the remaining line-based columns are
#'   NA when the document contains no line of at least
#'   \code{lmin} points.
#' @details
#' \strong{The line-based measures need documents an order of magnitude
#' longer than a typical survey response.} At the handful of segments a
#' survey answer yields, the recurrence matrix contains no diagonal or
#' vertical line at all, so DET,
#' LAM, L, TT and ENTR carry no information: the first two come back as 0
#' and the rest as NA. They are provided
#' because longer material makes them computable, not because this package
#' has evidence that they measure anything at survey lengths; interview
#' transcripts are where they would first become usable. At shorter lengths use
#' \code{\link{plot_recurrence}}, which needs no threshold and stays readable,
#' and \code{\link{trajectory_null}} with \code{stat = "far_mean"}, which tests
#' the same structure without requiring lines.
#' @seealso \code{\link{plot_recurrence}}, \code{\link{trajectory_null}},
#'   \code{\link{trajectory_stats}}
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

#' Plot conceptual recurrence for one document
#'
#' @description
#' Displays the full segment-by-segment cosine similarity matrix of a single
#' document (Angus et al., 2012). \strong{This is the first display to reach
#' for once a document has been segmented.} Cell (i, j) is the cosine
#' similarity between two segment embeddings computed in \emph{all}
#' dimensions: there is no dimension reduction anywhere, and both axes carry
#' nothing but segment position, so the distortion a two-dimensional
#' projection introduces (Chari and Pachter, 2023) never arises. Unlike
#' \code{\link{plot_trajectory}}, nothing here has to be read with a geometric
#' caveat attached. No threshold is applied, following Angus rather than the
#' Eckmann/Marwan binarisation.
#'
#' @section Reading the plot:
#' Blocks along the diagonal are stretches that stay on one topic. Blocks
#' away from the diagonal are returns to earlier material. A bright vertical
#' stripe is an early passage that everything afterwards keeps referring back
#' to. Dark regions are new material.
#'
#' Structure picked out by eye should then be tested with
#' \code{\link{trajectory_null}}. A display can make shifts and returns
#' equally visible while only one of them survives reordering of the
#' segments, a difference worth knowing before deciding
#' which of the two to write up.
#'
#' @param emb A numeric matrix of segment embeddings, one row per segment;
#'   rows are matched to the table by \code{docname} where possible, otherwise
#'   by position.
#' @param seg A segment table satisfying the \code{\link{as_segments}}
#'   contract.
#' @param doc Character; which document to draw. Required when the table
#'   holds more than one, since a recurrence plot is a within-document
#'   object; the function stops rather than pooling documents. Defaults to
#'   \code{NULL}.
#' @param anchor Character; how the colour scale is anchored.
#'   \code{"quantile"} takes the two ends from quantiles of the off-diagonal
#'   cells; commercial APIs return cosines packed into a narrow band whose
#'   position differs by provider, so a scale running from 0 to 1 paints the
#'   whole plot one colour. \code{"range"} uses the observed minimum and maximum instead.
#'   Defaults to \code{"quantile"}.
#' @param probs A numeric vector of length 2 giving the lower and upper
#'   probabilities used when \code{anchor = "quantile"}. Defaults to
#'   \code{c(.02, .98)}.
#' @param title Character; the plot title. Defaults to \code{NULL}, which
#'   prints \code{"Conceptual recurrence:"} followed by the document name.
#' @return A \pkg{ggplot2} object (printed when called at the top level): a
#'   raster of the n by n similarity matrix on a blue-white-red scale centered at the
#'   median off-diagonal similarity, with every segment numbered on both axes
#'   so that cells can be traced back to the text, and a fixed 1:1 aspect
#'   ratio. Documents with fewer than three segments raise an error.
#' @seealso \code{\link{trajectory_null}}, \code{\link{plot_arc}},
#'   \code{\link{recurrence_stats}}
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
    # 目盛りはすべてのセグメントに振る。既定の間引きだと本文一覧の番号と
    # 突き合わせられない行が出る。
    scale_y_reverse(expand = c(0, 0), breaks = seq_len(n)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq_len(n)) +
    coord_fixed() +
    labs(title = title %||% paste0("Conceptual recurrence: ", doc),
         x = "Segment", y = "Segment") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank())
  p
}

#' Plot a semantic arc across narrative position
#'
#' @description
#' Draws how one per-segment quantity moves from the start of a document to
#' its end: a score on an anchor axis, the distance from the preceding
#' segment, or the distance from everything said so far. Every vertical
#' quantity is computed in \emph{all} dimensions and the horizontal axis
#' carries nothing but segment position, so the display is free of the
#' distortion a two-dimensional projection introduces. Together with
#' \code{\link{plot_recurrence}} it is what to look at before any projected
#' display.
#'
#' @param emb A numeric matrix of segment embeddings, one row per segment;
#'   rows are matched to the table by \code{docname} where possible, otherwise
#'   by position.
#' @param seg A segment table satisfying the \code{\link{as_segments}}
#'   contract.
#' @param y Character; the quantity on the vertical axis.
#'   \code{"projection"} projects each segment onto the axis running from the
#'   low to the high anchors; \code{"step"} is the cosine distance from the
#'   previous segment, the speed of Toubia et al. (2021) resolved one step at
#'   a time; \code{"forward_flow"} is the mean distance from all preceding
#'   segments (Gray et al., 2019). The first segment is NA for the last two.
#'   Defaults to \code{"projection"}.
#' @param high,low Numeric matrices of anchor embeddings defining the two
#'   poles of the axis, required when \code{y = "projection"} and used as in
#'   \code{\link{semantic_projection}}. Embed the phrases that name each
#'   pole. Default to \code{NULL}.
#' @param x Character; the horizontal axis, either \code{"auto"} (segment
#'   position for a single document, relative position for several),
#'   \code{"position"}, or \code{"relative"}, which is (t - 0.5)/T and so puts
#'   documents of different lengths on a common scale. Defaults to
#'   \code{"auto"}.
#' @param smooth Logical, or the string \code{"auto"}; whether to add a loess
#'   curve. \code{"auto"} draws one only when the shortest document has at
#'   least ten segments, a loess through a handful of points having no
#'   readable shape. Defaults to \code{"auto"}.
#' @param null_band Integer; the number of random reorderings of the segments
#'   used to build a null, where 0 computes none and 999 is a
#'   reasonable value. For \code{y = "step"} and \code{y = "forward_flow"} a
#'   pointwise 2.5-97.5\% band is shaded. For \code{y = "projection"}
#'   \strong{no band is drawn}: projection scores do not depend on order, so
#'   reordering leaves the set of values untouched, and a pointwise band
#'   would be flat by construction and would test nothing. A permutation p
#'   for the rank correlation between position and projection is printed in
#'   the subtitle instead, which asks the question that is meaningful here,
#'   namely whether the arc has a slope. When bands are drawn for several
#'   documents the panels are faceted, being unreadable when overlaid.
#'   Defaults to 0.
#' @param center Character; how the values are centered, either \code{"auto"}
#'   (raw values for a single document, centered on the document's own mean
#'   when several are drawn), \code{"none"}, or \code{"mean"}. Defaults to
#'   \code{"auto"}.
#' @param title Character; the plot title. Defaults to \code{NULL}, which
#'   prints \code{"Semantic arc"}.
#' @return A \pkg{ggplot2} object (printed when called at the top level): one
#'   line per document with each segment marked by a numbered open circle, the
#'   optional loess curve, and either the shaded null band or the trend test
#'   in the subtitle.
#' @details
#' The shaded bands are pointwise and uncorrected, so they describe the
#' document rather than test it. In particular they do not license a claim
#' about any individual step: reordering makes each adjacent pair a random
#' pair from that document's own pairwise distances, which floors the
#' attainable p for a document of n segments at 2/(n^2 - n + 2), or .14 at
#' four segments and .03 at eight. See \code{\link{trajectory_null}} for the
#' tests that remain available at the document level.
#' @seealso \code{\link{plot_recurrence}}, \code{\link{trajectory_null}},
#'   \code{\link{trajectory_stats}}
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
  # 点は白抜きの丸に番号を入れる。軌跡の図と同じ書式にして、どちらの
  # 表示でも同じ番号で本文一覧を引けるようにする。
  p <- p + geom_line(aes(color = .data$doc_id, group = .data$doc_id),
                     linewidth = .7, na.rm = TRUE) +
    geom_point(aes(color = .data$doc_id), fill = "white", shape = 21,
               size = 3.2, stroke = .5, na.rm = TRUE) +
    geom_text(aes(label = .data$segid), size = 1.9, colour = "grey15",
              na.rm = TRUE)
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
  p
}

.layout_2d <- function(M, layout = c("mds", "pca"), axis = NULL) {
  layout <- match.arg(layout)
  v <- if (is.null(axis)) NULL
       else as.numeric(scale(M, scale = FALSE) %*% (axis / sqrt(sum(axis^2))))
  if (layout == "mds" && nrow(M) >= 4 &&
      requireNamespace("MASS", quietly = TRUE)) {
    D  <- stats::dist(M)
    xy <- if (min(D) > 0)
      try(suppressMessages(MASS::isoMDS(D, k = 2, trace = FALSE)), silent = TRUE)
      else structure("degenerate", class = "try-error")
    if (!inherits(xy, "try-error")) {
      P <- xy$points
      if (!is.null(v)) P <- .rotate_to(P, v)
      return(structure(P, layout = "mds", stress = xy$stress / 100,
                       ve = NA_real_))
    }
  }
  pc <- stats::prcomp(M)
  P  <- pc$x[, 1:2, drop = FALSE]
  if (!is.null(v)) P <- .rotate_to(P, v)
  structure(P, layout = "pca", stress = NA_real_,
            ve = sum(pc$sdev[1:2]^2) / sum(pc$sdev^2))
}

.ax_lab <- function(layout, k)
  if (identical(layout, "pca")) paste0("PC", k) else paste("Dimension", k)

# ── 2次元配置 ───────────────────────────────────────────────
# PCA は分散を最大化するのであって距離の順位を保たない。次元数が点数を
# 大きく超える埋め込みでは第2成分までに乗る分散がわずかで、平面は点を
# 細い帯に潰す。非計量MDS は順位そのものを目的関数にするので、同じ
# 2次元でも順位相関が上がる（デモ15の169セグメントで .54 → .84、
# 等方の点に同じ手続きを当てた帰無は .33 と .56）。既定を MDS にする。
#
# 回転は距離も角度も変えないので忠実さには効かない。効くのは軸の解釈で
# ある。axis を渡すと、その方向に最も沿う向きへ配置を回す——因子分析の
# 回転が負荷量を解釈可能にするのと同じ操作を、点配置に対して行う。
.rotate_to <- function(xy, v) {
  Y  <- scale(xy, scale = FALSE)
  th <- atan2(sum(v * Y[, 2]), sum(v * Y[, 1]))
  Y %*% matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
}

.layout_kd <- function(M, k, layout = c("pca", "mds")) {
  layout <- match.arg(layout)
  if (layout == "mds" && nrow(M) > k + 1 &&
      requireNamespace("MASS", quietly = TRUE)) {
    D <- stats::dist(M)
    if (min(D) > 0) {
      fit <- try(suppressMessages(MASS::isoMDS(D, k = k, trace = FALSE)),
                 silent = TRUE)
      if (!inherits(fit, "try-error")) return(fit$points)
    }
  }
  stats::prcomp(M)$x[, seq_len(k), drop = FALSE]
}

#' Measure how faithfully a two-dimensional layout keeps the distances
#'
#' @description
#' Diagnostic for whether an arrow plot can be believed. Projecting to two
#' dimensions distorts distances, and how much it distorts them in any given
#' case is an empirical question, so this measures it. For each document it
#' returns the rank correlation between the pairwise distances computed in
#' all dimensions and the pairwise distances on the page (a Shepard
#' correlation), together with the variance the plane holds.
#'
#' \strong{Never read the observed values on their own.} Both the variance
#' explained and the Shepard correlation rise mechanically as the number of
#' segments n gets smaller: centered points have rank n - 1, so at n = 3 the
#' plane is always exact (100\% of variance, correlation 1.00), and even at
#' n = 4 isotropic points return 2/(n - 1) = 67\%. The function therefore
#' also returns a null built by drawing the same number of segments at random
#' from the pool of all documents. What to read is the gap between observed
#' and null, not the size of the observed value.
#'
#' A per-document layout of a handful of segments will therefore look
#' excellent on both measures while a random pile of other people's segments
#' looks just as good, which is what the null exposes. A shared layout over
#' many documents has far more points than dimensions, so its variance and
#' correlation are low in absolute terms yet stand well clear of the
#' isotropic expectation. \strong{Use \code{scope = "shared"} whenever
#' several documents are to be compared.}
#'
#' @param emb A numeric matrix of segment embeddings, one row per segment;
#'   rows are matched to the table by \code{docname} where possible, otherwise
#'   by position.
#' @param seg A segment table satisfying the \code{\link{as_segments}}
#'   contract.
#' @param scope Character; how the layout is solved. \code{"document"} solves a
#'   separate layout for each document; \code{"shared"} solves one layout for
#'   the whole pool and reads each document's rows out of it, which is what
#'   makes panels comparable with one another. Defaults to \code{"document"}.
#' @param layout Character; either \code{"mds"} (non-metric MDS) or
#'   \code{"pca"}. MDS
#'   optimises the rank order of the distances, so it gives a more faithful
#'   plane than PCA when the number of dimensions far exceeds the number of
#'   points. \code{var_2d} is defined only for PCA, MDS having no variance
#'   explained, and is NA under MDS; fidelity is read from \code{shepard}
#'   under either. Defaults to \code{"mds"}.
#' @param null_reps Integer; the number of null draws, where 0 skips the
#'   null and returns NA in its columns. Defaults to 200.
#' @return A data frame with one row per document of at least three segments
#'   and columns \code{doc_id}, \code{n_seg}, \code{layout}, \code{var_2d}
#'   (proportion of variance in the first two components, PCA only),
#'   \code{var_null}, \code{shepard} (Spearman correlation between measured
#'   and plotted distances) and \code{shepard_null}. Under
#'   \code{scope = "shared"} with \code{layout = "pca"}, \code{var_null} is
#'   the isotropic expectation 2/(N - 1) rather than a resampled value,
#'   because the shared plane is fixed by the whole cloud and does not move
#'   when segments are redrawn.
#' @seealso \code{\link{plot_trajectory}}, \code{\link{plot_recurrence}},
#'   \code{\link{plot_arc}}
#' @export
trajectory_fidelity <- function(emb, seg, scope = c("document", "shared"),
                                layout = c("mds", "pca"), null_reps = 200) {
  scope <- match.arg(scope); layout <- match.arg(layout)
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  # 共通配置は一度だけ解く。文書ごとに解き直すとパネルが比較できない。
  shared <- if (scope == "shared") .layout_2d(emb, layout) else NULL
  N <- nrow(emb)

  # 1組の点について、平面の忠実さを返す。共通配置のときは基底が固定なので、
  # 解き直さずその行を取り出す。
  .fid <- function(k) {
    M <- emb[k, , drop = FALSE]
    if (is.null(shared)) {
      xy <- .layout_2d(M, layout)
      ve <- attr(xy, "ve")
    } else {
      xy <- shared[k, , drop = FALSE]
      ve <- attr(shared, "ve")
    }
    # コサイン距離 1-s と Euclid 距離 sqrt(2(1-s)) は単位ベクトルでは
    # 単調に対応するので、順位相関はどちらで測っても同じ。
    c(ve, suppressWarnings(stats::cor(
      stats::as.dist(1 - cos_sim_matrix(M)), stats::dist(xy),
      method = "spearman")))
  }

  do.call(rbind, lapply(unique(seg$doc_id), function(dd) {
    k <- which(seg$doc_id == dd); k <- k[order(seg$segid[k])]
    if (length(k) < 3) return(NULL)
    o <- .fid(k)
    # 帰無: 同じ本数を全文書のプールから無作為に取る。小さい n で
    # 説明率と相関が機械的に上がる分を、そのまま測って差し引けるようにする。
    nl <- c(NA_real_, NA_real_)
    if (null_reps > 0 && N > length(k)) {
      r <- vapply(seq_len(null_reps),
                  function(i) .fid(sample.int(N, length(k))), numeric(2))
      nl <- rowMeans(r, na.rm = TRUE)
      # 共通配置では平面は雲全体が決めるので、再抽出しても説明率は動かない。
      # 比べるべき相手は等方な点の期待値のほう（PCA のときだけ定義される）。
      if (!is.null(shared) && layout == "pca") nl[1] <- min(1, 2 / (N - 1))
    }
    data.frame(doc_id = dd, n_seg = length(k), layout = layout,
               var_2d = round(o[1], 3), var_null = round(nl[1], 3),
               shepard = round(o[2], 3), shepard_null = round(nl[2], 3),
               stringsAsFactors = FALSE)
  }))
}

#' Plot a document's trajectory through semantic space
#'
#' @description
#' Joins the segments of a document in the order they were spoken or written
#' and marks the direction of travel with arrows, so that where a text went,
#' and in what order, can be seen at a glance.
#'
#' \strong{This is not the display to look at first.} Look at
#' \code{\link{plot_recurrence}} and \code{\link{plot_arc}} first, neither of
#' which passes through a projection. Of the three displays this is the only
#' one that has to be read with a geometric caveat held in mind throughout, and
#' a display that needs caveats is a poor instrument for forming a first
#' impression of the data.
#'
#' @section What the picture supports:
#' \strong{The order the arrows carry is exact; what the projection distorts
#' is distance and angle.} A reading such as "it doubled back at the third
#' segment and then moved in one direction" may be taken from this plot. A
#' reading such as "A moved further than B" may not: that is a quantity
#' measured in all dimensions, and \code{\link{trajectory_stats}} is what
#' returns it. The subtitle prints how much of the distance structure the
#' projection preserves (a Shepard rank correlation) beside the null obtained
#' by drawing the same number of segments at random. Read the difference
#' between them, not the observed number.
#'
#' \strong{Distances are not unreadable; they have to be read as ranks.} All
#' three providers return unit-norm vectors, so Euclidean distance in all
#' dimensions corresponds exactly to cosine similarity s as
#' d = sqrt(2(1 - s)), their rank correlation being exactly -1, and centering
#' is a translation and leaves distances unchanged. Near and far on the page
#' therefore reproduce the cosine ordering that every other analysis here
#' uses. The scale, however, is non-linear: because d is the square root of
#' 2(1 - s), it stretches as s approaches 1, so the same step in similarity
#' spans several times more of the page between near segments than between
#' distant ones. Differences among similar segments are exaggerated and
#' differences among distant ones compressed. Read the ranks; do not read the
#' ratios.
#'
#' @section Choosing the scope:
#' The layout is solved per document by default. With few segments both the
#' variance explained and the rank correlation rise mechanically (at n = 3 a
#' plane is always exact), so the raw fidelity figures are not evidence that
#' the map can be trusted. To compare documents use \code{scope = "shared"}:
#' the basis and the axis limits are then taken from the whole pool, so
#' panels drawn one document at a time can be laid side by side and read
#' against one another. See \code{\link{trajectory_fidelity}} for the
#' diagnostic.
#'
#' @param emb A numeric matrix of segment embeddings, one row per segment;
#'   rows are matched to the table by \code{docname} where possible, otherwise
#'   by position.
#' @param seg A segment table satisfying the \code{\link{as_segments}}
#'   contract.
#' @param doc A character vector naming the document(s) to draw. Defaults to
#'   \code{NULL}, which draws them all, following \code{scope}.
#' @param scope Character; either \code{"document"} (a layout per document) or
#'   \code{"shared"} (one layout for the pool). Drawing more than one document
#'   promotes \code{"document"} to \code{"shared"} automatically. Defaults to
#'   \code{"document"}.
#' @param layout Character; either \code{"mds"} (non-metric MDS) or
#'   \code{"pca"}. MDS
#'   optimises the rank order of the distances, so it gives a more faithful
#'   plane when the number of dimensions far exceeds the number of points.
#'   There is little to gain from applying it to the handful of points in one
#'   document, three points sitting exactly in two dimensions; pair it with
#'   \code{scope = "shared"} when documents are to be compared. Defaults to
#'   \code{"mds"}.
#' @param zoom Logical; under \code{scope = "shared"}, whether to fit the
#'   axis limits to the document being drawn rather than to the whole pool.
#'   The basis stays shared, so positions keep their meaning, but the ranges
#'   then differ between panels and positions can no longer be read across
#'   them. Use it when a document that moves little collapses to a point.
#'   Defaults to \code{FALSE}.
#' @param axis An optional numeric vector with one element per embedding
#'   dimension, such as the difference between the high and low anchor means.
#'   The layout is rotated so that this direction lies as nearly as possible
#'   along the x axis. Rotation changes neither distances nor angles, so
#'   fidelity is unaffected and only the interpretation of the axes changes;
#'   it also removes the arbitrary sign and orientation that otherwise differ
#'   between providers. Defaults to \code{NULL} (no rotation).
#' @param arrows Logical; whether to draw an arrowhead on every step. Defaults
#'   to \code{TRUE}.
#' @param label Logical; whether to print the segment number inside each point.
#'   Defaults to \code{TRUE}.
#' @param text Logical; whether to print the segment texts beside the
#'   plot so that points can be matched to what was said. This is limited to
#'   a single document and to \code{text_max} segments; outside those limits,
#'   or without the \pkg{patchwork} package, the text is dropped with a
#'   warning and the plot is returned on its own. Defaults to \code{FALSE}.
#' @param text_wrap Integer; the wrapping width, in characters, for the printed
#'   texts. Defaults to 34.
#' @param text_max Integer; the largest number of segments for which the texts
#'   are printed, beyond which the panel cannot hold them. Defaults to 8.
#' @param title Character; the plot title. Defaults to \code{NULL}, which
#'   prints \code{"Trajectory through semantic space"}.
#' @param compact Logical; whether to draw small for placing plots side by
#'   side, with smaller text and points, no legend, and a one-line subtitle.
#'   Defaults to \code{FALSE}.
#' @param null_reps Integer; the number of random draws behind the fidelity
#'   null shown in the subtitle, where 0 skips it. Defaults to 200.
#' @param fidelity Logical; whether to print the fidelity line as a subtitle.
#'   When several shared-projection panels are laid out together the
#'   same figures repeat on each, so set it \code{FALSE} and state them once
#'   in the figure note. Defaults to \code{TRUE}.
#' @return A \pkg{ggplot2} object (printed when called at the top level): one
#'   point per segment numbered in narrative order, the last drawn as a square and
#'   the rest as circles, an arrow for every step, faint axes through the
#'   origin (which is the centroid of the segments), equal scaling with
#'   matching breaks on both axes, and the fidelity subtitle unless
#'   \code{fidelity = FALSE}. Several documents are faceted. With
#'   \code{text = TRUE} a \pkg{patchwork} of the plot and the segment texts
#'   is returned instead.
#' @details
#' A warning is raised when per-document planes preserve no more rank order
#' than the same number of segments drawn at random from the pool, which is
#' the usual case at survey lengths. Treat it as an instruction to read the
#' order rather than the distances, and to move to \code{scope = "shared"},
#' \code{\link{plot_recurrence}} or \code{\link{plot_arc}}.
#' @seealso \code{\link{trajectory_fidelity}}, \code{\link{trajectory_stats}},
#'   \code{\link{plot_recurrence}}. Which display to reach for first, and how
#'   to read each: \url{https://github.com/PsycholoStudio/qualembed/blob/main/docs/reading-plots.md}.
#' @export
plot_trajectory <- function(emb, seg, doc = NULL,
                            scope = c("document", "shared"),
                            layout = c("mds", "pca"), axis = NULL,
                            zoom = FALSE,
                            arrows = TRUE, label = TRUE, text = FALSE,
                            text_wrap = 34, text_max = 8, title = NULL,
                            compact = FALSE, null_reps = 200,
                            fidelity = TRUE) {
  scope <- match.arg(scope); layout <- match.arg(layout)
  a <- .align_emb(emb, seg); emb <- a$emb; seg <- a$seg
  # 共通射影の基底も軸の範囲も、描く文書ではなくプール全体から決める。
  # そうしないと、1文書ずつ描いて並べたパネル同士が比較できない。
  emb_all <- emb; seg_all <- seg; idx <- seq_len(nrow(emb))
  if (!is.null(doc)) {
    keep <- seg$doc_id %in% doc
    if (!any(keep)) stop("plot_trajectory(): no document named '",
                         paste(doc, collapse = "', '"), "'.", call. = FALSE)
    idx <- which(keep)
    emb <- emb[keep, , drop = FALSE]; seg <- seg[keep, , drop = FALSE]
  }
  docs <- unique(seg$doc_id)
  if (length(docs) > 1 && scope == "document") scope <- "shared"

  fid_pool <- trajectory_fidelity(emb_all, seg_all, scope = scope,
                                  layout = layout, null_reps = null_reps)
  fid <- fid_pool[fid_pool$doc_id %in% docs, , drop = FALSE]
  XY  <- if (scope == "shared") .layout_2d(emb_all, layout, axis)

  # 説明率も順位相関も n が小さいほど機械的に上がる。プール全体で帰無と
  # 比べ、差がないなら地図の遠近を読ませないよう呼び出し側に伝える。
  if (scope == "document" && any(!is.na(fid_pool$shepard_null))) {
    win <- mean(fid_pool$shepard > fid_pool$shepard_null, na.rm = TRUE)
    if (!is.na(win) && win <= .6)
      warning("plot_trajectory(): a per-document plane keeps no more rank ",
              "order than the same number of segments drawn at random from ",
              "the pool (observed above null in ", round(100 * win),
              "% of documents). With a median of ",
              stats::median(fid_pool$n_seg), " segments the fidelity numbers ",
              "are mostly a function of n. Read the order, not the ",
              "distances -- and use scope = \"shared\" for panels that can ",
              "be compared with one another. plot_recurrence() and plot_arc() ",
              "show the same document with no projection at all.", call. = FALSE)
  }

  # 射影: 文書ごと（既定）か、プール全体で一つか
  d <- do.call(rbind, lapply(docs, function(dd) {
    k <- which(seg$doc_id == dd); k <- k[order(seg$segid[k])]
    xy <- if (scope == "document") .layout_2d(emb[k, , drop = FALSE], layout, axis)
          else XY[idx[k], , drop = FALSE]
    data.frame(doc_id = dd, segid = seg$segid[k],
               PC1 = xy[, 1], PC2 = xy[, 2], stringsAsFactors = FALSE)
  }))

  .p2 <- function(x) sub("^0[.]", ".", sprintf("%.2f", x))
  # 共通射影の平面はプール全体が決めるので、忠実さもプール全体で報告する。
  # 1文書ぶんの数点で測った順位相関は、共通基底のもとでは揺らぎでしかない。
  .f  <- if (scope == "shared") fid_pool else fid
  .vo <- 100 * stats::median(.f$var_2d, na.rm = TRUE)
  .vn <- 100 * stats::median(.f$var_null, na.rm = TRUE)
  .so <- stats::median(.f$shepard, na.rm = TRUE)
  .sn <- stats::median(.f$shepard_null, na.rm = TRUE)
  # MDS には説明分散が無い（var_2d は NA）。PCA 用の書式をそのまま使うと
  # 副題に "NA%" が出るので、順位相関だけを報告する形に切り替える。
  .has_var <- is.finite(.vo) && is.finite(.vn)
  # 1 文書だけを描くときは帰無を作る材料が無く、shepard_null も NA になる。
  .has_null <- is.finite(.sn)
  sub <- paste(strwrap(
    if (.has_var) sprintf(
      "%s projection. PC1 + PC2 hold %.0f%% of the variance, against %.0f%% for %s, and distances on the page keep a rank correlation of %s with the distances actually measured, against %s for the same segments drawn at random. Because the vectors are unit length, that measured distance is sqrt(2(1 - s)) in the cosine similarity s every other analysis here uses: same order, compressed scale. The arrows show the order, which projection preserves exactly; their lengths are not the distances travelled.",
      if (scope == "document") "Per-document" else "Shared", .vo, .vn,
      if (scope == "document") "segments drawn at random" else "isotropic points",
      .p2(.so), .p2(.sn))
    else if (.has_null) sprintf(
      "%s projection. Distances on the page keep a rank correlation of %s with the full-dimensional ones, against %s for the same segments drawn at random.",
      if (scope == "document") "Per-document" else "Shared",
      .p2(.so), .p2(.sn))
    else sprintf(
      "%s projection. Distances on the page keep a rank correlation of %s with the full-dimensional ones. Too few documents to build a null.",
      if (scope == "document") "Per-document" else "Shared", .p2(.so)),
    width = 72), collapse = "\n")
  sub_short <- if (.has_var)
    sprintf("PC1 + PC2 = %d%% (null %d%%), rank corr. = %s (null %s)",
            round(.vo), round(.vn), .p2(.so), .p2(.sn))
  else if (.has_null) sprintf("rank corr. = %s (null %s)", .p2(.so), .p2(.sn))
  else sprintf("rank corr. = %s", .p2(.so))

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

  # 原点を通る軸を薄く敷く。主成分は中心化されているので、原点は
  # その文書のセグメントの重心であり、内外の別が読めるようになる。
  # 目盛りは両軸共通。coord_equal() だけでは、刻みが軸ごとに違うと
  # 1:1 に見えない。
  # 共通配置では軸の範囲もプール全体から取るのが既定。パネルどうしで
  # 位置を読み比べられる代わりに、動きの小さい文書は一点に潰れる。
  # zoom = TRUE は基底を共有したまま範囲だけ描く文書に合わせる。形は
  # 読めるようになるが、パネル間で位置は読めなくなる（目盛りで補う）。
  if (scope == "shared" && !isTRUE(zoom)) {
    .xl <- .yl <- range(XY, na.rm = TRUE)               # 共通の枠
  } else {
    # 描く点に枠を合わせる。x と y をまとめて一つの範囲にすると、
    # 広いほうの軸に合わせて狭いほうが引き伸ばされ、点のない側に
    # 枠が空く。1:1 を保つため幅は共通にし、中心は軸ごとに取る。
    .xr <- range(d$PC1, na.rm = TRUE); .yr <- range(d$PC2, na.rm = TRUE)
    .sp <- max(diff(.xr), diff(.yr), .Machine$double.eps) * 1.18
    .xl <- mean(.xr) + c(-.5, .5) * .sp
    .yl <- mean(.yr) + c(-.5, .5) * .sp
  }
  .rng <- range(c(.xl, .yl))
  .stp <- signif(max(diff(.xl), .Machine$double.eps) / 4, 1)
  .brk <- seq(floor(.rng[1] / .stp) * .stp, ceiling(.rng[2] / .stp) * .stp, .stp)

  p <- ggplot(d, aes(.data$PC1, .data$PC2)) +
    geom_hline(yintercept = 0, colour = "grey88", linewidth = .3) +
    geom_vline(xintercept = 0, colour = "grey88", linewidth = .3)
  # 矢印の頭が点の下に隠れないよう、線分を点の手前で止める
  if (!is.null(seg_df) && nrow(seg_df)) {
    dx <- seg_df$xend - seg_df$x; dy <- seg_df$yend - seg_df$y
    L  <- sqrt(dx^2 + dy^2); L[L == 0] <- 1
    sh <- .055 * mean(c(diff(range(d$PC1)), diff(range(d$PC2))))
    k  <- pmin(sh / L, .40)
    seg_df$x    <- seg_df$x    + dx * k; seg_df$y    <- seg_df$y    + dy * k
    seg_df$xend <- seg_df$xend - dx * k; seg_df$yend <- seg_df$yend - dy * k
  }
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
  # 番号は点の外ではなく中に置く。矢印と番号があれば始点・終点は自明なので、
  # 最後だけ四角にして残りは丸にする。塗りは白で、線が下を通っても読める。
  psz <- if (isTRUE(compact)) 3.4 else 4.6
  p <- p + geom_point(aes(colour = .data$doc_id, shape = .data$role),
                      fill = "white", size = psz, stroke = .5) +
    scale_shape_manual(values = c(start = 21, mid = 21, end = 22),
                       guide = "none") +
    labs(title = title %||% "Trajectory through semantic space",
         subtitle = if (!isTRUE(fidelity)) NULL
                    else if (isTRUE(compact)) sub_short else sub,
         x = if (is.null(axis)) .ax_lab(layout, 1) else "Anchor axis",
         y = .ax_lab(layout, 2), colour = NULL) +
    # 1:1 であることが目でわかるよう、両軸の刻みを揃える。
    scale_x_continuous(breaks = .brk) +
    scale_y_continuous(breaks = .brk) +
    coord_equal(xlim = .xl, ylim = .yl) +
    theme_minimal(base_size = if (isTRUE(compact)) 7 else 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 7.5, colour = "grey35",
                                       lineheight = 1.15),
          panel.grid = element_blank(),
          legend.position = if (isTRUE(compact)) "none" else "bottom",
          legend.box = "horizontal") +
    guides(colour = if (length(docs) > 1 && !isTRUE(compact))
                      guide_legend(order = 2) else "none",
           shape  = if (isTRUE(compact)) "none" else ggplot2::guide_legend())
  if (isTRUE(label))
    p <- p + geom_text(aes(label = .data$segid),
                       size = if (isTRUE(compact)) 2.0 else 2.7,
                       colour = "grey15")
  if (length(docs) > 1)
    p <- p + facet_wrap(~ doc_id,
                        scales = if (scope == "shared") "fixed" else "free")

  # セグメント本文の並記。番号だけの図では点と語りの対応が追えないが、
  # 本文は長さも数も文書しだいなので、収まらないときは黙って降りる。
  if (isTRUE(text)) {
    if (length(docs) > 1) {
      warning("plot_trajectory(): text = TRUE draws the segments beside one ",
              "document only; several were given, so the text is omitted. ",
              "Pass doc = to choose one.", call. = FALSE)
    } else if (nrow(d) > text_max) {
      warning("plot_trajectory(): ", nrow(d), " segments is more than ",
              "text_max = ", text_max, ", so the text is omitted; the panel ",
              "would not hold it. Raise text_max to override.", call. = FALSE)
    } else if (!requireNamespace("patchwork", quietly = TRUE)) {
      warning("plot_trajectory(): text = TRUE needs the patchwork package.",
              call. = FALSE)
    } else {
      zz  <- d[order(d$segid), ]
      # 本文は座標計算で落ちているので seg から引き直す
      key <- paste(zz$doc_id, zz$segid)
      src <- as.data.frame(seg)
      txt <- src$text[match(key, paste(src$doc_id, src$segid))]
      wr  <- vapply(txt, function(x)
                    paste(strwrap(x, text_wrap), collapse = "\n"), character(1))
      # 縦位置は折り返した行数に比例させる。等間隔だと、1行の断片も5行の
      # 断片も同じ幅の枠を取るので、短い側に空きが余り長い側は隣にかかる。
      # 行数ぶんの高さに項目間の一定の空きを足して積み上げれば、余白は
      # 出力の高さに応じて一様に伸縮し、重なりは高さ不足のときだけ起きる。
      nl  <- vapply(strsplit(wr, "\n", fixed = TRUE), length, integer(1))
      gap <- 1.2
      top <- cumsum(c(0, utils::head(nl + gap, -1)))
      lbl <- data.frame(i = zz$segid, y = -(top + (nl - 1) / 2),
                        txt = unname(wr), stringsAsFactors = FALSE)
      side <- ggplot(lbl, aes(0, .data$y)) +
        geom_text(aes(label = .data$i), hjust = 0, size = 2.2,
                  fontface = "bold") +
        geom_text(aes(x = .07, label = .data$txt), hjust = 0, size = 1.9,
                  lineheight = .95, vjust = .5) +
        scale_x_continuous(limits = c(-.02, .92)) +
        # 目盛りの範囲は中心位置ではなく、字が実際に占める上端と下端で
        # とる。中心だけで取ると、最初と最後の項目の折り返し行が枠の外に
        # 出て切れる。
        scale_y_continuous(
          limits = c(-(top[length(top)] + nl[length(nl)] - 1) - .6, .6),
          expand = expansion(mult = c(.02, .02))) +
        theme_void()
      p <- patchwork::wrap_plots(p, side, widths = c(1, 1.25))
    }
  }
  p
}

#' Plot segments on a two-dimensional projection (deprecated)
#'
#' @description
#' Deprecated: use \code{\link{plot_trajectory}} instead. The replacement marks
#' the narrative order with an arrow on every step and prints in the subtitle
#' how much of the distance structure the projection preserves. This function
#' is now a thin wrapper kept for backward compatibility.
#' @param emb,seg,line,label,title Arguments of the old interface. They are
#'   passed straight to \code{\link{plot_trajectory}}, where \code{line} becomes
#'   \code{arrows}.
#' @return The \pkg{ggplot2} object returned by
#'   \code{\link{plot_trajectory}}.
#' @seealso \code{\link{plot_trajectory}}
#' @export
plot_trajectory_2d <- function(emb, seg, line = TRUE, label = TRUE,
                               title = NULL) {
  plot_trajectory(emb, seg, arrows = line, label = label, title = title)
}


# ── 旧 API（非推奨・後方互換のためだけに残す）─────────────────

#' Measure path length in a two-dimensional projection (deprecated)
#'
#' @description
#' Deprecated, and warns when called. \strong{It measures distance in the
#' plane of PC1 and PC2, which is to say in the picture rather than in the
#' embedding space}, and a two-dimensional projection does not preserve the
#' distance structure (Chari and Pachter, 2023). Use
#' \code{\link{trajectory_stats}}, which measures in all dimensions and reports
#' a path length that can be interpreted.
#'
#' @param df A data frame: the \code{df} element of \code{\link{pca_2d}} with a
#'   person column and a time column added. Columns \code{PC1} and
#'   \code{PC2} are read from it.
#' @param person_col Character; the name of the person-identifier column.
#'   Defaults to \code{"person"}.
#' @param time_col Character; the name of the time-point column, by which rows
#'   are ordered within person. Defaults to \code{"time"}.
#' @return A data frame with one row per person: the identifier column,
#'   \code{n_timepoints}, and \code{total_dist_2d}, the summed Euclidean
#'   distance between consecutive points on the plane.
#' @seealso \code{\link{trajectory_stats}}
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

#' Plot longitudinal trajectories from a projection (deprecated)
#'
#' @description
#' Deprecated, and warns when called. Superseded by
#' \code{\link{plot_trajectory}}, which takes the embedding matrix and a
#' segment table directly, draws an arrow for every step, and prints how well
#' the projection preserves the measured distances. This function is kept only
#' for backward compatibility with version 0.2.0.
#'
#' @param df A data frame with columns \code{person}, \code{time}, \code{PC1},
#'   \code{PC2} and \code{label}, already in time order within person.
#' @param person_col Character; the name of the person-identifier column.
#'   Defaults to \code{"person"}.
#' @param title Character; the plot title. Defaults to
#'   \code{"Trajectories in semantic space"}.
#' @return A \pkg{ggplot2} object (printed when called at the top level): one
#'   path per person across the PC1-PC2 plane,
#'   with an arrowhead at its end and repelled point labels.
#' @seealso \code{\link{plot_trajectory}}
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
  p
}

#' Angular deviation of each item from its theorized position on a circumplex
#'
#' @description
#' Scores how far each item sits from the angle a circumplex theory assigns it,
#' on the plane that comes closest to those angles. The plane is the solution of
#' an orthogonal Procrustes problem rather than a search: with \eqn{X^T C = U D
#' V^T}, the rotation \eqn{Q = U_{[,1:2]} V^T} minimises the residual, so no
#' choice is made after seeing the fit. A best overall rotation is then removed,
#' which fixes the circular mean of the signed deviations at zero; the ten
#' deviations are therefore not independent of one another, and a per-item band
#' must come from permuting the labels rather than from a formula.
#'
#' The mean absolute deviation failing to beat a null does not mean the ring is
#' blurred. Item by item the function separates two different conclusions: that
#' the configuration is diffuse, and that particular items are displaced in a
#' fixed direction that several providers agree on.
#'
#' The principal components used here are a full-rank rotation and drop no
#' dimensions, so nothing is lost and the metric-versus-non-metric question that
#' arises for \code{\link{coords_2d}} does not arise.
#'
#' @param emb A numeric matrix with one row per item, as returned by
#'   \code{\link{embed}}. Rows must already be in the theoretical order around
#'   the ring.
#' @param angles Numeric; the theorized angle of each row in radians.
#'   \code{NULL}, the default, uses an evenly spaced ring.
#' @param n_perm Integer; permutations of the item labels used for the null.
#'   Defaults to 999. Zero skips the null and returns \code{NA} for the last two
#'   elements.
#' @return A list with \code{coords} (coordinates on the best plane),
#'   \code{theory} (the theorized angles), \code{observed} (the observed
#'   angles), \code{signed} (signed deviation in degrees, named by row),
#'   \code{mean_abs} (mean absolute deviation in degrees), \code{null_median}
#'   (median of the null distribution of \code{mean_abs}), and \code{p} (the
#'   one-sided permutation p for \code{mean_abs}).
#'
#'   Note that \code{null_median} and \code{p} describe the mean over all items,
#'   not one item. The null of a mean is narrower than one item's by roughly
#'   \eqn{\sqrt{n}}, so it is not a band to read a single deviation against; for
#'   that, permute the labels and keep every item's own deviation.
#' @seealso \code{\link{mantel_test}}, which scores the ring order from
#'   distances rather than angles and carries the confirmatory weight.
#' @examples
#' \dontrun{
#' vals <- c("self-direction", "stimulation", "hedonism", "achievement",
#'           "power", "security", "conformity", "tradition",
#'           "benevolence", "universalism")
#' emb  <- embed(vals, provider = "gemini")
#' cd  <- circumplex_deviation(emb, n_perm = 999)
#' round(cd$signed, 1)
#' }
#' @export
circumplex_deviation <- function(emb, angles = NULL, n_perm = 999) {
  n   <- nrow(emb)
  ang <- if (is.null(angles)) (seq_len(n) - 1) * 2 * pi / n else angles
  stopifnot(length(ang) == n)
  X <- stats::prcomp(emb, center = TRUE)$x[
         , seq_len(min(n - 1, ncol(emb))), drop = FALSE]
  C <- scale(cbind(cos(ang), sin(ang)), scale = FALSE)
  .pa <- function(Z) {
    sv <- svd(crossprod(Z, C))
    Y  <- Z %*% (sv$u[, 1:2] %*% t(sv$v))
    th <- atan2(Y[, 2], Y[, 1])
    d  <- atan2(sin(th - ang), cos(th - ang))
    o  <- atan2(mean(sin(d)), mean(cos(d)))       # align by the best overall rotation
    sg <- atan2(sin(th - ang - o), cos(th - ang - o)) * 180 / pi
    R  <- matrix(c(cos(o), sin(o), -sin(o), cos(o)), 2)
    list(xy = Y %*% R, theta = th - o, signed = sg, mean_abs = mean(abs(sg)))
  }
  obs <- .pa(X)
  nl <- if (n_perm > 0)
    vapply(seq_len(n_perm),
           function(i) .pa(X[sample(n), , drop = FALSE])$mean_abs, numeric(1))
    else NA_real_
  rownames(obs$xy) <- rownames(emb)
  list(coords = obs$xy, theory = ang, observed = obs$theta,
       signed = stats::setNames(obs$signed, rownames(emb)),
       mean_abs = obs$mean_abs,
       null_median = if (n_perm > 0) stats::median(nl) else NA_real_,
       p = if (n_perm > 0) (1 + sum(nl <= obs$mean_abs)) / (n_perm + 1) else NA_real_)
}
