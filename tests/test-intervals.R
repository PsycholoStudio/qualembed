# 区間推定の検査
#
# ここで固定するのは 2 点。
#   (1) cor_jackknife() が jackknife_ci() と同じ値を返すこと。
#       前者は積和の downdate で 1 観測抜きを求めるので速いが、
#       「速いから使う」ではなく「一致を確かめた上で使う」ようにする。
#   (2) jackknife_ci(cores > 1) が逐次と同じ値を返すこと。
#
# 実行: R CMD check、または  Rscript tests/test-intervals.R

# インストール済みに関数があればそれを、無ければソースを読む
if (requireNamespace("qualembed", quietly = TRUE) &&
    "cor_jackknife" %in% getNamespaceExports("qualembed")) {
  library(qualembed)
} else {
  d <- if (dir.exists("R")) "R" else file.path("..", "R")
  for (f in list.files(d, "[.]R$", full.names = TRUE)) suppressWarnings(source(f))
}

set.seed(20260903)
n <- 400; p <- 6
X <- matrix(round(runif(n * p, 1, 6)), n, p)      # 有界な整数尺度（評定を模す）
X[sample(length(X), 40)] <- NA                    # 欠損を入れて pairwise を効かせる
g <- rep(1:2, each = p / 2)

dstd <- function(S, g) {
  ut <- upper.tri(S); same <- outer(g, g, "==")[ut]
  (mean(S[ut][same]) - mean(S[ut][!same])) / sd(S[ut][!same])
}

# (1) downdate と再計算の一致 ------------------------------------------------
fast <- cor_jackknife(X, function(R) dstd(abs(R), g))
slow <- jackknife_ci(nrow(X), function(k) {
  R <- abs(cor(X[k, ], use = "pairwise.complete.obs")); dstd(R, g) })

d <- max(abs(fast - slow))
cat(sprintf("cor_jackknife vs jackknife_ci: 最大差 %.3e\n", d))
stopifnot(d < 1e-10)

# 相関行列そのものの再構成も見ておく（統計量を通す前の段階）
M <- !is.na(X); Z <- X; Z[!M] <- 0; Mn <- M + 0
N <- crossprod(Mn); Sxy <- crossprod(Z)
Sx <- crossprod(Z, Mn); Sxx <- crossprod(Z^2, Mn)
v <- N * Sxx - Sx^2
R1 <- (N * Sxy - Sx * t(Sx)) / sqrt(v * t(v)); diag(R1) <- 1
R2 <- cor(X, use = "pairwise.complete.obs")
cat(sprintf("相関行列の再構成:            最大差 %.3e\n", max(abs(R1 - R2))))
stopifnot(max(abs(R1 - R2)) < 1e-12)

# (2) 並列と逐次の一致 --------------------------------------------------------
if (.Platform$OS.type == "unix" && parallel::detectCores() > 1) {
  th  <- function(k) dstd(abs(cor(X[k, ], use = "pairwise.complete.obs")), g)
  a <- jackknife_ci(nrow(X), th, blocks = rep(1:40, each = 10), cores = 1)
  b <- jackknife_ci(nrow(X), th, blocks = rep(1:40, each = 10), cores = 2)
  cat(sprintf("cores=1 vs cores=2:          最大差 %.3e\n", max(abs(a - b))))
  stopifnot(identical(a, b) || max(abs(a - b)) < 1e-12)
}

# (3) loo_range が振れ幅を返すこと -------------------------------------------
lr <- loo_range(p, function(i) mean(abs(R2[i, i])[upper.tri(diag(length(i)))]))
stopifnot(lr[["lo"]] <= lr[["obs"]], lr[["obs"]] <= lr[["hi"]], lr[["n_unit"]] == p)

cat("区間推定の検査: すべて通過\n")
