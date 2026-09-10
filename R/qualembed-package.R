#' qualembed: embedding-based semantic measurement for survey and qualitative text
#'
#' A thin, auditable toolkit accompanying the paper \emph{Embedding
#' qualitative data in LLM semantic space}. Provides a unified interface to
#' commercial text-embedding APIs (Gemini, Voyage AI, OpenAI) and a set of
#' calibration statistics: permutation-tested
#' within-between contrasts, Ward clustering with adjusted Rand indices,
#' Mantel congruence, Procrustes alignment with per-item residuals, and
#' theory-anchored semantic projection. Also segments long documents into
#' measurement units and summarises their trajectories. Pure R: no Python,
#' no GPU; sentence and word boundaries come from ICU via \pkg{stringi}, so
#' English and Japanese are handled by the same call.
#'
#' @section Where to start:
#' \code{\link{check_api}} confirms a key works. \code{\link{embed}} turns
#' texts into a matrix and caches the result, so a second run costs no API
#' calls; \code{\link{cache_info}} says what the cache holds and where it is.
#'
#' \code{\link{cos_sim_matrix}} turns that matrix into pairwise similarity.
#' Watch which of the two each function then wants.
#' \code{\link{within_between_sim}} and \code{\link{test_delta}} take the
#' similarity matrix; \code{\link{test_ari}} and \code{\link{coords_2d}}
#' take the embedding matrix, and \code{\link{test_ari_sim}} is the variant
#' of \code{\link{test_ari}} that takes a similarity matrix instead. Where
#' the same items are measured in two spaces, in two languages or under two
#' providers, \code{\link{mantel_test}} compares the two structures and is
#' the primary statistic for that question, with
#' \code{\link{procrustes_m2}} reported alongside it as description; both
#' take similarity or distance matrices.
#' \code{\link{semantic_projection}} scores texts on an axis named in
#' advance, from the embedding matrix, and \code{\link{coords_2d}} lays a
#' set of texts out for looking at, which is not the same as measuring them.
#'
#' Long answers are cut into units by \code{\link{segment_text}} and carried
#' by the \code{\link{as_segments}} contract;
#' \code{\link{trajectory_stats}} summarises the path through the space and
#' \code{\link{trajectory_null}} tests whether the order matters. Read its
#' help before relying on a non-significant result: at survey lengths the
#' arithmetic bounds how small the p can get.
#'
#' @section What the geometry does not carry:
#' Embeddings are not spread around the origin. Any two texts sit at a high
#' cosine whatever they mean, so a small difference between groups is the
#' normal case and not evidence of a weak effect: read
#' \code{delta_std} and the permutation test rather than the raw difference.
#' Opposition is the harder loss. Cosines are almost never negative even
#' between texts that mean opposite things, so a similarity matrix cannot by
#' itself tell agreement from disagreement.
#' \code{\link{centered_sim_matrix}} restores negative values but does not
#' repair the sign.
#'
#' @import ggplot2
#' @import httr2
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr arrange group_by summarise bind_rows rename n .data
#' @importFrom vegan mantel procrustes protest
#' @importFrom stats prcomp dist hclust cutree cor residuals as.dist setNames
#' @importFrom stringi stri_split_boundaries stri_locate_all_boundaries stri_count_boundaries stri_opts_brkiter
#' @importFrom stats sd median quantile ave
#' @importFrom utils head read.csv unzip write.csv packageVersion
#' @keywords internal
"_PACKAGE"

# ggplot2 の aes() が参照する列名。R CMD check は式の中の裸の名前を
# 束縛のない大域変数と読むので、ここで宣言して黙らせる。
utils::globalVariables(c("PC1", "PC2", "group", "label", "sim", "x", "y"))

# NULL-coalescing helper (internal)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nchar(a[1]) > 0) a else b
