#' qualembed: embedding-based semantic measurement for survey and qualitative text
#'
#' A thin, auditable toolkit accompanying the paper \emph{Embedding
#' qualitative data in LLM semantic space}. Provides a unified interface to
#' commercial text-embedding APIs (Gemini, Voyage AI, OpenAI) and the
#' calibration statistics used in the paper: permutation-tested
#' within-between contrasts, Ward clustering with adjusted Rand indices,
#' Mantel congruence, Procrustes alignment with per-item residuals, and
#' theory-anchored semantic projection. Also segments long documents into
#' measurement units and summarises their trajectories. Pure R: no Python,
#' no GPU; sentence and word boundaries come from ICU via \pkg{stringi}, so
#' English and Japanese are handled by the same call.
#'
#' @import ggplot2
#' @import httr2
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr arrange group_by summarise bind_rows rename n .data
#' @importFrom vegan mantel procrustes protest
#' @importFrom stats prcomp dist hclust cutree cor residuals as.dist setNames
#' @importFrom stringi stri_split_boundaries stri_locate_all_boundaries stri_count_boundaries stri_opts_brkiter
#' @importFrom stats sd median quantile ave
#' @importFrom utils head read.csv unzip
#' @keywords internal
"_PACKAGE"

# NULL-coalescing helper (internal)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nchar(a[1]) > 0) a else b
