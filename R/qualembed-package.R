#' qualembed: embedding-based semantic measurement for survey and qualitative text
#'
#' A thin, auditable toolkit accompanying the paper \emph{Embedding
#' qualitative data in LLM semantic space}. Provides a unified interface to
#' commercial text-embedding APIs (Gemini, Voyage AI, OpenAI) and the
#' calibration statistics used in the paper: permutation-tested
#' within-between contrasts, Ward clustering with adjusted Rand indices,
#' Mantel congruence, Procrustes alignment with per-item residuals, and
#' theory-anchored semantic projection. Pure R: no Python, no GPU.
#'
#' @import ggplot2
#' @import httr2
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr arrange group_by summarise bind_rows rename n .data
#' @importFrom vegan mantel procrustes protest
#' @importFrom stats prcomp dist hclust cutree cor residuals as.dist setNames
#' @importFrom utils head
#' @keywords internal
"_PACKAGE"

# NULL-coalescing helper (internal)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nchar(a[1]) > 0) a else b
