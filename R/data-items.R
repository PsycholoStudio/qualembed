# ============================================================
# items_data.R
# 全デモで共有する項目テキスト・グループ構造・アンカーセット
#
# データ入手元（すべてRパッケージ内蔵または論文本文から入手可能）:
#   - Big Five 25項目: psych::bfi.dictionary（英語原文をパッケージから取得）
#   - PANAS 20項目:    Watson, Clark, & Tellegen (1988) 表1
#   - Schwartz 10価値: Schwartz (1992) の定義文
#   - 日本語:          PANAS のみ。妥当化済み尺度（川人ら, 2011）を使用する。
#
# 日本語版 BFI・Schwartz について（2026-07-31 撤回）:
#   以前のバージョンには BFI と Schwartz の日本語訳が含まれていたが、
#   これらは公刊された妥当化済み翻訳ではなく、執筆中に言語モデルが
#   生成したものだった。未妥当化の翻訳を測定材料として用いることは
#   できないため、分析から完全に除去した。日英対比は、公刊された
#   妥当化済み対訳が存在する PANAS のみが担う。
#
# 使い方: source("items_data.R")
# ============================================================

# ── Big Five（psych::bfi の25項目）──────────────────────────
# 項目文は psych パッケージ内蔵の bfi.dictionary から直接取得する。
# 戻り値: list(en, ja, factor, factor_labels, reversed)
#' Big Five item texts (English, from psych::bfi.dictionary)
#'
#' @return A list of five: \code{$en}, a named character vector of the
#'   twenty-five item texts (names are the \pkg{psych} item codes A1, A2, ...);
#'   \code{$ja}, \code{NULL}; \code{$factor}, the factor each item belongs to;
#'   \code{$factor_labels}, the five factor names; and \code{$reversed}, the
#'   codes of the seven reverse-keyed items.
#' @examples
#' if (requireNamespace("psych", quietly = TRUE)) {
#'   b <- get_bfi_items()
#'   str(b, max.level = 1)
#'   head(b$en, 3)       # the texts to embed
#'   table(b$factor)     # the grouping to test against
#' }
#' @export
get_bfi_items <- function() {
  if (!requireNamespace("psych", quietly = TRUE))
    stop("the psych package is required: install.packages('psych')")

  dict <- psych::bfi.dictionary[1:25, ]
  items_en <- setNames(as.character(dict$Item), rownames(dict))

  list(
    en            = items_en,
    ja            = NULL,   # 撤回（ファイル冒頭の注記を参照）
    factor        = substr(names(items_en), 1, 1),
    factor_labels = c(A = "Agreeableness", C = "Conscientiousness",
                      E = "Extraversion",  N = "Neuroticism",
                      O = "Openness"),
    # 因子内で逆転採点される項目（psych::bfi の標準採点キーに基づく）。
    # 注意: bfi.dictionary$Keying は Big6（情緒安定性）方向のキーで
    #       N1〜N5 がすべて -1 になるため、逆転項目の判定には使えない。
    reversed      = c("A1", "C4", "C5", "E1", "E2", "O2", "O5")
  )
}


# ── PANAS 20項目（Watson, Clark, & Tellegen, 1988）─────────
#' PANAS item texts (EN original; JA = validated Japanese scale) and affect labels
#'
#' @format A list of three: \code{$en} and \code{$ja}, each a named character
#'   vector of the twenty terms (PA01-PA10, NA01-NA10), and \code{$affect}, a
#'   character vector marking each term positive or negative.
#' @export
panas_items <- list(
  en = c(
    PA01 = "interested",      PA02 = "excited",
    PA03 = "strong",          PA04 = "enthusiastic",
    PA05 = "proud",           PA06 = "alert",
    PA07 = "inspired",        PA08 = "determined",
    PA09 = "attentive",       PA10 = "active",
    NA01 = "distressed",      NA02 = "upset",
    NA03 = "guilty",          NA04 = "scared",
    NA05 = "hostile",         NA06 = "irritable",
    NA07 = "ashamed",         NA08 = "nervous",
    NA09 = "jittery",         NA10 = "afraid"
  ),
  # 日本語版は妥当化済み尺度を使用する（下の panas_ja_validated）。
  # 開発初期にはスクリプト検証用の仮訳を用いたが、報告には使用しない。
  ja = NULL
)
panas_items$affect <- ifelse(startsWith(names(panas_items$en), "PA"),
                             "Positive Affect", "Negative Affect")

# 日本語材料は妥当化済み尺度（下で定義）を指す（ファイル末尾で代入）

# 日本語版PANAS（妥当化済み尺度・本デモの日本語材料）
# 出典: 川人潤子・大塚泰正・甲斐田幸佐・中田光紀 (2011). 日本語版
#   The Positive and Negative Affect Schedule (PANAS) 20項目の信頼性と
#   妥当性の検討. 広島大学心理学研究, 11, 225–240.
#   https://doi.org/10.15027/32396
# 項目文は同論文の Table 2 および付録質問紙（p. 240）より転記（目視照合済み）。
# 名前は対応する英語原項目のID（panas_items$en と同順）。
#' Validated Japanese PANAS item wordings (Kawahito et al., 2011)
#'
#' @format A named character vector of the twenty Japanese terms, named
#'   PA01-PA10 and NA01-NA10 to match \code{panas_items$en} one for one.
#' @export
panas_ja_validated <- c(
  PA01 = "\u8208\u5473\u306e\u3042\u308b",      # interested
  PA02 = "\u8208\u596e\u3057\u305f",        # excited
  PA03 = "\u5f37\u6c17\u306a",          # strong
  PA04 = "\u71b1\u72c2\u3057\u305f",        # enthusiastic
  PA05 = "\u8a87\u3089\u3057\u3044",        # proud
  PA06 = "\u6a5f\u654f\u306a",          # alert
  PA07 = "\u3084\u308b\u6c17\u304c\u308f\u3044\u305f",  # inspired
  PA08 = "\u6c7a\u5fc3\u3057\u305f",        # determined
  PA09 = "\u6ce8\u610f\u6df1\u3044",        # attentive
  PA10 = "\u6d3b\u6c17\u306e\u3042\u308b",      # active
  NA01 = "\u82e6\u60a9\u3057\u305f",        # distressed
  NA02 = "\u3046\u308d\u305f\u3048\u305f",      # upset
  NA03 = "\u3046\u3057\u308d\u3081\u305f\u3044",    # guilty
  NA04 = "\u304a\u3073\u3048\u305f",        # scared
  NA05 = "\u6575\u610f\u3092\u3082\u3063\u305f",    # hostile
  NA06 = "\u30a4\u30e9\u30a4\u30e9\u3057\u305f",    # irritable
  NA07 = "\u6065\u305a\u304b\u3057\u3044",      # ashamed
  NA08 = "\u3074\u308a\u3074\u308a\u3057\u305f",    # nervous  ※Table 2 目視確認済み（項目16）
  NA09 = "\u795e\u7d4c\u8cea\u306a",        # jittery  ※Table 2 目視確認済み（項目19）
  NA10 = "\u6050\u308c\u305f"           # afraid
)


# ── Schwartz 基本的価値観 10項目（Schwartz, 1992）───────────
# ベクトルの並び順 = 理論的円環順序（SD→ST→…→UN→SDと一周）
#' Schwartz value descriptions (English) and the theoretical ring order
#'
#' @format A list of three: \code{$en}, a named character vector of the ten
#'   value descriptions; \code{$ja}, \code{NULL} (no validated translation is
#'   used); and \code{$ring_order}, the theoretical circular order of the ten.
#' @export
schwartz_items <- list(
  en = c(
    SD = "self-direction: independent thought and action, freedom to choose",
    ST = "stimulation: excitement, novelty, and challenge in life",
    HE = "hedonism: pleasure, enjoyment, and sensuous gratification",
    AC = "achievement: personal success through demonstrating competence",
    PO = "power: social status, prestige, and control over people",
    SE = "security: safety, harmony, and stability of society",
    CO = "conformity: restraint from actions that violate social norms",
    TR = "tradition: respect and commitment to cultural and religious customs",
    BE = "benevolence: preserving and enhancing the welfare of close others",
    UN = "universalism: understanding and tolerance of all people and nature"
  ),
  ja = NULL,   # 撤回（ファイル冒頭の注記を参照）
  ring_order = c("SD", "ST", "HE", "AC", "PO", "SE", "CO", "TR", "BE", "UN")
)


# ── Semantic projection 用アンカーセット ─────────────────────
# Method セクションの事前指定に対応: 各射影について複数のアンカーセットを
# 用意し、全セットの結果を報告する（Kozlowski et al., 2019; Grand et al., 2022）。

# PANAS の valence 軸（基準: Warriner et al., 2013 の valence 規範）
#' Pre-specified anchor sets for the valence axis (EN/JA)
#'
#' @format A list of two languages, \code{$en} and \code{$ja}. Each holds
#'   anchor sets \code{$A} (the primary, three words per pole), \code{$B} (an
#'   alternative wording, also three per pole) and \code{$C} (one word per
#'   pole, the thinned sensitivity set). Each set is a list of \code{$high}
#'   and \code{$low} character vectors.
#' @export
valence_anchors <- list(
  en = list(
    A = list(high = c("happy", "pleased", "delighted"),
             low  = c("sad", "unhappy", "miserable")),
    B = list(high = c("positive", "good", "pleasant"),
             low  = c("negative", "bad", "unpleasant")),
    C = list(high = c("joyful"),
             low  = c("gloomy"))
  ),
  ja = list(
    A = list(high = c("\u5b09\u3057\u3044", "\u559c\u3070\u3057\u3044", "\u697d\u3057\u3044"),
             low  = c("\u60b2\u3057\u3044", "\u4e0d\u5e78\u306a", "\u60e8\u3081\u306a")),
    B = list(high = c("\u30dd\u30b8\u30c6\u30a3\u30d6\u306a", "\u826f\u3044", "\u5feb\u3044"),
             low  = c("\u30cd\u30ac\u30c6\u30a3\u30d6\u306a", "\u60aa\u3044", "\u4e0d\u5feb\u306a")),
    C = list(high = c("\u559c\u3073\u306b\u6e80\u3061\u305f"),
             low  = c("\u9670\u9b31\u306a"))
  )
)

# 職業威信軸（基準: car::Prestige の Pineo-Porter 威信スコア）
# アンカーは「高／低威信の仕事を表す句」であり、データセット内の職業名は使わない。
#' Pre-specified anchor sets for the occupational-prestige axis
#'
#' @format A list of three anchor sets, \code{$A} (the primary, three phrases
#'   per pole), \code{$B} and \code{$C} (the thinned sensitivity sets). Each is
#'   a list of \code{$high} and \code{$low} character vectors. English only.
#' @export
prestige_anchors <- list(
  A = list(high = c("a highly respected profession",
                    "a prestigious occupation",
                    "an occupation with high social standing"),
           low  = c("a lowly regarded job",
                    "a menial occupation",
                    "an occupation with low social standing")),
  B = list(high = c("high-status work"),
           low  = c("low-status work")),
  C = list(high = c("an admired and esteemed occupation"),
           low  = c("a disrespected occupation that people look down on"))
)

# 日本語PANASの正規材料 = 妥当化済み尺度
panas_items$ja <- panas_ja_validated
stopifnot(identical(names(panas_items$ja), names(panas_items$en)))
