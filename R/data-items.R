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
#' @format See the source for structure.
#' @export
get_bfi_items <- function() {
  if (!requireNamespace("psych", quietly = TRUE))
    stop("psych パッケージが必要です: install.packages('psych')")

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
#' @format See the source for structure.
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
#' @format See the source for structure.
#' @export
panas_ja_validated <- c(
  PA01 = "興味のある",      # interested
  PA02 = "興奮した",        # excited
  PA03 = "強気な",          # strong
  PA04 = "熱狂した",        # enthusiastic
  PA05 = "誇らしい",        # proud
  PA06 = "機敏な",          # alert
  PA07 = "やる気がわいた",  # inspired
  PA08 = "決心した",        # determined
  PA09 = "注意深い",        # attentive
  PA10 = "活気のある",      # active
  NA01 = "苦悩した",        # distressed
  NA02 = "うろたえた",      # upset
  NA03 = "うしろめたい",    # guilty
  NA04 = "おびえた",        # scared
  NA05 = "敵意をもった",    # hostile
  NA06 = "イライラした",    # irritable
  NA07 = "恥ずかしい",      # ashamed
  NA08 = "ぴりぴりした",    # nervous  ※Table 2 目視確認済み（項目16）
  NA09 = "神経質な",        # jittery  ※Table 2 目視確認済み（項目19）
  NA10 = "恐れた"           # afraid
)


# ── Schwartz 基本的価値観 10項目（Schwartz, 1992）───────────
# ベクトルの並び順 = 理論的円環順序（SD→ST→…→UN→SDと一周）
#' Schwartz value descriptions (English) and the theoretical ring order
#'
#' @format See the source for structure.
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
#' @format See the source for structure.
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
    A = list(high = c("嬉しい", "喜ばしい", "楽しい"),
             low  = c("悲しい", "不幸な", "惨めな")),
    B = list(high = c("ポジティブな", "良い", "快い"),
             low  = c("ネガティブな", "悪い", "不快な")),
    C = list(high = c("喜びに満ちた"),
             low  = c("陰鬱な"))
  )
)

# 職業威信軸（基準: car::Prestige の Pineo-Porter 威信スコア）
# アンカーは「高／低威信の仕事を表す句」であり、データセット内の職業名は使わない。
#' Pre-specified anchor sets for the occupational-prestige axis
#'
#' @format See the source for structure.
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
