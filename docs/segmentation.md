# Cutting long documents by hand

Part of the [qualembed](../README.md) documentation. Read
`?segment_text` and `?as_segments` first; this page is for the case where
you cut in a spreadsheet or a CAQDAS tool and bring the result back.

### Cutting by hand

Syntactical units — words, sentences, paragraphs — are the only kind a program
can find. Krippendorff's other four (physical, categorial, propositional,
thematic) are defined by what the text *means*, and there is no automating them.
Most qualitative work needs those, so the normal path into this package is a
spreadsheet, not `segment_text()`.

There is also no established criterion for how large a unit should be. Graneheim
and Lundman put the tradeoff well: too broad and one unit carries several
meanings, too narrow and the account fragments. Bengtsson is blunter — there are
no rules. Since the package cannot choose for you, **write your rule down before
you start**, and report it. Segmentation changes every number downstream; leaving
it unreported is like not reporting how you cleaned your data.

**Minimal procedure.**

1. Write the unit rule in one sentence, and name the kind of unit
   (syntactical / propositional / thematic) if you can.
2. One transcript per file, UTF-8.
3. Optionally rough-cut with `segment_text()`, then export **with a BOM** so the
   file opens cleanly in Excel: `readr::write_excel_csv()`. `write.csv()` does
   not write one.
4. Edit in the spreadsheet, one row per segment. To split, insert a row. To
   merge, mark a `merge_up` column rather than deleting a row — a deleted row
   leaves no trace of itself.
5. Read back with `read_segments()`, which strips the byte-order mark and
   checks the encoding for you, apply the merges, and let
   `as_segments(df, renumber = TRUE)` renumber.
6. Check segment counts and the `n_char` range before you spend any API calls.

```r
# rough cut -> spreadsheet
seg <- segment_text(transcripts, ids = pid)
seg$merge_up <- ""
readr::write_excel_csv(seg[, c("doc_id", "segid", "text", "merge_up")],
                       "to_edit.csv")

# ... edit by hand ...

# back into R
back <- read_segments("to_edit.csv")   # strips the BOM, checks the encoding
back$grp <- ave(back$merge_up, back$doc_id,
                FUN = function(m) cumsum(m != "x"))
fused <- aggregate(text ~ doc_id + grp, data = back, FUN = paste, collapse = "")
fused <- fused[order(fused$doc_id, as.integer(fused$grp)), ]
seg <- as_segments(fused[, c("doc_id", "text")], renumber = TRUE)
```

**Japanese and English are handled by the same call.** Sentence splitting and
word counting use ICU boundary analysis (via `stringi`), so `。！？` and `.!?`
both work and Japanese word counts are morpheme-based rather than
whitespace-based. Two things to know anyway. A short abbreviation list protects
`Dr.`, `e.g.` and friends from being read as sentence ends; extend it with
`abbrev = c(qe_abbreviations(), "Univ")` or switch it off with
`abbrev = character(0)`. And an ICU "word" in Japanese is a morpheme, so the
same content yields more words than its English translation — `size = 50` is
not the same window in the two languages. **If you are comparing English and
Japanese, cut by sentence**, the one unit whose count a translation tends to
preserve. If you are cutting because of a token limit, use
`by = "chars"`, the one unit whose size means the same thing in both.

**Japanese CSV, four ways to lose data.**

- UTF-8 without a BOM opens as mojibake in Japanese Excel. Save as "CSV UTF-8",
  or write with `readr::write_excel_csv()`.
- A Shift_JIS/CP932 file read as UTF-8 does not merely garble — the affected
  rows vanish, and in a mixed English/Japanese file the loss is *partial* and
  quiet. `read_segments()` stops when it sees this, but if a segment count falls
  anywhere else, suspect the encoding first.
- Saving as Shift_JIS silently drops ①–⑳, ～, —, and emoji.
- Excel truncates a cell at 32,767 characters (Google Sheets at 50,000) — that
  is characters, not bytes. A long uncut narrative can exceed it.

Also: keep `doc_id` alphabetic (`P01`, not `01`) or Excel eats the leading zero,
and de-duplicate CAQDAS exports — Taguette repeats a highlight once per tag, so
a multi-tagged segment would otherwise be embedded several times.

### Reporting agreement on the cutting

If two people segmented, say so and give a number.

`quallmer::qlm_compare()` (v0.4.0, 2026) computes Krippendorff's alpha for
unitizing in R — the four variants of Krippendorff et al. (2016). It needs no
LLM and no API key. `irr`, `icr`, `krippendorffsalpha` and `DescTools` do **not**
do unitizing; they assume the units are already given. Mathet's gamma exists only
in Python (`pygamma-agreement`), and Krippendorff's own u-Alpha is a standalone
Java tool.

**One trap.** `alpha_u_binary` measures agreement on which spans are material
versus gap. If your segments exhaust the transcript — no gaps, which is the
normal case here, and automatic in Japanese where nothing separates sentences —
it is undefined and returns `NA`, *even for two identical segmentations*. It is
also blind to boundaries between adjacent segments. For exhaustive segmentation
report either the nominal variants (which need a code column, and where
`alpha_cu_nominal` separates coding disagreement from boundary disagreement) or
a plain boundary-set statistic of your own.

The practical minimum, if you do nothing else: state the rule, state how many
people applied it, and state how disagreements were resolved.

