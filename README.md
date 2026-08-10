# qualembed

Turn text into coordinates, then do statistics on it — in R, with no Python, no GPU,
and no model training.

`qualembed` sends text to a commercial embedding API and gets back a numeric vector
for each piece of text. Texts that mean similar things land near each other, so
occupation names, open-ended answers, or scale items become something you can
correlate, cluster, and test. The package wraps three providers behind one function
and adds the statistics used in the companion paper, *Embedding qualitative data in
LLM semantic space: An intellectual lineage, R tutorial, and empirical calibration*.

Nothing is generated and no respondents are simulated. Your participants' own words
stay the data; the model is a measuring instrument. Every matrix it returns records
which model produced it, on what date, and from exactly which strings — so an
archived result can be traced back to its text, and rechecked, months later.

---

**Contents.**
[1 Install](#1-install) ·
[2 Get an API key](#2-get-an-api-key) ·
[3 Store the key](#3-store-the-key-so-r-can-find-it) ·
[4 Check it works](#4-check-that-it-works) ·
[5 First embedding](#5-your-first-embedding) ·
[6 Your own data](#6-using-your-own-survey-data) ·
[7 Cost and caching](#7-what-it-costs-and-how-the-cache-saves-you-money) ·
[8 The statistics](#8-the-statistics) ·
[9 Long documents](#9-long-documents-interviews-diaries-transcripts) ·
[10 Providers](#10-choosing-a-provider) ·
[11 Troubleshooting](#11-when-something-goes-wrong) ·
[12 Participant data](#12-before-you-send-participant-data)

---

## 1. Install

You need R 4.1 or later. From the R console:

```r
install.packages("remotes")                          # once
remotes::install_github("PsycholoStudio/qualembed")
library(qualembed)
```

The only hard dependency is `httr2`, which handles the web requests. Plotting uses
`ggplot2` and `ggrepel`; Mantel tests use `vegan`. `remotes` installs these for you.

## 2. Get an API key

An API key is a password that identifies you to the provider. You need exactly one of
the three below. All three have an allowance that is more than enough to work through
this README and most small studies.

**Gemini (Google)** — easiest to start with, most generous free tier.
Go to <https://aistudio.google.com/apikey>, sign in, click **Create API key**, copy
the string.

**Voyage AI** — Anthropic's recommended embedding provider.
Go to <https://dash.voyageai.com>, create an account, create a key under **API Keys**.

**OpenAI** — most widely used, but requires a payment method before any request
succeeds. Embeddings are cheap (cents for thousands of texts); there is no free tier.
Go to <https://platform.openai.com/api-keys>, add billing, create a secret key — you
can only see it once.

Keep the key somewhere safe. Anyone who has it can spend your allowance.

## 3. Store the key so R can find it

**Do not paste your key into a script.** Scripts get shared, emailed, and committed to
repositories. Put the key in a file R reads at startup instead.

That file is `.Renviron`, in your home directory. The easiest way to open it:

```r
install.packages("usethis")   # once
usethis::edit_r_environ()     # opens ~/.Renviron in your editor
```

Add one line — no quotes, no spaces around the `=`:

```
GEMINI_API_KEY=AIzaSy...your_key_here
```

Use `VOYAGE_API_KEY=` or `OPENAI_API_KEY=` instead if you chose one of those. You can
have all three.

Save the file, then **restart R** (RStudio: Session → Restart R). Environment
variables are read only at startup, so the key is invisible until you do.

Check that R can see it:

```r
nchar(Sys.getenv("GEMINI_API_KEY"))   # a number > 30, not 0
```

If it prints `0`: the file was not saved, R was not restarted, or the name is
misspelled. Print the line back to yourself with `readLines("~/.Renviron")` — a stray
space, or a smart quote pasted from a webpage, is the usual culprit.

<details>
<summary>Without <code>usethis</code></summary>

`~/.Renviron` is a plain text file; `file.edit("~/.Renviron")` opens it. On Windows
the home directory is usually `C:\Users\yourname\Documents`. If the file does not
exist, creating it is enough — R reads it at next startup.
</details>

## 4. Check that it works

```r
check_api("gemini")
```

This embeds one short sentence and reports the model and the number of dimensions. If
it fails, the message says what to do in plain language rather than showing an HTTP
status code. Section 11 lists the common cases.

## 5. Your first embedding

```r
library(qualembed)

occupations <- c("physician", "nurse", "teacher", "carpenter", "lawyer")
emb <- embed(occupations)

dim(emb)          # 5 rows (one per text) x 3072 columns (the coordinates)
rownames(emb)     # the texts themselves
```

`embed()` returns a matrix: one row per text, one column per dimension. You will
almost never look at the numbers directly. What you want is how close the texts are
to one another:

```r
sim <- cos_sim_matrix(emb)
round(sim, 2)
```

Cosine similarity runs from −1 to 1. In practice these values are all high and
positive — a known property of embedding spaces, not a bug. **Never interpret an
absolute similarity**; compare them with each other. Is `physician`–`nurse` higher
than `physician`–`carpenter`? That comparison is meaningful.

A quick map:

```r
plot_embedding_2d(pca_2d(emb), labels = rownames(emb))
```

Treat the map as a sketch. Two dimensions cannot hold what 3,072 encode, and the
paper explains why distances on such a plot should not be measured.

## 6. Using your own survey data

Say you asked an open-ended question and have the answers in a spreadsheet.

**Save the file as CSV with UTF-8 encoding.** This matters if your text is Japanese,
or has accented characters, or curly quotes from Word. In Excel: File → Save As →
*CSV UTF-8*. Getting this wrong produces garbled text that embeds as nonsense, and
the numbers will look fine while meaning nothing.

```r
d <- read.csv("responses.csv", fileEncoding = "UTF-8")
str(d)                                                 # is the column there?

d <- d[!is.na(d$answer) & trimws(d$answer) != "", ]    # drop blanks first
emb <- embed(d$answer)
```

`embed()` stops with a clear message if you pass a data frame instead of a column, or
if any entry is blank — before spending any of your allowance.

To place each answer on a meaning axis you specify in advance:

```r
anchors <- embed(c("I feel satisfied with my life",      # high pole
                   "I feel dissatisfied with my life"))  # low pole

score <- semantic_projection(emb,
                             high_mat = anchors[1, , drop = FALSE],
                             low_mat  = anchors[2, , drop = FALSE])

cor(score, d$life_satisfaction_scale)   # does it track a measure you trust?
```

That last line is the point. A projection score means something only if it corresponds
to something outside the text. Use several phrases per pole rather than one, and
report how much the result moves when you vary them — the paper does this throughout,
and the answers move more than you would like.

## 7. What it costs, and how the cache saves you money

Embeddings are **deterministic**: the same text and model always return the same
vector. There is no reason to pay for a text twice, so `qualembed` caches every result
to disk.

```r
emb <- embed(texts)   # first run: fetches from the API
emb <- embed(texts)   # second run: reads from disk, no request, no cost
```

The cache is per provider and per model, and is written **after every batch**, so an
interrupted run keeps what it already fetched. Progress is reported as it goes.

Before a large job, ask what it will actually cost:

```r
embed(texts, dry_run = TRUE)
#> dry run: 1200 texts -> 950 cached, 250 would be fetched
```

Other controls:

```r
cache_info()                            # what is cached, per provider and model
embed(texts, rpm = 60)                  # pace at 60 requests/minute
#>   [gemini] 100/250 texts (40%)  ETA ~46s
embed(texts, refresh = TRUE)            # ignore the cache and re-fetch
cache_clear("gemini")                   # delete one provider's cache
embed(texts, cache_dir = "my_cache")    # put the cache elsewhere
```

By default the cache goes in `output/embed_cache/`, relative to your working
directory.

### What the matrix remembers about itself

Every matrix `embed()` returns carries four attributes. Three are provenance;
the fourth is the one that keeps your archive usable.

```r
emb <- embed(setNames(d$answer, d$participant_id))

rownames(emb)[1]         #> "P01"                    -- the label you asked for
attr(emb, "texts")[1]    #> "I felt calm all week."  -- what was actually sent
attr(emb, "provider")    #> "gemini"
attr(emb, "model")       #> "gemini-embedding-001"
attr(emb, "access_date") #> "2026-08-02"
```

The distinction matters more than it looks. Naming your rows by participant is
the normal thing to do, and the moment you do it the matrix stops recording
*what was embedded*. Save it, come back in six months, and the vectors are
anonymous numbers: you cannot rebuild the cache from them, you cannot check that
row 12 is the text you think it is, and neither can a reviewer. The `texts`
attribute keeps that link whatever the row names say.

### What to write in your method section

`embedding_info()` prints exactly the fields a method section needs, from the
matrix or from an archived file, so the record is read off the data rather than
reconstructed from memory:

```r
embedding_info(emb)
#> Provider        : gemini
#> Model           : gemini-embedding-001
#> Dimensions      : 3072
#> Texts           : 240
#> Request options : dims = unset, task_type = unset
#> Fetched         : 2026-07-14 to 2026-08-05
#> Assembled       : 2026-08-05 (12 fetched, 228 from cache)
#> Request batches : 3 requests (n=96;h=4c11ab x96, n=96;h=8e0d72 x96, ...)
#> Software        : qualembed 0.4.1; R version 4.5.0 (2025-04-11)

embedding_info(readRDS("output/embeddings/study1_gemini.rds"))  # archives too
```

It returns the same fields as a one-row data frame, invisibly, so they can be
written straight into a results file.

Three of those lines are easy to get wrong by hand. *Request options* lists the
settings that change the returned vector **including the ones you left unset**,
because defaults differ between clients and change without notice, so "I did not
set it" is part of the specification and cannot be recovered later. *Fetched* is
when the vectors were actually retrieved from the API, which is not the same as
when you built the matrix: a fully cached run touches no endpoint at all. Dates
are recorded per text in the cache from v0.4.0 on; anything cached before that
reports as undated rather than guessing. *Request batches* names which texts
travelled together in one call. The vector a provider returns for a text depends
on what accompanied it in the same request: the same word, same model, same
options, sent alone with nineteen others and then with seventy-six, came back at
a cosine of .9998 to .9999. That is small, and it is enough to move a
cross-language congruence by .01. No provider documents the composition of a
request as a setting, so nothing else records it; `embed()` fingerprints each
request by its size and a hash of its contents, which lets you check whether a
later run sent the same set. Batches are recorded from v0.4.1 on.

Commercial embedding models are versioned products that get retired on the
provider's schedule. A matrix that records only its numbers cannot be matched to
the instrument that produced it once that instrument is gone.

Two consequences worth knowing:

- **Subsetting drops it.** `emb[1:10, ]` returns a matrix with no `texts` — that
  is how R attributes work, not a bug. Embed once and subset for analysis, or
  re-attach with `attr(sub, "texts") <- attr(emb, "texts")[1:10]`.
- **`save_embeddings()` warns** when you archive a matrix that has lost it, and
  again if two matrices in one archive share a name (name-based lookup would
  return only the first).

### Reproducing an analysis without an API key

Because the archives record their own texts, a cache can be rebuilt from them:

```r
source("seed_cache_from_archive.R")   # archives -> cache
```

Then every downstream analysis runs from disk with no requests at all. This is
how the companion paper is reproducible: a reader with the archived matrices can
recompute every number without a key, without quota, and without the texts
having drifted. The paper's own repository includes
`verify_archive_recoverable.R`, which checks that every archived matrix can
still be traced back to its texts — worth borrowing if you archive your own.

## 8. The statistics

These are the calibration tests from the paper. Each takes embeddings and a structure
you specified **before** looking at the result, and tests it against a permutation
null.

```r
items <- get_bfi_items()          # 25 Big Five item texts, with factor labels
emb   <- embed(items$en)

# Do items of the same factor sit closer than items of different factors?
test_delta(cos_sim_matrix(emb), items$factor)

# Does clustering recover the five factors?
test_ari(emb, items$factor, k = 5)

# Do two spaces agree about the relations among the same items?
mantel_test(cos_sim_matrix(emb_a), cos_sim_matrix(emb_b))
```

| Function | Question it answers |
|---|---|
| `test_delta()` | Is within-group similarity higher than between-group? |
| `test_ari()`, `test_ari_sim()` | Does clustering recover a partition you specified? |
| `mantel_test()` | Do two similarity matrices agree? |
| `procrustes_m2()`, `procrustes_sensitivity()` | How well do two spaces align, and which items disagree? |
| `semantic_projection()` | Where does each text fall on an axis you defined? |
| `within_between_sim()`, `cos_sim_matrix()`, `euclidean_dist()` | The underlying quantities |
| `pca_2d()`, `plot_embedding_2d()`, `plot_similarity_heatmap()` | Visual sketches |
| `save_embeddings()`, `write_stats()`, `save_fig()` | Archive results reproducibly |

All tests use 9,999 permutations by default; pass `n_perm =` to change it.

**One thing the package will not do for you.** Do not ask the embeddings how many
dimensions your construct has. Eigenvalue rules and network methods applied to an
embedding similarity matrix return artifacts of the space rather than properties of
the construct — the paper demonstrates this at length. Fix the structure from theory,
then test it.

Built-in materials for trying things out: `get_bfi_items()`, `panas_items`,
`schwartz_items`, `valence_anchors`, `prestige_anchors`.

## 9. Long documents: interviews, diaries, transcripts

Everything above embeds one short text per row. An interview transcript is a
different object. Three facts set the terms.

1. **You may not have a choice about splitting it.** `gemini-embedding-001`
   accepts 2,048 tokens, OpenAI's models 8,192, Voyage's 32,000. A 3,000-word
   transcript does not fit in the first at all.
2. **Even where it fits, one vector for a whole interview is one point.**
   Everything the interview did — the shift when the topic changed, the return to
   an earlier theme — is averaged away before you measure anything.
3. **How you cut it is a decision you own.** Content analysis has treated
   unitizing as a step separate from coding, with its own reliability, for
   decades, and there is no established criterion for how large a unit should be.
   The package will not pick for you.

### Bring your own segmentation

The functions do not care how you split the text. They care that the result is a
**long data frame with one row per segment**:

| column | required | meaning |
|---|---|---|
| `doc_id` | yes | which participant / interview the segment came from |
| `segid` | no | order within the document; derived from row order if absent |
| `text` | yes | the segment |

`as_segments()` is the single door in. It accepts several shapes, checks them, and
adds `n_words`, `n_char`, and a unique `docname` you can use to name the embeddings.

```r
# (a) a data frame you built any way you like — column names are auto-detected
seg <- as_segments(my_coded_export)

# (b) a named list: names become doc_id, element order becomes segid
seg <- as_segments(list(P01 = c("first turn", "second turn"),
                        P02 = c("...")))

# (c) the convenience splitter, if you want one
seg <- segment_text(transcripts, by = "words", size = 80, overlap = 20,
                    ids = participant_ids)

# then, always:
emb <- embed(setNames(seg$text, seg$docname))
```

Naming the embeddings with `seg$docname` is not decoration — it lets every
downstream function match segments to vectors by name instead of trusting row
order.

`as_segments()` recognises the column names your software already produces
(`document`, `File`, `participant`, `content`, `Coded`, …). If two columns could
be the same thing it stops and asks rather than guessing; a silently wrong column
here would be a silently wrong analysis.

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
5. Read back with `fileEncoding = "UTF-8-BOM"` (also correct for files without a
   BOM), apply the merges, and let `as_segments(df, renumber = TRUE)` renumber.
6. Check segment counts and the `n_char` range before you spend any API calls.

```r
# rough cut -> spreadsheet
seg <- segment_text(transcripts, ids = pid)
seg$merge_up <- ""
readr::write_excel_csv(seg[, c("doc_id", "segid", "text", "merge_up")],
                       "to_edit.csv")

# ... edit by hand ...

# back into R
back <- read.csv("to_edit.csv", fileEncoding = "UTF-8-BOM",
                 colClasses = "character")
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
same content yields roughly 1.4× as many words as its English translation —
`size = 50` is not the same window in the two languages. **If you are comparing
English and Japanese, cut by sentence**, the one unit that matched across a
translation pair in our checks. If you are cutting because of a token limit, use
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

### Reading what is already on your disk

```r
seg <- read_segments("transcripts/")          # a folder of files
seg <- read_segments("highlights.csv")        # a CAQDAS export
```

| You have | What to do |
|---|---|
| Coded segments exported from Taguette, QualCoder, NVivo, MAXQDA, ATLAS.ti, Dedoose | Export CSV (or XLSX) — those exports are already one row per segment. `read_segments()` reads them. |
| `.docx` transcripts | `read_segments()` reads them; each paragraph becomes a segment, which for a transcript is usually one speaker turn. Needs `xml2`. |
| `.vtt` / `.srt` from Zoom, Teams, or Whisper | Best case — these carry speaker names and timings, both of which are kept. |
| Plain `.txt` | One file per participant, **speaker turns separated by blank lines**, speaker written as a `Name:` prefix. |

**On the plain-text convention, one warning.** "One segment per line" sounds like
the obvious format and is a trap. The only tool that actually writes it is
Whisper's `.txt` output, whose lines are 2–5 second audio chunks — not turns, not
sentences, not meaning units. Such a file looks correctly structured and is not.
So `read_segments()` splits `.txt` on **blank lines**, not on every line, and warns
if it meets a many-line file with no blank lines. If you genuinely want one segment
per line, ask for it: `unit = "line"`. If you have the `.vtt` from the same
transcription, use that instead.

### Looking at a trajectory

```r
plot_trajectory(emb, seg, doc = "P07")                       # the map with arrows
plot_recurrence(emb, seg, doc = "P07")                       # nothing projected
plot_arc(emb, seg, y = "forward_flow", null_band = 999)      # the arc over time
plot_arc(emb, seg, y = "projection", high = H, low = L)      # on your own axis

trajectory_stats(emb, seg)          # path length, step, straightness (full space)
trajectory_null(emb, seg, 999)      # is the order doing anything?
trajectory_fidelity(emb, seg)       # can the map be trusted?
recurrence_stats(emb, seg)          # RR, DET, LAM at a fixed recurrence rate
```

`plot_trajectory()` is the picture most people picture: each segment placed by the
first two principal components, joined in the order it was spoken, with arrows.
**The order is exact — projection cannot distort which segment follows which — so
"the account went out and came back" is a reading you may take from the arrows.
Distance is not exact, so "this person travelled further" is not.** That is a
full-space quantity and `trajectory_stats()` measures it. The subtitle prints how
much variance the two components hold and how well the on-page distances
rank-correlate with the measured ones, so you can see each time how far to trust
the ruler.

Draw one document at a time. On our data a per-document projection holds 69% of
the variance with a rank correlation of .89 against the full-space distances; put
every participant on one shared projection and that falls to 12% and .37. The
function switches to a shared projection when you pass several documents and warns
when fidelity collapses.

`plot_recurrence()` is a segment × segment cosine matrix for one document. Every
cell is computed in the **full** space and both axes are just position, so nothing
is projected at all. Near-diagonal blocks are topic episodes; an off-diagonal block
is the speaker returning to an earlier theme; a bright vertical stripe is one early
passage the rest of the interview keeps referring back to.

`plot_arc()` draws one full-space quantity per segment against narrative position:
a projection onto an axis you defined, the distance from the previous segment, or
the mean distance from everything said so far. `null_band` shuffles the segment
order and shades where a bag of the same segments would have fallen.

**Three cautions the package enforces rather than merely documents.**

- Raw **path length is not a measure of how far the account travelled**. In our
  own data it correlates .93–.96 with the number of segments and only .55–.64 with
  word count — it mostly counts how many times you cut. Use `step_mean`,
  `straightness`, or the arc; report `path_length` only beside the segment count.
- **A distance measured on a two-dimensional picture is a property of the
  picture.** `trajectory_length()`, which did exactly that, is deprecated and
  warns; `plot_trajectory()` prints its own fidelity so you are never guessing.
- **The same summary number is compatible with different trajectories.** Report the
  scalar and the plot together; neither alone is the finding.

None of these statistics are new — segment-embed-trajectory has been done at scale
in marketing and in clinical speech research, and psychology has its own
chained-utterance measure with a published dispute about what it means. What this
package adds is that they run in R, on commercial APIs, with a permutation null
attached. Their validity on interview-length material has not been established by
anyone, including us.

## 10. Choosing a provider

```r
embed(texts, provider = "gemini")   # default
embed(texts, provider = "voyage")
embed(texts, provider = "openai")
```

| Provider | Default model | Dim. | Notes |
|---|---|---|---|
| `gemini` | `gemini-embedding-001` | 3,072 | Generous free tier; multilingual; a daily quota a large job can hit |
| `voyage` | `voyage-multilingual-2` | 1,024 | Multilingual; a legacy product of its provider |
| `openai` | `text-embedding-3-small` | 1,536 | Requires billing; the least anisotropic of the three |

Override the model, or pass provider-specific options, through `...`:

```r
embed(x, provider = "openai", model = "text-embedding-3-large")
embed(x, provider = "openai", dims = 512)             # shorter vectors
embed(x, provider = "gemini", task_type = "SEMANTIC_SIMILARITY")
embed(x, provider = "voyage", input_type = NULL)       # no instruction
```

Options that change the returned vectors get their own cache file, so results from
different settings never mix.

**One default is not neutral.** For Voyage, `input_type` defaults to `"document"`,
which makes the endpoint prepend a retrieval instruction to the text before encoding
it. Gemini's `task_type` and OpenAI's `dimensions` are left unset, so those two encode
the string as given. The instruction is not cosmetic: on the Big Five items the two
Voyage spaces agree with each other at Mantel *r*~M~ = .86, and clustering agreement
with the theoretical factors rises from .26 to .36 when it is removed. Pass
`input_type = NULL` to send none, and say which you used whenever you report a
comparison across providers — otherwise the comparison is of provider *and*
configuration.

**Vectors from different providers are not comparable.** They live in different spaces
with different dimensionalities. Compare *relations* between them —
`mantel_test()` on the two similarity matrices — never the coordinates.

Models are versioned products and get retired. If a result matters, archive the matrix
with `save_embeddings()` rather than relying on being able to re-fetch it.

## 11. When something goes wrong

`embed()` translates provider errors into plain language. The common ones:

| What you see | What to do |
|---|---|
| Key not found | Missing from `~/.Renviron`, or R was not restarted after editing it |
| Key rejected (401/403) | The key is wrong, revoked, or belongs to a different provider than you asked for |
| Quota exhausted (429) | Free tiers reset daily — wait, switch provider, or enable billing. Fetched batches are cached, so a re-run resumes rather than restarts |
| Model not found (404) | The model was retired; pass a current one via `model =` |
| Could not connect | No network, or a proxy is blocking the request |
| `texts` must be a character vector | You passed a data frame; pass the column: `embed(d$answer)` |
| Missing or empty entries at position(s) … | Drop blanks before embedding; the positions are listed |

If a long run stops partway, run it again. Everything already fetched is cached.

## 12. Before you send participant data

An API call sends your text to a third party. Four rules follow; the paper's Method
sets them out more fully.

**De-identify before the call, not after.** Names, places, employers, and diagnoses in
open-ended answers reach the provider exactly as written. Redact first.

**Do not use free tiers for participant data.** Free tiers generally permit the
provider to use submitted content for training; paid tiers usually do not. Read the
terms for the tier you are actually on, and state in your paper which one it was.

**Treat the vectors like the text.** An embedding is not a one-way hash — source text
can be partially recovered from it. Archived matrices deserve the same access controls
as the transcripts they came from, and the same care about what you deposit publicly.

**Tell your ethics board and your participants.** "Responses will be processed by a
third-party language-model service" belongs in the consent form if that is what will
happen.

## Relation to other packages

`qualembed` is the *confirmatory calibration* layer for survey research: theory
specifies the structure, the statistics test it. For exploratory embedding workflows
(grouping, projection, LLM-assisted labeling) see
[dwulff/embedR](https://github.com/dwulff/embedR); for transformer-based language
analysis in R see [`text`](https://r-text.org). The full analysis scripts and archived
embedding matrices for the paper are deposited on OSF (link forthcoming).

## Citation

```r
citation("qualembed")
```

## License

GPL-3. Issues and pull requests: <https://github.com/PsycholoStudio/qualembed>.
