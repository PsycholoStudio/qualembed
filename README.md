# qualembed

Turn text into coordinates, then do statistics on it — in R, with no Python, no GPU,
and no model training.

`qualembed` sends text to a commercial embedding API and gets back a numeric vector
for each piece of text. Texts that mean similar things land near each other, so
occupation names, open-ended answers, or scale items become something you can
correlate, cluster, and test. The package wraps three providers behind one function
and adds a set of calibration statistics. It accompanies *Embedding qualitative data in
LLM semantic space: A tutorial on conceptualization, measurement, and validation*.

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

Longer notes live in `docs/`:
[recording provenance](docs/provenance.md) ·
[cutting long documents by hand](docs/segmentation.md) ·
[reading a trajectory plot](docs/reading-plots.md)

---

## 1. Install

You need R 4.1 or later. From the R console:

```r
install.packages("remotes")                          # once
remotes::install_github("PsycholoStudio/qualembed")
library(qualembed)
```

The request layer depends on `httr2` for the web requests and `stringi` for the ICU
boundary analysis that cuts text into units. Plotting uses `ggplot2` and `ggrepel`;
Mantel tests use `vegan`; tables use `dplyr`. `remotes` installs these for you.
Two things it does not install: `psych`, which the Big Five example in section 8
needs, and `MASS`, without which the non-metric MDS of section 8 falls back to PCA.

## 2. Get an API key

An API key is a password that identifies you to the provider. You need exactly one of
the three below. Gemini and Voyage have a free allowance that is more than enough to
work through this README and most small studies.

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
plot_embedding_2d(coords_2d(emb), labels = rownames(emb))
```

Treat the map as a sketch. Two dimensions cannot hold what 3,072 encode, so read
it by which points are nearer than which, never by how far apart they look.

## 6. Using your own survey data

Say you asked an open-ended question and have the answers in a spreadsheet.

**Save the file as CSV with UTF-8 encoding.** This matters if your text is Japanese,
or has accented characters, or curly quotes from Word. In Excel: File → Save As →
*CSV UTF-8*. Getting this wrong produces garbled text that embeds as nonsense, and
the numbers will look fine while meaning nothing.

```r
d <- read.csv("responses.csv", encoding = "UTF-8")
str(d)                                             # is the column there?

txt <- d$answer                                    # take the column first
txt <- txt[!is.na(txt) & trimws(txt) != ""]        # then drop the blanks
emb <- embed(txt)
```

Take the column before filtering, not after. Base `read.csv()` gives you a data
frame whose `[` drops a single-column result to a plain vector, so filtering the
frame first breaks on a one-column file; taking the column first behaves the same
whatever you read with.

And you can read with whatever you like — `embed()` needs only a character vector.
`readr::read_csv()` and `data.table::fread()` take no encoding argument: they read
as UTF-8, drop the byte-order mark Excel writes, and keep non-ASCII column names,
all of which base `read.csv()` needs help with outside a UTF-8 locale. What none of
them can do is rescue a file saved in the wrong encoding, which is why the
instruction above is about saving rather than reading.

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
report how much the result moves when you vary them.

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
#> embed[gemini/gemini-embedding-001] dry run: 1200 texts -> 950 cached,
#>   250 would be fetched (Gemini free tier allows ~1,000 per day)
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

Every matrix `embed()` returns carries its own provenance as attributes: the
provider, model, dimensionality and access date, how many texts came from the
API and how many from the cache, the request options and batch sizes, and the
software that fetched them. One of them, `texts`, is what keeps your archive
usable.

```r
emb <- embed(setNames(d$answer, d$participant_id))

rownames(emb)[1]            #> "P01"                    -- the label you asked for
attr(emb, "texts")[1]       #> "I felt calm all week."  -- what was actually sent
attr(emb, "provider")       #> "gemini"
attr(emb, "model")          #> "gemini-embedding-001"
attr(emb, "access_date")    #> "2026-08-02"  -- when the matrix was assembled
attr(emb, "fetch_dates")[1] #> "2026-07-30"  -- when that text was fetched
```

The distinction matters more than it looks. Naming your rows by participant is
the normal thing to do, and the moment you do it the matrix stops recording
*what was embedded*. Save it, come back in six months, and the vectors are
anonymous numbers: you cannot rebuild the cache from them, you cannot check that
row 12 is the text you think it is, and neither can a reviewer. Subsetting drops
the attribute too — `emb[1:10, ]` returns a matrix with no `texts` — so subset
the segment table and re-embed rather than slicing the matrix. The `texts`
attribute keeps that link whatever the row names say.

### What to write in your method section

`embedding_info()` prints the fields a method section needs, read off the
matrix rather than remembered: provider, model, dimensionality, request
options, when the vectors were fetched and by what software. It works on an
archived file too. [docs/provenance.md](docs/provenance.md) shows the output
and what each field settles.

### Reproducing an analysis without an API key

Because the archives record their own texts, a cache can be rebuilt from them:

```r
source("seed_cache_from_archive.R")   # archives -> cache
```

Then every downstream analysis runs from disk with no requests at all. That
script is not part of this package: it ships with the paper's own repository,
along with `verify_archive_recoverable.R`, which checks that every archived
matrix can still be traced back to its texts. Both are worth borrowing if you
archive your own.

## 8. The statistics

`embed()` is the part of this package you need. It returns an ordinary numeric
matrix, so whatever you would do with a matrix of coordinates, you can do with
it — your own clustering, your own regression, your own plots. Nothing below is
required.

What is below are worked examples of that kind of analysis. Each takes
embeddings and a structure you specified **before** looking at the result, and
tests it against a permutation null.

```r
items <- psych::bfi.dictionary[1:25, ]   # 25 Big Five item texts
fct   <- substr(rownames(items), 1, 1)   # A C E N O
emb   <- embed(items$Item)

# Do items of the same factor sit closer than items of different factors?
d <- test_delta(cos_sim_matrix(emb), fct)
d$delta_std   # divided by SD(between); the raw delta is not comparable across models

# Does clustering recover the five factors?
a <- test_ari(emb, fct, k = 5)
a[c("ari", "p")]

# Do two spaces agree about the relations among the same items?
emb2 <- embed(items$Item, provider = "openai")
mantel_test(cos_sim_matrix(emb), cos_sim_matrix(emb2))
```

| Function | Question it answers |
|---|---|
| `test_delta()` | Is within-group similarity higher than between-group? (`delta_std` compares across models) |
| `test_ari()`, `test_ari_sim()` | Does clustering recover a partition you specified? |
| `mantel_test()` | Do two similarity matrices agree? |
| `procrustes_m2()`, `procrustes_sensitivity()` | How well do two spaces align, and which items disagree? |
| `semantic_projection()` | Where does each text fall on an axis you defined? |
| `within_between_sim()`, `cos_sim_matrix()`, `euclidean_dist()` | The underlying quantities |
| `coords_2d()`, `plot_embedding_2d()`, `plot_similarity_heatmap()` | Visual sketches |
| `save_embeddings()`, `write_stats()`, `save_fig()` | Archive results reproducibly |
| `progress_ticker()` | Show progress and ETA in long permutation loops |

The permutation tests use 9,999 draws by default; pass `n_perm =` to change
it. `procrustes_sensitivity()` is the exception: it sweeps k and does not
permute.

**Two dimensions, and which two.** `coords_2d()` defaults to non-metric
multidimensional scaling rather than to PCA. What a reader takes off a scatter
plot is the *ranking* of the distances on it, and that is what non-metric MDS
fits. PCA maximises variance instead, and an embedding holds far fewer texts
than the space has dimensions, so the first two components have little variance
to maximise. `procrustes_m2()` is the exception and uses PCA, because Procrustes
is a metric criterion and non-metric coordinates carry no common scale.
`?coords_2d` gives the argument in full; `layout =` overrides either default.

**Rotation changes meaning, not accuracy.** Rotating a set of points in the
plane changes no distance and no angle between them, so it cannot make a display
more faithful — factor analysis rotates because *loadings* are what gets
interpreted there, and a scatter of points has no loadings. What rotation can do
is put an interpretable direction on an axis. Pass `axis =` a direction you
specified in advance — the difference between your high and low anchor
centroids, say — and the configuration turns so that direction lies along the
horizontal. It also makes panels comparable when the direction comes out
reversed in one of them.

**One thing the package will not do for you.** Do not ask the embeddings how many
dimensions your construct has. Eigenvalue rules and network methods applied to an
embedding similarity matrix return artifacts of the space rather than properties
of the construct. Fix the structure from theory, then test it.

This package ships functions, not datasets. It carries no scale items and no
anchor sets: those belong to the instruments and studies they come from, and
you supply your own. The materials used in the paper are in its OSF deposit (<https://doi.org/10.17605/OSF.IO/GU2BQ>).

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
#     `?segment_text` for choosing the unit, and for why "words" is not
#     the same window in English and Japanese
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

`as_segments()` takes any table with one row per segment, so you can cut in a
spreadsheet or a CAQDAS tool and bring the result back. Use
`read_segments()` to read it: it strips the byte-order mark that Excel writes,
checks the encoding, and stops rather than losing rows silently.
[docs/segmentation.md](docs/segmentation.md) has the procedure, the four ways
a Japanese CSV goes wrong, and how to report agreement on the cutting.

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

Start with the two displays that project nothing. `plot_recurrence()` draws the
segment-by-segment similarity matrix, and `plot_arc()` plots one full-space
quantity against narrative position; every value in both is computed in the full
space. `plot_trajectory()` comes third, drawing one panel per document with each
segment placed by a two-dimensional layout and joined in the order it was
spoken — the only one of the three whose geometry you have to qualify as you
read it. `trajectory_stats()` summarises the path, `trajectory_null()` asks
whether the order matters, `trajectory_fidelity()` says how much of the
distance structure the picture keeps, and `recurrence_stats()` returns the
recurrence-quantification measures — which are undefined on short documents,
where the matrix has no line to measure. What the picture supports and what it does
not, and why a short document cannot settle a single transition, are in
[docs/reading-plots.md](docs/reading-plots.md) and in `?plot_trajectory`.

## 10. Choosing a provider

```r
embed(texts, provider = "gemini")   # default
embed(texts, provider = "voyage")
embed(texts, provider = "openai")
```

| Provider | Default model | Dim. | Notes |
|---|---|---|---|
| `gemini` | `gemini-embedding-001` | 3,072 | Generous free tier; multilingual; a daily quota a large job can hit |
| `voyage` | `voyage-4` | 1,024 | Multilingual; strict free-tier rate limit |
| `openai` | `text-embedding-3-small` | 1,536 | Requires billing; no free tier |

Override the model, or pass provider-specific options, through `...`:

```r
embed(x, provider = "openai", model = "text-embedding-3-large")
embed(x, provider = "openai", dims = 512)             # shorter vectors
embed(x, provider = "gemini", task_type = "CLUSTERING")
embed(x, provider = "voyage", input_type = "document")  # prepend an instruction
```

Options that change the returned vectors get their own cache file, so results from
different settings never mix. The key is built from the *resolved* options rather
than from what you typed, so an option left at `NULL` does not enter it and a
default that changes in a future version moves the key rather than silently
serving old vectors under a name that no longer describes them. Running someone
else's code with `cache = TRUE` reads their cache rather than the endpoint; pass
`refresh = TRUE` to reach the API. `?embed` has the rest.

**The defaults are chosen, not inherited.** Every statistic in this package rests
on a cosine similarity matrix, which makes every task here a symmetric one, and
the three APIs do not agree on what to do when you say nothing. Gemini's
`task_type` defaults to `"SEMANTIC_SIMILARITY"`, because sending no task type is
not a neutral option. Voyage's `input_type` defaults to `NULL`, its own default,
because setting it makes the endpoint prepend a retrieval instruction to your
text before encoding. OpenAI exposes no equivalent parameter. `?embed` gives the
argument for each.

A common condition across all three is therefore not attainable. State the
options you used whenever you report a comparison across providers — otherwise
the comparison is of provider *and* configuration.

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

**The cache is a second copy of the text.** `embed()` writes what you sent to disk
verbatim and unencrypted, so the cache inherits whatever obligations the responses
carry. Pass `cache = FALSE`, or set `cache_dir` to a controlled-access location, and
say which you did in the data-management plan.

## Relation to other packages

`qualembed` is the *confirmatory calibration* layer for survey research: theory
specifies the structure, the statistics test it. For exploratory embedding workflows
(grouping, projection, LLM-assisted labeling) see
[dwulff/embedR](https://github.com/dwulff/embedR); for transformer-based language
analysis in R see [`text`](https://r-text.org). The full analysis scripts and archived
embedding matrices for the paper are deposited on OSF (<https://doi.org/10.17605/OSF.IO/GU2BQ>).

## Citation

```r
citation("qualembed")
```

## License

GPL (>= 3). Issues and pull requests: <https://github.com/PsycholoStudio/qualembed>.
