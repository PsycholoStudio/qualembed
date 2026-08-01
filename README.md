# qualembed

Turn text into coordinates, then do statistics on it — in R, with no Python, no GPU,
and no model training.

`qualembed` sends text to a commercial embedding API and gets back a numeric vector
for each piece of text. Texts that mean similar things land near each other, so
occupation names, open-ended answers, or scale items become something you can
correlate, cluster, and test. The package wraps three providers behind one function
and adds the statistics used in the companion paper, *Embedding qualitative data in
LLM semantic space: A conceptual history, R tutorial, and empirical calibration*.

Nothing is generated and no respondents are simulated. Your participants' own words
stay the data; the model is a measuring instrument.

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
[9 Providers](#9-choosing-a-provider) ·
[10 Troubleshooting](#10-when-something-goes-wrong) ·
[11 Participant data](#11-before-you-send-participant-data)

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
status code. Section 10 lists the common cases.

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

All tests use 9,999 permutations by default; `options(n_perm = ...)` changes it.

**One thing the package will not do for you.** Do not ask the embeddings how many
dimensions your construct has. Eigenvalue rules and network methods applied to an
embedding similarity matrix return artifacts of the space rather than properties of
the construct — the paper demonstrates this at length. Fix the structure from theory,
then test it.

Built-in materials for trying things out: `get_bfi_items()`, `panas_items`,
`schwartz_items`, `valence_anchors`, `prestige_anchors`.

## 9. Choosing a provider

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
embed(x, provider = "voyage", input_type = "document")
```

Options that change the returned vectors get their own cache file, so results from
different settings never mix.

**Vectors from different providers are not comparable.** They live in different spaces
with different dimensionalities. Compare *relations* between them —
`mantel_test()` on the two similarity matrices — never the coordinates.

Models are versioned products and get retired. If a result matters, archive the matrix
with `save_embeddings()` rather than relying on being able to re-fetch it.

## 10. When something goes wrong

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

## 11. Before you send participant data

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
