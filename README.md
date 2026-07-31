# qualembed

Embedding-based semantic measurement and calibration for survey and
qualitative text. Companion package to the paper *Embedding qualitative
data in LLM semantic space: A conceptual history, R tutorial, and
empirical calibration*.

- One function to embed text via **Gemini / Voyage AI / OpenAI** (pure R,
  `httr2` only)
- **Caching by default** — embeddings are deterministic, so a re-run costs no
  API calls; only new texts are fetched, and partial results survive an
  interrupted run
- **Free-tier friendly** — `dry_run` tells you how many texts would be fetched
  before you commit, `rpm` paces requests under a rate limit, and progress is
  reported per batch
- Plain-language errors (invalid key, exhausted quota, no connection) and
  input validation that fires *before* any request
- The paper's calibration statistics: permutation-tested within-between
  contrast (`test_delta`), Ward clustering + adjusted Rand index
  (`test_ari`), Mantel congruence (`mantel_test`), Procrustes alignment
  with per-item residuals (`procrustes_m2`), theory-anchored
  `semantic_projection`
- Visualization helpers (`pca_2d`, `plot_embedding_2d`, ...)

## Installation

```r
install.packages("remotes")   # once
remotes::install_github("PsycholoStudio/qualembed")
```

## API key

Get a free key (Gemini: https://aistudio.google.com/apikey) and store it
as an environment variable in `~/.Renviron`:

```
GEMINI_API_KEY=paste_your_key_here
```

Restart R. Never put keys in scripts. **Do not send participant data
through free API tiers** — see the paper's data-ethics section.

## Minimal example

```r
library(qualembed)

occupations <- c("physician", "nurse", "teacher", "carpenter", "athlete")
emb  <- embed(occupations, provider = "gemini")
sim  <- cos_sim_matrix(emb)
round(sim["physician", ], 2)
proj <- pca_2d(emb)
plot_embedding_2d(proj)
```

## Working within a free tier

```r
# How many texts would actually be fetched? (no API calls)
embed(my_responses, provider = "gemini", dry_run = TRUE)
#> dry run: 1200 texts -> 950 cached, 250 would be fetched

# Pace requests instead of hitting the limit, with progress
emb <- embed(my_responses, provider = "gemini", rpm = 60)
#>   [gemini] 100/250 texts (40%)  ETA ~46s

cache_info()      # what is cached, per provider and model
cache_clear("gemini")   # start over for one provider
```

## Changing the model or its options

```r
embed(x, provider = "openai", model = "text-embedding-3-large")
embed(x, provider = "openai", dims = 512)          # shorter vectors
embed(x, provider = "gemini", task_type = "SEMANTIC_SIMILARITY")
embed(x, provider = "voyage", input_type = "query")
embed(x, refresh = TRUE)   # ignore the cache and fetch again
```

Options that change the returned vectors get their own cache file, so results
from different settings never mix.

## Relation to other packages

`qualembed` is the *confirmatory calibration* layer for survey research:
theory specifies structure, the statistics test it. For exploratory
embedding workflows (grouping, projection, LLM-assisted labeling) see
[dwulff/embedR](https://github.com/dwulff/embedR); for transformer-based
language analysis in R see the [`text`](https://r-text.org) package.
The full analysis scripts and archived embedding matrices for the paper
live in the OSF repository (link forthcoming).

## License

GPL (>= 3)
