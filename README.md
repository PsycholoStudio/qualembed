# qualembed

Embedding-based semantic measurement and calibration for survey and
qualitative text. Companion package to the paper *Embedding qualitative
data in LLM semantic space: A conceptual history, R tutorial, and
empirical calibration*.

- One function to embed text via **Gemini / Voyage AI / OpenAI** (pure R,
  `httr2` only; automatic retry with exponential backoff; plain-language
  error messages)
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
