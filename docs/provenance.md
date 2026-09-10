# Recording what a matrix was measured with

Part of the [qualembed](../README.md) documentation.

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
#> Request options : dims = unset, task_type = "SEMANTIC_SIMILARITY"
#> Fetched         : 2026-07-14 to 2026-08-05
#> Assembled       : 2026-08-05 (12 fetched, 228 from cache)
#> Request batches : 3 requests (n=96;h=4c11ab x96, n=96;h=8e0d72 x96, ...)
#> Software        : qualembed 1.0.0; R version 4.5.0 (2025-04-11)

embedding_info(readRDS("output/embeddings/study1_gemini.rds"))  # archives too
```

It returns those fields as a one-row data frame, invisibly, except the
per-request batch sizes, which are printed but not returned, so they can be
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
options, sent in batches of different sizes, does not always come back bit for
bit identical. The differences are small, and small differences propagate into
any statistic computed over the whole matrix. No provider documents the composition of a
request as a setting, so nothing else records it; `embed()` fingerprints each
request by its size and a hash of its contents, which lets you check whether a
later run sent the same set. Batches are recorded from v0.4.1 on.

Commercial embedding models are versioned products that get retired on the
provider's schedule. A matrix that records only its numbers cannot be matched to
the instrument that produced it once that instrument is gone.

Two consequences worth knowing:

- **Subsetting drops the `texts` attribute.** `emb[1:10, ]` returns a matrix
  with no `texts` — that
  is how R attributes work, not a bug. Embed once and subset for analysis, or
  re-attach with `attr(sub, "texts") <- attr(emb, "texts")[1:10]`.
- **`save_embeddings()` warns** when you archive a matrix that has lost it, and
  again if two matrices in one archive share a name (name-based lookup would
  return only the first).

