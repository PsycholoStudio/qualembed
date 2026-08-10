# qualembed 0.4.1

* `embed()` now records which texts were sent in the same request, and
  `embedding_info()` prints it. The vector a provider returns for a text depends
  on what accompanied it in that request: the same word, same model, same
  options, sent once alongside nineteen others and once alongside seventy-six,
  came back at a cosine of .9998 to .9999, enough to move a cross-language
  congruence by .01. No provider exposes the composition of a request as a named
  option, so it appeared in no existing record. Each request is fingerprinted by
  its size and an order-independent hash of its contents, which is enough to
  check whether a later run sent the same set without storing the texts a second
  time. The fingerprints live in a cache attribute, so existing caches remain
  valid and report as unrecorded rather than being guessed at.

# qualembed 0.4.0

* New `embedding_info()` prints the provenance of an embedding matrix, or of an
  archived list of them: provider, model, dimensionality, request options, when
  the vectors were fetched, and the software that fetched them. It returns the
  same fields as a one-row data frame, invisibly, so they can be written to a
  results file. These are the fields a method section needs in order to identify
  the instrument; commercial embedding models are versioned products that get
  retired, and a matrix recording only its numbers cannot be matched to the model
  that produced it once that model is gone.
* `embed()` now records the request options that were actually in force,
  **including the ones left at their defaults**. Defaults differ between clients
  and change without notice, so "not set" is part of the specification and cannot
  be recovered afterwards.
* The cache now records, per text, the date the vectors were fetched, and
  `embed()` reports that date rather than the date the matrix was assembled --
  a fully cached run contacts no endpoint, and dating it today would misstate
  when the measurement was taken. The dates live in an attribute of the cache
  file, so existing caches remain valid; texts cached before this release report
  as undated rather than being guessed at.
* `embed()` also attaches `dim_embedding`, `n_fetched`, `n_from_cache` and
  `software`.
* New `centered_sim_matrix()` computes cosine similarity after centering the
  embeddings on the item pool, and `double_center()` double-centres a similarity
  matrix. Both are offered as diagnostics rather than repairs. Centering makes
  most similarities in a pool negative, but it does so by an algebraic identity
  that guarantees negatives whether or not anything opposes, so a negative value
  it produces is not evidence of opposition. `centered_sim_matrix()` refuses to
  run on fewer than three texts, where centering forces the similarity to -1 by
  construction.

# qualembed 0.3.0

* Segmentation for text too long to embed whole. `as_segments()` accepts a
  segmentation the analyst produced any way they like (data frame, named list,
  character vector, quanteda corpus) and stops rather than guesses when two
  columns could be the same thing; `read_segments()` reads .txt, .docx, .vtt,
  .srt, .csv, .tsv and .xlsx. Plain text splits on blank lines rather than on
  every line, and a file with one segment per line is flagged: the tool that
  writes that shape is Whisper, whose lines are two-to-five-second audio chunks
  rather than turns, so the file looks correctly structured and is not.
* `segment_text()` splits by sentence, words, characters or paragraph. The
  sentence splitter now uses ICU boundaries; the previous regular expression
  required a sentence-final period, a space and a capital letter, so it never
  split Japanese at all, and it also mis-split "Dr. Smith". Word counting moved
  with it: whitespace counting made every Japanese paragraph one word. An ICU
  "word" in Japanese is a morpheme, so the same content yields about 1.4 times
  the English count -- use `by = "sentence"` when comparing the two languages
  and `by = "chars"` when cutting to a token limit.
* Trajectories through semantic space: `plot_trajectory()`, with
  `trajectory_stats()`, `trajectory_fidelity()`, `trajectory_null()`,
  `recurrence_stats()`, `plot_recurrence()` and `plot_arc()`. Projection
  preserves the order of segments exactly, so the arrows may be read; it does
  not preserve distance, so their lengths may not, and the subtitle prints the
  rank correlation between on-page and full-space distances. Projection is
  per-document by default, because a shared projection dropped that correlation
  from .89 to .37 on our data.
* `trajectory_length()` measured a two-dimensional PCA path -- the picture
  rather than the space -- and is deprecated with a warning.
  `plot_trajectories()` is superseded by `plot_trajectory()` and kept as a
  deprecated alias.
* `embed()` records the exact strings sent to the API as `attr(m, "texts")`.
  Row names are whatever the caller asked for, usually participant IDs, so a
  matrix alone did not record what was embedded and an archive of such matrices
  could not be traced back or used to rebuild a cache. `save_embeddings()` warns
  when the attribute is missing, and when two matrices share a name, since
  name-based lookup would silently return only the first.
* The embedding cache is written to a temporary file and renamed. A direct
  `saveRDS()` left it truncated and unreadable when a run was interrupted
  mid-write.

# qualembed 0.2.0

* `get_bfi_items()` and `schwartz_items` now return `ja = NULL`. The Japanese
  renderings previously shipped for these two instruments were not taken from
  published validated translations and have been removed; use the instruments in
  English, or supply a validated translation of your own.
* The Japanese PANAS items (`panas_ja_validated`) are unaffected -- they are the
  twenty items of the published validated scale (Kawahito et al., 2011).
* The semantic-projection anchor sets are unaffected.

# qualembed 0.1.0

Initial release: the `embed()` wrapper for Gemini, Voyage and OpenAI, with
transparent caching, progress reporting, rate pacing, a dry-run mode and
overridable provider options; the calibration statistics layer; and the shared
instrument texts.
