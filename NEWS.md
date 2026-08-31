# qualembed 0.5.0

* **`within_between_sim()` と `test_delta()` が尺度不変な Δ を返す。** 生の
  `delta` は空間の密度に依存する。同じ 25 項目でも、グループ間コサインの
  平均は Gemini で .77、Voyage で .29、OpenAI で .15 と三倍以上ひらく。
  差をそのまま並べるとモデルの等方性を測っていることになるので、グループ間
  類似度の標準偏差で割った `delta_std` を `sd_between` とともに返すように
  した。生の `delta` は従来どおり返るので、既存のコードは壊れない。

* **並べ替え検定に進捗表示がつき、Δ の帰無分布が速くなった。**
  `progress_ticker()` を公開した。端末なら `\r` で 1 行を上書きし、ログに
  リダイレクトされていれば一定間隔で 1 行ずつ追記する。見積もりが
  `min_secs` 秒に満たない処理では何も出さないので、短い呼び出しの出力で
  ログが埋まらない。`test_delta()` と `test_ari_sim()` が使う。
  あわせて `test_delta()` の帰無分布を組み直した。従来は毎反復で n × n の
  論理行列を作って添字していたため、n が千を超えるとそこが支配的になって
  いた。ペアの行・列番号と類似度を一度だけ取り出してベクトルで回す形に
  変え、1,120 件で二倍速くなった。算術も乱数列も同じなので、返る値は
  一致する（9,999 回の帰無分布で最大差 0 を確認）。

* **Two-dimensional displays now default to non-metric MDS instead of PCA.**
  PCA maximises variance, not the ranking of distances, and an embedding puts
  far fewer texts than dimensions into the space, so the first two components
  carry little of it and the plane flattens the points into a band. Measured on
  six materials under three providers, the rank correlation between plotted and
  measured distances rose in all eighteen cells: for 169 narrative segments from
  .16--.54 under PCA to .76--.84 under MDS, against .33 and .56 for the same
  procedures applied to isotropic points. Under two of three providers the PCA
  plane held no more rank order than random points. `coords_2d()` is the new
  general entry point, `plot_trajectory()` and `trajectory_fidelity()` take
  `layout = "mds"` (default) or `"pca"`, and `pca_2d()` remains as a thin
  wrapper. MASS moves to Suggests; without it the functions fall back to PCA.

* **`axis =` rotates a configuration onto a direction you specify.** Rotation
  changes neither distances nor angles, so it cannot make a plane more faithful;
  what it changes is what the axes mean. Rotating the MDS configuration of the
  narrative segments onto a pre-specified harmony axis left fidelity at
  .84/.84/.76 and moved the correlation between the horizontal axis and the
  projection on that axis from .88/.53/-.45 to .88/.76/.72 -- the sign under one
  provider had been reversed. This is the factor-analysis intuition applied
  where it holds: choose the configuration by an objective, then rotate it for
  interpretation.

* `trajectory_null()` gains `stat = "far_mean"`, which tests the structure a
  recurrence plot displays: the mean cosine between segments at least `lag_min`
  apart, against reorderings of the document's own segments. Permuting the order
  leaves the set of similarities untouched and moves only which pairs are
  adjacent, so the null asks exactly whether the ordering makes the structure. A
  value above the null is a return to an earlier theme, below it a topic shift.

* `plot_embedding_2d()` no longer prints a variance-explained subtitle when the
  coordinates come from MDS. Variance explained is a PCA quantity and printing
  it over MDS axes would misinform.

* `procrustes_m2()` and `procrustes_sensitivity()` take `layout` and still
  default to `"pca"`, and that is not an inconsistency. For Euclidean distances
  classical (metric) MDS and PCA are the same configuration, so the choice
  anywhere in the package is metric against non-metric, under one rule: match
  the reduction to what the next step reads. A display is read by the ranking of
  distances, so it gets non-metric MDS. Procrustes m² is a metric criterion --
  squared distances after a similarity transform, which admits one uniform scale
  factor -- while non-metric MDS gives each of the two spaces its own monotone
  transformation, landing them on scales no single factor reconciles. So m²
  keeps the metric reduction. The difference is small in any case (PANAS
  English-Japanese at k = 5: .42/.64/.66 under PCA against .40/.61/.72 under
  non-metric MDS, ordering across providers unchanged).

# qualembed 0.4.2

* **The projection-free displays are now the recommended ones.** Offering
  `plot_trajectory()` first, and then telling the reader which parts of it not
  to read, put the caveat and the picture in the wrong order. `plot_recurrence()`
  and `plot_arc()` compute every quantity in the full space and use the axes for
  nothing but position, so nothing about them has to be qualified while reading.
  The README lists them first, `?plot_recurrence` says it is where to start, and
  `?plot_trajectory` and its fidelity warning now name the two alternatives.
  The map is still there and still documented; it is no longer the default look.

* The README's advice to "draw one document at a time" is removed. It predates
  the finding that a per-document plane's fidelity is mostly a function of how
  few segments it was fitted to: 69% of the variance and a rank correlation of
  .89 sound like a faithful plane until the same count of segments drawn at
  random from the pool returns 74% and .87. `scope = "shared"` is now the advice
  when panels are to be compared, which is what the function has done since
  0.4.1.

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
