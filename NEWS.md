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
