# qualembed 0.2.0

## Withdrawn materials

The Japanese renderings of the Big Five and Schwartz value descriptions have been
removed from `get_bfi_items()` and `schwartz_items`, which now return `ja = NULL`.

Those translations were not taken from published, validated Japanese versions of the
instruments; they were produced by a language model while the accompanying paper was
being drafted, and an earlier version of the package described them as author
translations. An unvalidated translation cannot establish that a recovered structure
belongs to the construct rather than to the wording, which is exactly what a
cross-linguistic comparison is asked to settle, so they have been withdrawn rather
than relabelled.

The Japanese PANAS items are unaffected: they are the twenty items of the published
validated scale (Kawahito, Otsuka, Kaida, & Nakata, 2011) and remain available.

The anchor sets for semantic projection are also unaffected. They are phrases written
to define an axis, not translations of any instrument, and no equivalence between the
English and Japanese anchor sets is claimed or required.

# qualembed 0.1.0

Initial release: the `embed()` wrapper for Gemini, Voyage and OpenAI, with transparent
caching, progress reporting, rate pacing, a dry-run mode and overridable provider
options; the calibration statistics layer; and the shared instrument texts.
