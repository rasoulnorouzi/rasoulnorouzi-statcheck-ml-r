# statcheckml 0.1.0

* First release: kit loading and verification, the normalise stage, and the
  pattern-based extract stage.
* The character tagger runs in pure R: `sc_load_model()` reads the kit's
  `weights.json`, and `sc_tag()` runs the bidirectional GRU and the CRF
  Viterbi decode over a batch of texts. Its logits match the Python
  reference to 1e-3 on the kit's parity cases.
* The prefilter (`sc_prefilter()`), the arithmetic-driven operator repair
  (`sc_repair()`), the tagged-span grouping (`sc_tags_to_spans()`,
  `sc_group()`), and the p-value check (`sc_compute_p()`, `sc_verdict()`).
  `sc_check_text()` runs all of it in order -- normalise, repair, prefilter,
  find, check -- tagging a whole document's windows in one `sc_tag()` call.
  Every stage matches the Python reference on the kit's parity cases.
* Fixed a latent bug in `sc_extract()`'s chi-square pattern: without PCRE's
  `(*UCP)` verb, `\b` does not treat the Greek chi as a word character, so a
  chi-square result written with the Greek symbol and preceded by a space
  was silently never matched. Python's `re` is Unicode-aware by default, so
  this brought the port back in line with it rather than changing its
  behaviour.
