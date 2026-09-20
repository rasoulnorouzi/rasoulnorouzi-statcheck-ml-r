# statcheckml 0.1.0

* First release: kit loading and verification, the normalise stage, and the
  pattern-based extract stage.
* The character tagger runs in pure R: `sc_load_model()` reads the kit's
  `weights.json`, and `sc_tag()` runs the bidirectional GRU and the CRF
  Viterbi decode over a batch of texts. Its logits match the Python
  reference to 1e-3 on the kit's parity cases.
