# Python's `re` reads a `str` pattern with Unicode rules: `\s` matches a
# no-break space (U+00A0), and `\d`, `\w` and `\b` know letters and digits
# outside ASCII. PCRE does that only under `(*UCP)`. Without it a PDF that
# sets "p < .01" with no-break spaces passes the reference's pattern and
# fails this port's. Every pattern this port runs goes through `ucp()`.
ucp <- function(pattern) {
  if (startsWith(pattern, "(*UCP)")) pattern else paste0("(*UCP)", pattern)
}
