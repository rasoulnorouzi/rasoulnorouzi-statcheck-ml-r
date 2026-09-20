# statcheckml

statcheckml is the R port of statcheck-ml, a machine-learned replacement for
the extraction step of the R package statcheck. It reads a shared kit of
rules and a trained model, and it turns text from a PDF into reported
statistical test results.

Install:

```r
remotes::install_github("rasoulnorouzi/statcheck-ml-r")
```

statcheckml is not yet on CRAN.

## Status

| stage | in this package |
|---|---|
| read a PDF to text | yes, `sc_read_pdf()` |
| normalise the text | yes, `sc_normalize()` |
| repair a damaged operator | not yet |
| prefilter a line | not yet |
| find a result, pattern baseline | yes, `sc_extract()` |
| find a result, trained model | not yet |
| check a p-value | not yet |

## Use

```r
library(statcheckml)
kit <- sc_kit()
text <- sc_normalize("The effect was significant, t(28) = 2.87,\np = .006.", kit)
sc_extract(text, kit)
```

Every function that reads a rule takes a `kit`. Load one with `sc_kit()`. It
verifies the rules and the model against `manifest.json` before it returns
one, so a copy that has drifted from the mother repository is refused.
