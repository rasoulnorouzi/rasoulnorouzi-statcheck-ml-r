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
| repair a damaged operator | yes, `sc_repair()` |
| prefilter a line | yes, `sc_prefilter()` |
| find a result, pattern baseline | yes, `sc_extract()` |
| find a result, trained model | yes, `sc_tag()` plus `sc_group()` |
| check a p-value | yes, `sc_compute_p()` and `sc_verdict()` |
| the whole pipeline, text to checked results | yes, `sc_check_text()` |

## Use

```r
library(statcheckml)
kit <- sc_kit()
model <- sc_load_model(kit)

sc_check_text("The effect was significant, t(28) = 2.87, p = .006.", kit, model)
```

That one call runs every stage in order: normalise, repair, prefilter, find
(pattern first, then the model for what the pattern did not already find),
and check. Each stage is also usable on its own:

```r
text <- sc_normalize("The effect was significant, t(28) = 2.87,\np = .006.", kit)
sc_extract(text, kit)

sc_tag(text, kit, model)[[1]]

sc_verdict(list(test_type = "t", statistic = 2.87, df1 = 28,
                p_operator = "=", p_value = 0.006))
```

`sc_tag()` takes a whole vector of texts and tags them in one batch, so give
it a document's windows together rather than one at a time. `sc_check_text()`
does this itself, tagging every kept window of the document in one call.
Load the model once with `sc_load_model()` and pass it in.

Every function that reads a rule takes a `kit`. Load one with `sc_kit()`. It
verifies the rules and the model against `manifest.json` before it returns
one, so a copy that has drifted from the mother repository is refused.
