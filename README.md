# statcheckml

[![R-CMD-check](https://github.com/rasoulnorouzi/statcheck-ml-r/actions/workflows/check.yml/badge.svg)](https://github.com/rasoulnorouzi/statcheck-ml-r/actions/workflows/check.yml)

statcheckml is the R port of statcheck-ml, a machine-learned replacement for
the extraction step of the R package statcheck. It reads a PDF or plain
text and finds every reported statistical test result with a
character-level model shipped inside the package's kit. It then recomputes
each result's p-value with base R arithmetic and reports whether the
reported and the recomputed p-values agree. The model only finds results;
it never judges them, so the verdict comes from closed-form mathematics in
every case.

## Install

statcheckml is not yet on CRAN.

```r
remotes::install_github("rasoulnorouzi/statcheck-ml-r")
```

Or from a clone:

```r
# in a shell, inside the clone
R CMD INSTALL .
```

### System requirement

Reading a PDF needs pdftools, which bundles or links poppler. On Windows
and macOS, `install.packages("pdftools")` gets a poppler binary along with
the package, so nothing further is needed. On Linux, install the poppler
development headers before installing pdftools:

```
sudo apt-get install libpoppler-cpp-dev   # Debian, Ubuntu
```

pdftools is an Imports dependency of statcheckml, not a Suggests one:
`sc_check()` and `sc_read_pdf()` are exported functions built around it, so
there is no supported way to use this package without a working pdftools
install.

## Use

```r
library(statcheckml)
res <- sc_check("paper.pdf")
res
```

`sc_check()` needs a `kit` (the shared rules) and a `model` (the trained
tagger). Both default to the package's own copies, loaded on first use.
Loading the model costs about a second, so a caller checking many PDFs
should load it once and pass it to every call:

```r
kit <- sc_kit()
model <- sc_load_model(kit)
res <- sc_check("paper.pdf", kit, model)
```

Running it on the damaged sample PDF shipped with the package:

```r
library(statcheckml)
res <- sc_check(system.file("extdata/sample_paper_damaged.pdf", package = "statcheckml"))
res
```

```
                            source_file  source test_type statistic df1 df2
1 inst/extdata/sample_paper_damaged.pdf pattern         t      2.45  23  NA
2 inst/extdata/sample_paper_damaged.pdf pattern         f      5.10   2  30
3 inst/extdata/sample_paper_damaged.pdf pattern         f      9.20   1 118
4 inst/extdata/sample_paper_damaged.pdf pattern         t      1.80  46  NA
5 inst/extdata/sample_paper_damaged.pdf pattern         t      4.15  19  NA
  p_operator p_value   computed_p        verdict line
1          =   0.022 0.0223157282     consistent    2
2          =   0.012 0.0124001810     consistent    2
3          =   0.003 0.0029766142     consistent    2
4          =   0.040 0.0784206648 decision_error    2
5          =   0.001 0.0005439714     consistent    4
                                                          reason
1
2
3
4 the reported and computed p-values disagree about significance
5
```

This PDF has its operators destroyed on purpose (the source paper reads
`t(23) = 2.45` as `t(23) \x03 2.45`), so `sc_check()` also shows the repair
stage: it recovers the `=` before pattern matching runs, from the p-values
that agree with the recomputed ones. Row 4 stays a `decision_error` on
purpose: `t(46) = 1.80` is reported significant at `p = .04`, but the
recomputed p-value is `.078`.

### Columns

| column | meaning |
|---|---|
| `source_file` | the PDF `sc_check()` read (absent from `sc_check_text()`) |
| `source` | `"pattern"` or `"model"`: which stage found the result |
| `test_type` | `t`, `f`, `r`, `z`, `chi2`, or `q` |
| `statistic` | the reported test statistic |
| `df1`, `df2` | degrees of freedom; `NA` when the test does not report one |
| `p_operator` | `"="`, `"<"`, or `">"`, as reported |
| `p_value` | the reported p-value |
| `computed_p` | the p-value recomputed from `statistic` and the degrees of freedom |
| `verdict` | `"consistent"`, `"inconsistent"`, `"decision_error"`, or `"undecidable"` |
| `line` | the line of the normalised text the result's window starts at |
| `reason` | why the verdict is what it is; empty when `"consistent"` |

### Text you already have

`sc_check()` is `sc_read_pdf()` followed by `sc_check_text()`. If the text
is already in hand -- read some other way, or pasted from an article --
call `sc_check_text()` directly and skip PDF reading entirely:

```r
kit <- sc_kit()
model <- sc_load_model(kit)
sc_check_text("The effect was significant, t(28) = 2.87, p = .006.", kit, model)
```

## Stages

Every stage in the mother project's pipeline is ported. `sc_check()` and
`sc_check_text()` run all of them in order; each is also usable on its own.

| stage | function |
|---|---|
| read a PDF to text | `sc_read_pdf()` |
| normalise the text | `sc_normalize()` |
| repair a damaged operator | `sc_repair()` |
| prefilter a line | `sc_prefilter()` |
| find a result, pattern baseline | `sc_extract()` |
| find a result, trained model | `sc_tag()` plus `sc_group()` |
| check a p-value | `sc_compute_p()` and `sc_verdict()` |
| the whole pipeline, PDF to checked results | `sc_check()` |
| the whole pipeline, text to checked results | `sc_check_text()` |

## The kit

Every function that reads a rule takes a `kit`, loaded with `sc_kit()`. A
kit holds the prefilter, the normalisation rules, the character
vocabulary, the p-value constants, and one or more trained models -- all
exported by the mother project and shipped read-only inside this package
at `inst/kit`, described in its own `inst/kit/README.md`.

`sc_kit()` verifies every file's hash against the kit's `manifest.json`
before it returns, so an install whose kit has drifted from the mother
project is refused rather than silently trusted. `sc_kit_info()` prints
what is loaded:

```r
sc_kit_info(sc_kit())
```

```
statcheck-ml kit 2.0.0
mother commit b71a9f92304cb99144279a852b7c815471be0168
models: gru-crf
```

## Limits

- The model runs in pure R: about 8 seconds per 100 windows of tagging.
  A long article, with more windows to tag, can take close to a minute.
- pdftools (poppler) reads less of some PDFs than the PyMuPDF engine the
  models were trained on. Measured across a 200-document, 327-result
  corpus, poppler's recall is 0.872 against 0.911 for PyMuPDF -- a real
  gap, not a rounding difference, though it did not show up on the small
  sample PDFs in this repository.
- The trained model was fit on bronze-tier labels: machine-annotated text,
  never a human-adjudicated gold set. See the mother project's `PLAN.md`
  for the label tiers and what each one means.
