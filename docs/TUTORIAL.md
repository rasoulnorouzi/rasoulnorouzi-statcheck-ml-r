---
title: "statcheckml"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{statcheckml}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## What it does

statcheckml reads a PDF or plain text, finds every reported statistical test
result, and checks whether the result's p-value is consistent with its test
statistic and degrees of freedom. It is the R port of statcheck-ml, a
machine-learned replacement for the extraction step of the R package
`statcheck`.

A trained character-level model finds where a result sits in the text. It
never decides whether that result is correct. The p-value comparison that
follows is closed-form arithmetic -- `pt()`, `pf()`, `pchisq()`, `pnorm()` --
run the same way whether the result came from the model or from the pattern
baseline. No model output reaches a verdict.

The model was trained on bronze labels: three rater agents (Claude haiku,
sonnet, and opus) annotated the training and holdout windows, a scripted
consensus rule combined their readings, and an adjudicator agent settled
what the rule could not. No person checked a label by hand, and there is no
human gold set. The mother project's `PLAN.md` calls this the bronze tier,
and reports every number against it, never against a claim of ground truth.

Measured on the holdout (576 windows from 515 documents, 327 results), the cascade -- the
pattern baseline after operator repair, plus what the model finds beyond it
-- reaches F1 0.908 [0.875, 0.937]. The pattern baseline alone reaches 0.636
after repair and 0.308 on raw text. Source: the mother repository
`statcheck-ml`, `results/REPORT.md`, Abstract and section 6.

## Install


``` r
remotes::install_github("rasoulnorouzi/statcheck-ml-r")
```

Or from a clone:


``` r
# in a shell, inside the clone
# R CMD INSTALL .
```

Not yet on CRAN; when it is, `install.packages("statcheckml")` replaces
both.

### System requirement

Reading a PDF needs pdftools, an Imports dependency, which bundles or links
poppler. On Windows and macOS, `install.packages("pdftools")` installs a
poppler binary with the package, so nothing further is needed. On Debian and
Ubuntu, install the poppler development headers first:


``` r
# sudo apt-get install libpoppler-cpp-dev
```

`sc_check()` and `sc_read_pdf()` are exported functions built around
pdftools, so there is no supported way to use this package without a
working pdftools install.

## First run


``` r
library(statcheckml)
sc_check(system.file("extdata/sample_paper_damaged.pdf", package = "statcheckml"))
#>                                                                                               source_file
#> 1 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 2 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 3 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 4 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 5 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#>    source test_type statistic df1 df2 p_operator p_value   computed_p
#> 1 pattern         t      2.45  23  NA          =   0.022 0.0223157282
#> 2 pattern         f      5.10   2  30          =   0.012 0.0124001810
#> 3 pattern         f      9.20   1 118          =   0.003 0.0029766142
#> 4 pattern         t      1.80  46  NA          =   0.040 0.0784206648
#> 5 pattern         t      4.15  19  NA          =   0.001 0.0005439714
#>          verdict line
#> 1     consistent    2
#> 2     consistent    2
#> 3     consistent    2
#> 4 decision_error    2
#> 5     consistent    4
#>                                                           reason
#> 1                                                               
#> 2                                                               
#> 3                                                               
#> 4 the reported and computed p-values disagree about significance
#> 5
```

`sample_paper_damaged.pdf` is a copy of the bundled sample paper with its
operators destroyed on purpose: the source reads `t(23) = 2.45`, and this
copy reads `t(23) \x03 2.45`. `sc_check()` repairs the operator before
matching, and row 4 stays a `decision_error` on purpose -- `t(46) = 1.80` is
reported significant at `p = .04`, but the recomputed p-value is `.078`.

| column | meaning |
|---|---|
| `source_file` | the PDF `sc_check()` read; absent from `sc_check_text()` |
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

## The stages, on one damaged sentence

`sc_check()` runs five stages in order: normalise, repair, prefilter, find,
check. Each is a function on its own. This section runs all five, plus
`sc_tag()`, on one sentence whose stat operator a PDF font destroyed:


``` r
kit <- sc_kit()
model <- sc_load_model(kit)
sentence <- "F(1, 40) \x02 6.20, p = .016"
sentence
#> [1] "F(1, 40) \002 6.20, p = .016"
```

`\x02` is a control character, the kind a font with no ToUnicode map
leaves where an operator belongs. `sc_normalize()` does not touch it here --
it is already one of the kit's canonical operator slots, so there is
nothing to rename:


``` r
normalised <- sc_normalize(sentence, kit)
normalised
#> [1] "F(1, 40) \002 6.20, p = .016"
```

`sc_repair()` tries to read what the control character means. One result is
too few to test a mapping by arithmetic (the kit's `repair.json` sets that
floor at three testable results), so it falls back to the simpler rule: the
character right after a set of degrees of freedom is always an equals sign:


``` r
repaired <- sc_repair(normalised, kit)
repaired
#> $text
#> [1] "F(1, 40) = 6.20, p = .016"
#> 
#> $replacements
#> [1] 1
```

`sc_prefilter()` selects the windows of text worth reading further. A
one-line document keeps its one line, with no context lines to add:


``` r
windows <- sc_prefilter(repaired$text, kit)
windows
#>   start end line                      text
#> 1     0  25    0 F(1, 40) = 6.20, p = .016
```

`sc_extract()` is the pattern baseline -- what the R package `statcheck`
would find, faithfully ported faults and all:


``` r
hits <- sc_extract(windows$text[1], kit)
hits
#>   test_type statistic df1 df2 p_operator p_value
#> 1         f      6.20   1  40          =    .016
```

`sc_tag()` is the trained model, run on the same window. It returns one
BIOES tag per character:


``` r
tags <- sc_tag(windows$text[1], kit, model)
tags[[1]]
#>  [1] "S-TEST"   "O"        "O"        "O"        "O"        "B-DF2"   
#>  [7] "E-DF2"    "O"        "O"        "S-POP_EQ" "O"        "B-STAT"  
#> [13] "I-STAT"   "I-STAT"   "E-STAT"   "O"        "O"        "O"       
#> [19] "O"        "O"        "O"        "B-PVAL"   "I-PVAL"   "I-PVAL"  
#> [25] "E-PVAL"
```

The pattern already found this result, so `sc_check_text()` would keep the
pattern's reading and never call the model on this window at all -- the
model only runs on what the prefilter kept and the pattern missed. Checking
the pattern's own reading:


``` r
result <- list(
  test_type = hits$test_type[1],
  statistic = sc_parse_number(hits$statistic[1]),
  df1 = sc_parse_number(hits$df1[1]),
  df2 = sc_parse_number(hits$df2[1]),
  p_operator = hits$p_operator[1],
  p_value = sc_parse_number(hits$p_value[1])
)
sc_verdict(result, reported_p_text = hits$p_value[1])
#> $verdict
#> [1] "inconsistent"
#> 
#> $computed_p
#> [1] 0.01702996
#> 
#> $reported_p
#> [1] 0.016
#> 
#> $reason
#> [1] "the reported and computed p-values disagree"
#> 
#> $missing
#> character(0)
```

`F(1, 40) = 6.20` implies `p = .0170`, and the sentence reports `.016` --
close, but outside the tolerance `sc_verdict()` allows for three decimals,
so the verdict is `inconsistent` rather than `consistent`.

### Where the rules live

Every stage above reads its rules from `kit`, not from R code. The rules
live in `inst/kit/spec/*.json` -- `normalize.json`, `repair.json`,
`prefilter.json`, `charmap.json`, `font_table.json` -- shipped read-only
inside the package and exported by the mother project. No port restates a
rule in its own language.

`sc_kit()` checks every file the kit's `manifest.json` lists against a
sha256 hash before it returns, so a copy that has drifted from the mother
project is refused rather than silently trusted. `sc_kit_info()` prints
what is loaded:


``` r
sc_kit_info(kit)
#> statcheck-ml kit 2.0.0
#> mother commit b71a9f92304cb99144279a852b7c815471be0168
#> models: gru-crf
```

## The API

Every exported function, its arguments and their defaults, what it returns,
and one call that runs.

### `sc_kit(path = system.file("kit", package = "statcheckml"))`

Loads and verifies a kit. Returns an `sc_kit`: a list with `spec` (one
parsed JSON per file in `spec/`, named after the file), `model_dir`, the
parsed `manifest`, and `path`.


``` r
kit <- sc_kit()
kit$spec$normalize$target_line_width
#> [1] 90
```

### `sc_kit_info(kit)`

Prints the kit version, the mother repository commit it was exported from,
and the models it ships. Returns `kit`, invisibly.


``` r
sc_kit_info(kit)
#> statcheck-ml kit 2.0.0
#> mother commit b71a9f92304cb99144279a852b7c815471be0168
#> models: gru-crf
```

### `sc_load_model(kit, config = kit$manifest$model_default)`

Reads a model's `weights.json` -- the embedding table, both directions of
both recurrent layers, the tag projection, and the CRF -- and returns an
`sc_model`.


``` r
model <- sc_load_model(kit)
model
#> <sc_model> gru-crf: 2 bidirectional GRU layers, 128 hidden, 37 tags, CRF
```

### `sc_tag(texts, kit, model = sc_load_model(kit))`

Tags the characters of a text (or a character vector of texts) with the
trained model. Returns a list of character vectors, one BIOES tag per
character, one vector per text.


``` r
sc_tag("F(2, 30) = 4.11, p = .03", kit, model)[[1]]
#>  [1] "S-TEST"   "O"        "S-DF1"    "O"        "O"        "B-DF2"   
#>  [7] "E-DF2"    "O"        "O"        "S-POP_EQ" "O"        "B-STAT"  
#> [13] "I-STAT"   "I-STAT"   "E-STAT"   "O"        "O"        "O"       
#> [19] "O"        "O"        "O"        "B-PVAL"   "I-PVAL"   "E-PVAL"
```

### `sc_normalize(text, kit)`

Cuts a joined column back into lines of a normal width, and renames a
damaged operator to a character the model knows. Returns the normalised
text; adds and removes no other text.


``` r
sc_normalize("one\r\ntwo\rthree\n", kit)
#> [1] "one\ntwo\nthree\n"
```

### `sc_repair(text, kit)`

Recovers a destroyed operator by arithmetic: every plausible reading is
tried, and the one whose recomputed p-values agree with the reported ones
most often wins. Returns a list with `text` (the repaired document) and
`replacements` (how many operator characters changed).


``` r
sc_repair("F(1, 214) \x01 50.54, p \x01 .001", kit)
#> $text
#> [1] "F(1, 214) = 50.54, p = .001"
#> 
#> $replacements
#> [1] 2
```

### `sc_prefilter(text, kit, drop_references = TRUE)`

Selects the windows of text worth reading further: a line survives if it is
long enough, holds a digit, and is dense enough in non-letter characters.
Returns a data frame with one row per kept window: `start`, `end`, `line`,
`text`.


``` r
sc_prefilter("The effect was significant, t(28) = 2.87, p = .006.", kit)
#>   start end line                                                text
#> 1     0  51    0 The effect was significant, t(28) = 2.87, p = .006.
```

### `sc_extract(text, kit)`

The pattern-based baseline: what `statcheck`'s regular expressions find.
Returns a data frame, one row per match: `test_type`, `statistic`, `df1`,
`df2`, `p_operator`, `p_value`, all character, as written.


``` r
sc_extract("The main effect was significant, F(2, 87) = 4.11, p = .03.", kit)
#>   test_type statistic df1 df2 p_operator p_value
#> 1         f      4.11   2  87          =     .03
```

### `sc_group(text, tags)`

Groups a `sc_tag()` output back into results: reads spans out of the BIOES
tags, groups them one result per test, and parses each part into a number.
Returns a data frame: `test_type`, `statistic`, `df1`, `df2`, `p_operator`,
`p_value`, `start`, `end`.


``` r
txt <- "F(2, 30) = 4.11, p = .03"
sc_group(txt, sc_tag(txt, kit, model)[[1]])
#>   test_type statistic df1 df2 p_operator p_value start end
#> 1         f      4.11   2  30          =    0.03     0  24
```

### `sc_compute_p(test_type, statistic, df1 = NA, df2 = NA, one_tailed = FALSE)`

Recomputes the p-value a test statistic implies, two-tailed by default.
Returns a number, or `NA_real_` when it cannot be computed -- a
non-positive degree of freedom, a correlation at or beyond 1, or an
unrecognised test name.


``` r
sc_compute_p("f", 4.11, 2, 87)
#> [1] 0.01969753
sc_compute_p("t", 2.87, 28)
#> [1] 0.00772796
```

### `sc_verdict(result, alpha = 0.05, p_equal_alpha_sig = TRUE, reported_p_text = NULL)`

Compares a reported p-value against the one its statistic implies. `result`
is a list with `test_type`, `statistic`, `df1`, `df2`, `p_operator`,
`p_value`. Returns a list: `verdict`, `computed_p`, `reported_p`, `reason`,
`missing`.


``` r
sc_verdict(list(test_type = "f", statistic = 4.11, df1 = 2, df2 = 87,
                 p_operator = "=", p_value = 0.03))
#> $verdict
#> [1] "inconsistent"
#> 
#> $computed_p
#> [1] 0.01969753
#> 
#> $reported_p
#> [1] 0.03
#> 
#> $reason
#> [1] "the reported and computed p-values disagree"
#> 
#> $missing
#> character(0)
```

### `sc_parse_number(text)`

Parses a number written in a paper's text: strips a leading comparison
operator, restores a missing leading zero, and accepts a Unicode minus or
en dash for a hyphen. Returns a number, or `NA_real_`.


``` r
sc_parse_number("<.001")
#> [1] 0.001
sc_parse_number(".03")
#> [1] 0.03
```

### `sc_check_text(text, kit = sc_kit(), model = sc_load_model(kit))`

The whole pipeline from text to checked results: normalise, repair,
prefilter, find with the pattern then the model, check. Returns a data
frame, one row per result found, with the stage counts attached as the
`"stages"` attribute.


``` r
sc_check_text("The effect was significant, t(28) = 2.87, p = .006.", kit, model)
#>    source test_type statistic df1 df2 p_operator p_value computed_p
#> 1 pattern         t      2.87  28  NA          =   0.006 0.00772796
#>        verdict line                                      reason
#> 1 inconsistent    0 the reported and computed p-values disagree
```

This sentence was written for this example, not copied from a paper: `t(28)
= 2.87` implies `p = .0077`, not `.006`, so `sc_check_text()` correctly
calls it inconsistent.

### `sc_read_pdf(path, kit, timeout = 60)`

Reads one PDF with pdftools and normalises the text. Returns the normalised
text, or `""` when the document could not be read within `timeout` seconds
(R.utils installed) or at all.


``` r
txt <- sc_read_pdf(system.file("extdata/sample_paper.pdf", package = "statcheckml"), kit)
substr(txt, 1, 120)
#> [1] "Attention and recall under time pressure\nA. Example, B. Sample, and C. Fictional\nDepartment of Nothing in Particular\n\nAb"
```

### `sc_check(path, kit = sc_kit(), model = sc_load_model(kit), timeout = 60)`

Reads a PDF and checks every result in it: `sc_read_pdf()` followed by
`sc_check_text()`. Returns the same data frame as `sc_check_text()`, with
`source_file` added as the first column.


``` r
sc_check(system.file("extdata/sample_paper_damaged.pdf", package = "statcheckml"), kit, model)
#>                                                                                               source_file
#> 1 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 2 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 3 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 4 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 5 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#>    source test_type statistic df1 df2 p_operator p_value   computed_p
#> 1 pattern         t      2.45  23  NA          =   0.022 0.0223157282
#> 2 pattern         f      5.10   2  30          =   0.012 0.0124001810
#> 3 pattern         f      9.20   1 118          =   0.003 0.0029766142
#> 4 pattern         t      1.80  46  NA          =   0.040 0.0784206648
#> 5 pattern         t      4.15  19  NA          =   0.001 0.0005439714
#>          verdict line
#> 1     consistent    2
#> 2     consistent    2
#> 3     consistent    2
#> 4 decision_error    2
#> 5     consistent    4
#>                                                           reason
#> 1                                                               
#> 2                                                               
#> 3                                                               
#> 4 the reported and computed p-values disagree about significance
#> 5
```

## Reading a verdict

`sc_verdict()` returns one of four strings.

`consistent`: the reported and the recomputed p-value agree.


``` r
sc_verdict(list(test_type = "t", statistic = 2.45, df1 = 23, df2 = NA_real_,
                 p_operator = "=", p_value = 0.022))
#> $verdict
#> [1] "consistent"
#> 
#> $computed_p
#> [1] 0.02231573
#> 
#> $reported_p
#> [1] 0.022
#> 
#> $reason
#> [1] ""
#> 
#> $missing
#> character(0)
```

`inconsistent`: they disagree, but the disagreement does not flip
significance at `alpha`.


``` r
sc_verdict(list(test_type = "f", statistic = 6.20, df1 = 1, df2 = 40,
                 p_operator = "=", p_value = 0.016))
#> $verdict
#> [1] "inconsistent"
#> 
#> $computed_p
#> [1] 0.01702996
#> 
#> $reported_p
#> [1] 0.016
#> 
#> $reason
#> [1] "the reported and computed p-values disagree"
#> 
#> $missing
#> character(0)
```

`decision_error`: they disagree, and the disagreement flips significance --
one side of `alpha`, the other side not.


``` r
sc_verdict(list(test_type = "t", statistic = 1.80, df1 = 46, df2 = NA_real_,
                 p_operator = "=", p_value = 0.040))
#> $verdict
#> [1] "decision_error"
#> 
#> $computed_p
#> [1] 0.07842066
#> 
#> $reported_p
#> [1] 0.04
#> 
#> $reason
#> [1] "the reported and computed p-values disagree about significance"
#> 
#> $missing
#> character(0)
```

`undecidable`: there is not enough information to judge -- here, no
reported p-value at all.


``` r
sc_verdict(list(test_type = "t", statistic = 2.45, df1 = 23, df2 = NA_real_))
#> $verdict
#> [1] "undecidable"
#> 
#> $computed_p
#> [1] 0.02231573
#> 
#> $reported_p
#> [1] NA
#> 
#> $reason
#> [1] "no p-value found beside this result"
#> 
#> $missing
#> [1] "p_value"
```

### The rounding rule

`sc_verdict()` reads how many digits the p-value was reported with, from
`reported_p_text` when given, and treats the reported value as correct if
the computed one lands within half a unit of that last decimal place.
When `reported_p_text` is not given, the digits are read from
`result$p_value`'s own printed form instead, so a value stored as `0.016`
and one stored as `0.0160` are not read as the same number of decimals even
though they are the same number.

### A known difference from R statcheck

The R package `statcheck` accepts a reported p-value when the interval
implied by the *rounded* test statistic contains it; statcheckml does not
yet widen the check that way. statcheckml also accepts a p-value reported
as zero, or as a fixed small bound, in cases where `statcheck` flags it.
Compared against `statcheck`'s own verdicts on the results it reports after
repair, the two agree on 135 comparisons, disagree on 15, and 2 carry no
p-value to compare. Neither convention is a fault in the extraction; a
future version of the p-value core adopts `statcheck`'s rounding rule.
Source: `results/REPORT.md`, section 7.

## Choosing a model

This kit ships one model, `gru-crf`: two bidirectional GRU layers, 128
hidden units, a linear-chain CRF over 37 BIOES tags.


``` r
model
#> <sc_model> gru-crf: 2 bidirectional GRU layers, 128 hidden, 37 tags, CRF
```

The mother project trained six configurations -- three families (LSTM,
GRU, a dilated CNN), each with a softmax or a CRF head -- and selected by
dev F1. `gru-crf`'s dev F1 is 0.927; its holdout F1 is 0.904 [0.871, 0.934]
(`results/REPORT.md`, section 5). Two more configurations, `gru-softmax`
and `lstm-softmax`, ship in the mother project's model zoo; this R kit
carries `gru-crf` alone. The three do not differ on the holdout: `gru-crf`
against `gru-softmax` differs by 0.001 F1 [-0.022, 0.023] (p = 0.930), and
against `lstm-softmax` by 0.010 F1 [-0.015, 0.036] (p = 0.447) --
both intervals contain zero, so among the three the choice is one of size
and latency, not of accuracy (`results/REPORT.md`, section 7). `gru-crf` is
1.87 MB, 1.81 MB quantised, at a median 5.4 ms per window on one thread;
`lstm-softmax` quantises smaller, 0.64 MB, at 6.1 ms (`results/REPORT.md`,
section 8).

`sc_load_model()`'s `config` argument names a subdirectory of the kit's
`model/` folder. This kit's manifest lists one:


``` r
unlist(kit$manifest$models)
#> [1] "gru-crf"
kit$manifest$model_default
#> [1] "gru-crf"
```

so an explicit `config` is, today, the same call as the default:


``` r
sc_load_model(kit, config = kit$manifest$model_default)
#> <sc_model> gru-crf: 2 bidirectional GRU layers, 128 hidden, 37 tags, CRF
```

A kit built with a second model -- from the mother project's
`pipeline/08_port_kit.py --config gru-softmax` -- would add a
`gru-softmax/` directory next to `gru-crf/` and list it in
`manifest$models`. `sc_load_model(kit, config = "gru-softmax")` would then
read weights from there instead.

## Batch: checking a folder

`sc_check()` takes one PDF at a time. Checking a folder means calling it
once per file and keeping track of which result came from which file. This
runs over the package's own `inst/extdata/`, which ships two PDFs:


``` r
kit <- sc_kit()
model <- sc_load_model(kit)

pdf_dir <- system.file("extdata", package = "statcheckml")
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
pdf_files
#> [1] "C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf"        
#> [2] "C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf"
```


``` r
results <- lapply(pdf_files, sc_check, kit = kit, model = model)
all_results <- do.call(rbind, results)
all_results
#>                                                                                                source_file
#> 1          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 2          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 3          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 4          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 5          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 6          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 7          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 8          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 9          C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper.pdf
#> 10 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 11 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 12 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 13 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#> 14 C:/Users/norouzin/AppData/Local/Programs/R/R-4.6.1/library/statcheckml/extdata/sample_paper_damaged.pdf
#>     source test_type statistic df1 df2 p_operator p_value   computed_p
#> 1  pattern         t      2.45  23  NA          =   0.022 0.0223157282
#> 2  pattern         f      5.10   2  30          =   0.012 0.0124001810
#> 3  pattern      chi2      8.69   1  NA          =   0.003 0.0031996062
#> 4    model         r      0.42  NA  NA          =   0.020           NA
#> 5  pattern         t      1.80  46  NA          =   0.040 0.0784206648
#> 6  pattern         f      9.20   1 118          =   0.003 0.0029766142
#> 7  pattern         z      1.96  NA  NA          =   0.050 0.0499957903
#> 8  pattern         t      4.15  19  NA          <   0.001 0.0005439714
#> 9    model         f      2.71   3  92       <NA>      NA 0.0496037025
#> 10 pattern         t      2.45  23  NA          =   0.022 0.0223157282
#> 11 pattern         f      5.10   2  30          =   0.012 0.0124001810
#> 12 pattern         f      9.20   1 118          =   0.003 0.0029766142
#> 13 pattern         t      1.80  46  NA          =   0.040 0.0784206648
#> 14 pattern         t      4.15  19  NA          =   0.001 0.0005439714
#>           verdict line
#> 1      consistent   17
#> 2      consistent   17
#> 3      consistent   17
#> 4     undecidable   21
#> 5  decision_error   22
#> 6      consistent   25
#> 7      consistent   25
#> 8      consistent   26
#> 9     undecidable   28
#> 10     consistent    2
#> 11     consistent    2
#> 12     consistent    2
#> 13 decision_error    2
#> 14     consistent    4
#>                                                            reason
#> 1                                                                
#> 2                                                                
#> 3                                                                
#> 4                  no degrees of freedom found beside this result
#> 5  the reported and computed p-values disagree about significance
#> 6                                                                
#> 7                                                                
#> 8                                                                
#> 9                             no p-value found beside this result
#> 10                                                               
#> 11                                                               
#> 12                                                               
#> 13 the reported and computed p-values disagree about significance
#> 14
```

`sc_check()` already attaches `source_file` to every row, so `rbind()` is
enough -- there is no need to add a filename column by hand. Writing the
table out:


``` r
out_csv <- file.path(tempdir(), "checked_results.csv")
write.csv(all_results, out_csv, row.names = FALSE)
```

A real folder walk over a directory the build does not own reads the same
way, with a different `pdf_dir`:


``` r
pdf_dir <- "~/papers"
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
results <- lapply(pdf_files, sc_check, kit = kit, model = model)
all_results <- do.call(rbind, results)
write.csv(all_results, "checked_results.csv", row.names = FALSE)
```

## Limits

- **Model time in R.** The model runs in pure R, with no compiled runtime:
  on one thread, tagging 100 windows takes 7.49 seconds in R, against 0.83
  seconds in the Python reference (ONNX Runtime) and 1.01 seconds in the
  browser port (onnxruntime-web). A long article, with hundreds of windows,
  can take close to a minute in R. Source: the mother repository,
  `results/ports.json`.
- **poppler vs. PyMuPDF recall.** pdftools (poppler, this port's reader)
  reads less of some PDFs than the PyMuPDF engine the models were trained
  on. Measured on 200 holdout documents and 327 gold results: PyMuPDF
  recall 0.911, pdfium 0.902, poppler 0.872 -- a spread of 0.040, under the
  mother project's gate of 0.060, but a real gap for any one document.
  Source: `results/REPORT.md`, section 9.
- **What the prefilter drops is gone for good.** A line must be at least 20
  characters, hold a digit, and be at least 20% non-letter characters to
  become a window; text outside the kept windows never reaches the pattern
  or the model. The threshold was chosen for recall, not for looking like a
  result: at 0.20 it keeps every corpus line known to hold a statistic, and
  33.9% of the other lines besides. Source: `inst/kit/spec/prefilter.json`.
- **Damage families the model still misses.** On the holdout's `decimal
  point lost` family (`n = 3`), the best model's recall is 0.000 [0.000,
  0.562] -- the same as `statcheck` after repair. On the `control
  character` family (`n = 82`, the largest damaged family), the best
  model's recall is 0.744 [0.640, 0.826], the cascade's 0.756 [0.653,
  0.836] -- clearly ahead of `statcheck_repaired`'s 0.695 [0.589, 0.784],
  but well under 1. Source: `results/REPORT.md`, section 6.
- **Bronze labels, still.** The model was trained and evaluated against
  labels three rater agents wrote, adjudicated by a fourth agent, with no
  person checking a label by hand. Agreement between raters that share a
  model family cannot catch a mistake all of them make the same way.
  Source: `results/REPORT.md`, section 10; the mother project's `PLAN.md`.

## Elsewhere

- Mother project: [rasoulnorouzi/ml-statcheck](https://github.com/rasoulnorouzi/ml-statcheck#readme)
- Every number in this vignette: [`results/REPORT.md`](https://github.com/rasoulnorouzi/ml-statcheck/blob/main/statcheck-ml/results/REPORT.md)
- Python tutorial: [`statcheck-ml/docs/TUTORIAL_PYTHON.md`](https://github.com/rasoulnorouzi/ml-statcheck/blob/main/statcheck-ml/docs/TUTORIAL_PYTHON.md)
