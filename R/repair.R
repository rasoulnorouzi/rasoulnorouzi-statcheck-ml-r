# Repair damaged operators, and let the arithmetic choose the mapping.
#
# A publisher sets an operator in a font with no ToUnicode map, so the
# character is lost, but the numbers survive. Every candidate operator is
# tried, the p-value is recomputed for each, and the mapping that agrees
# with the reported p-values most often wins. The test is independent of any
# annotator: a mapping that produces 80% disagreement is wrong regardless of
# how plausible it looked.
#
# The rules live in `spec/repair.json`, so that the Python, JavaScript and R
# ports read one definition instead of three copies.

# Match `pattern` against `text` and return every match's captured groups, as
# a list of character vectors (`NA_character_` for a group that did not
# participate). Positional, unlike `extract.R`'s named-capture reader,
# because `repair.json`'s pattern captures by position.
regex_captures <- function(text, pattern, ignore.case = FALSE) {
  m <- gregexpr(pattern, text, perl = TRUE, ignore.case = ignore.case)[[1]]
  if (m[1] == -1) return(list())
  starts <- attr(m, "capture.start")
  lens <- attr(m, "capture.length")
  lapply(seq_along(m), function(i) {
    vapply(seq_len(ncol(starts)), function(j) {
      if (starts[i, j] <= 0) NA_character_
      else substr(text, starts[i, j], starts[i, j] + lens[i, j] - 1L)
    }, character(1))
  })
}

# `re.sub(pattern, fn, text)`: rebuild `text` with every match replaced by
# `fn(whole_match, captures)`. Needed because R has no callback form of
# `gsub`, and both repair stages replace only what a match captured, not the
# whole line.
regex_sub <- function(text, pattern, fn, ignore.case = FALSE) {
  m <- gregexpr(pattern, text, perl = TRUE, ignore.case = ignore.case)[[1]]
  if (m[1] == -1) return(text)
  starts <- as.integer(m)
  mlens <- attr(m, "match.length")
  cstarts <- attr(m, "capture.start")
  clens <- attr(m, "capture.length")
  n <- length(starts)

  pieces <- character(2L * n + 1L)
  pos <- 1L
  for (i in seq_len(n)) {
    pieces[2L * i - 1L] <- substr(text, pos, starts[i] - 1L)
    whole <- substr(text, starts[i], starts[i] + mlens[i] - 1L)
    caps <- vapply(seq_len(ncol(cstarts)), function(j) {
      if (cstarts[i, j] <= 0) NA_character_
      else substr(text, cstarts[i, j], cstarts[i, j] + clens[i, j] - 1L)
    }, character(1))
    pieces[2L * i] <- fn(whole, caps)
    pos <- starts[i] + mlens[i]
  }
  pieces[2L * n + 1L] <- substr(text, pos, nchar(text, type = "chars"))
  paste(pieces, collapse = "")
}

# All combinations of `values` taken `k` at a time, in `itertools.product`
# order: the LAST position varies fastest. `infer_validated` tries every
# mapping in this order and keeps the first to reach the best score, so a
# tie must land on the same mapping Python picks.
product_combos <- function(values, k) {
  if (k == 0L) return(list(character(0)))
  rest <- product_combos(values, k - 1L)
  out <- vector("list", length(values) * length(rest))
  idx <- 1L
  for (v in values) {
    for (r in rest) {
      out[[idx]] <- c(v, r)
      idx <- idx + 1L
    }
  }
  out
}

# Build the character class of a possible damaged operator from
# `suspect_characters`. Only `\`, `]`, `^` and `-` need escaping inside a
# bracket expression, and repair.json's literal set holds only one of them.
escape_class_char <- function(ch) {
  if (ch %in% c("]", "^", "-", "\\")) paste0("\\", ch) else ch
}

suspect_class <- function(spec) {
  ranges <- vapply(spec$suspect_characters$control_range, function(r) {
    sprintf("\\x%02x-\\x%02x", as.integer(r[[1]]), as.integer(r[[2]]))
  }, character(1))
  literal <- vapply(spec$suspect_characters$literal, escape_class_char, character(1))
  paste0("[", paste(ranges, collapse = ""), paste(literal, collapse = ""), "]")
}

repair_spec <- function(kit) {
  spec <- kit$spec$repair
  list(
    operators = unlist(spec$operators, use.names = FALSE),
    max_suspects = spec$max_suspects,
    min_testable = spec$min_testable_results,
    result_pattern = gsub("SUSPECT", suspect_class(spec), spec$result_pattern, fixed = TRUE)
  )
}

# Every complete result in the text, with its two operator characters. The
# seven captures are, in order: test name, df1, df2, the stat operator, the
# statistic, the p operator, the p-value.
repair_candidates <- function(text, spec) {
  lapply(regex_captures(text, spec$result_pattern, ignore.case = TRUE), function(caps) {
    list(test = tolower(caps[1]), df1 = sc_parse_number(caps[2]),
        df2 = sc_parse_number(caps[3]), stat_op = caps[4],
        stat = sc_parse_number(caps[5]), p_op = caps[6], p = sc_parse_number(caps[7]))
  })
}

# The number of digits after the decimal point in a value already parsed as
# a float -- different from `pvalue.R`'s `p_decimals`, which reads the text
# as written. `repair_validated.py` keeps its own copy for the same reason:
# by the time a candidate's p-value reaches this check, the text it was
# written with is gone.
repair_decimals <- function(value) {
  s <- sub("0+$", "", sprintf("%.10f", value))
  dot <- regexpr(".", s, fixed = TRUE)
  if (dot[1] == -1) return(0L)
  nchar(substring(s, dot[1] + 1L), type = "chars")
}

CHI2_ALIAS <- c(c2 = "chi2", v2 = "chi2", x2 = "chi2", "\u03c72" = "chi2")

# Does the recomputed p-value agree with a candidate's reported one, once
# its p operator is read as `p_operator`?
repair_agrees <- function(row, p_operator) {
  test <- unname(CHI2_ALIAS[row$test])
  if (is.na(test)) test <- row$test
  computed <- sc_compute_p(test, row$stat, row$df1, row$df2)
  if (is.na(computed) || is.na(row$p)) return(NA)
  if (identical(p_operator, "=")) {
    return(abs(computed - row$p) <= max(0.5 * 10 ^ (-repair_decimals(row$p)), 1e-6))
  }
  if (identical(p_operator, "<")) return(computed < row$p)
  computed > row$p
}

# How many of `rows` agree under `mapping`, and how many could be tested at
# all. A test statistic is always reported with an equals sign, so a
# mapping that reads it as an inequality is not worth testing further.
score_mapping <- function(rows, mapping) {
  agree <- 0L
  testable <- 0L
  for (row in rows) {
    sop <- if (is.null(mapping[[row$stat_op]])) row$stat_op else mapping[[row$stat_op]]
    pop <- if (is.null(mapping[[row$p_op]])) row$p_op else mapping[[row$p_op]]
    if (!identical(sop, "=")) next
    verdict <- repair_agrees(row, pop)
    if (is.na(verdict)) next
    testable <- testable + 1L
    if (isTRUE(verdict)) agree <- agree + 1L
  }
  c(agree = agree, testable = testable)
}

# Choose the operator mapping the arithmetic supports best. Returns an empty
# mapping when there is nothing to infer or too few results to test one,
# and the caller falls back to the simple rule.
infer_validated <- function(text, spec) {
  rows <- repair_candidates(text, spec)
  if (length(rows) == 0) return(list())

  ops_used <- unique(c(vapply(rows, `[[`, character(1), "stat_op"),
                       vapply(rows, `[[`, character(1), "p_op")))
  suspects <- setdiff(ops_used, spec$operators)
  # Sorted by code point, never by `order()` on the characters themselves:
  # string order follows the locale, and the same input would then assign
  # different slots on different machines.
  suspects <- suspects[order(vapply(suspects, utf8ToInt, integer(1)))]
  if (length(suspects) == 0 || length(suspects) > spec$max_suspects) return(list())

  best <- list()
  best_rate <- -1
  best_testable <- 0L
  for (combo in product_combos(spec$operators, length(suspects))) {
    mapping <- stats::setNames(as.list(combo), suspects)
    scored <- score_mapping(rows, mapping)
    if (scored[["testable"]] < spec$min_testable) next
    rate <- scored[["agree"]] / scored[["testable"]]
    if (rate > best_rate || (rate == best_rate && scored[["testable"]] > best_testable)) {
      best <- mapping
      best_rate <- rate
      best_testable <- scored[["testable"]]
    }
  }
  best
}

# The characters a font without a ToUnicode map can leave in place of an
# operator: the control range `repair.json` also lists, plus the fraction
# sign, a backslash, and the letters a broken glyph table maps to instead.
# Hardcoded, because this simple fallback is not the part of `repair.py`
# that reads the shared spec in the mother repository either.
SIMPLE_CONTROL_CLASS <- "\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f"
ANCHOR_STAT_PATTERN <- paste0("\\)\\s*([", SIMPLE_CONTROL_CLASS, "\u00bc\\\\!bNp])\\s*-?\\d*\\.?\\d")
ANCHOR_P_PATTERN <- paste0("\\bp\\s*([", SIMPLE_CONTROL_CLASS, "\u00bc\\\\!bN])\\s*(-?\\d*\\.?\\d+)")

# Work out what each suspect character means from two anchors: the character
# right after the degrees of freedom is always an equals sign, and the
# character after "p" is a threshold-shaped value nine times in ten when it
# is a less-than sign.
simple_infer_map <- function(text) {
  mapping <- list()
  for (caps in regex_captures(text, ANCHOR_STAT_PATTERN)) {
    ch <- caps[1]
    if (is.null(mapping[[ch]])) mapping[[ch]] <- "="
  }

  after_p <- list()
  for (caps in regex_captures(text, ANCHOR_P_PATTERN)) {
    ch <- caps[1]
    if (!is.null(mapping[[ch]])) next
    v <- suppressWarnings(as.numeric(caps[2]))
    if (is.na(v)) next
    after_p[[ch]] <- c(after_p[[ch]], v)
  }

  thresholds <- c(0.05, 0.01, 0.001, 0.0001, 0.1)
  for (ch in names(after_p)) {
    share_round <- sum(after_p[[ch]] %in% thresholds) / max(length(after_p[[ch]]), 1L)
    mapping[[ch]] <- if (share_round >= 0.8) "<" else "="
  }
  mapping
}

# The fallback repair: only the positions an operator can occupy are
# touched, so a letter elsewhere in the document is left alone.
simple_repair <- function(text) {
  mapping <- simple_infer_map(text)
  if (length(mapping) == 0) return(list(text = text, replacements = 0L))

  replaced <- 0L
  apply_anchor <- function(text_in, pattern) {
    regex_sub(text_in, pattern, function(whole, caps) {
      ch <- caps[1]
      if (is.null(mapping[[ch]])) return(whole)
      replaced <<- replaced + 1L
      sub(ch, mapping[[ch]], whole, fixed = TRUE)
    })
  }

  out <- apply_anchor(text, ANCHOR_STAT_PATTERN)
  out <- apply_anchor(out, ANCHOR_P_PATTERN)
  list(text = out, replacements = replaced)
}

#' Repair operators a PDF conversion destroyed
#'
#' Tries every plausible reading of a document's damaged operator characters
#' and keeps the one whose recomputed p-values agree with the reported ones
#' most often -- a test that needs no annotator, only the p-value
#' mathematics in [sc_compute_p()]. When too few results in the document can
#' be tested this way, falls back to a simpler rule: the character right
#' after a set of degrees of freedom is always an equals sign.
#'
#' @param text A document, already normalised with [sc_normalize()].
#' @param kit A loaded [sc_kit()].
#' @return A list with `text` (the repaired document) and `replacements`
#'   (how many operator characters were changed).
#' @examples
#' kit <- sc_kit()
#' sc_repair("F(1, 214) \x01 50.54, p \x01 .001", kit)
#' @export
sc_repair <- function(text, kit) {
  spec <- repair_spec(kit)
  mapping <- infer_validated(text, spec)

  if (length(mapping) == 0) {
    fallback <- simple_repair(text)
    return(list(text = fallback$text, replacements = fallback$replacements))
  }

  fixed <- regex_sub(text, spec$result_pattern, function(whole, caps) {
    for (ch in names(mapping)) whole <- gsub(ch, mapping[[ch]], whole, fixed = TRUE)
    whole
  }, ignore.case = TRUE)

  # Counts every occurrence of a mapped character anywhere in the document,
  # not only the ones inside a matched result -- the same count
  # `repair_validated.py` reports, kept faithful rather than made precise.
  replacements <- sum(vapply(names(mapping), function(ch) {
    m <- gregexpr(ch, text, fixed = TRUE)[[1]]
    if (m[1] == -1) 0L else length(m)
  }, integer(1)))

  list(text = fixed, replacements = as.integer(replacements))
}
