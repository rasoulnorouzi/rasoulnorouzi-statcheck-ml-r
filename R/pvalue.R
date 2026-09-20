# Recompute a p-value from a test statistic, and compare it with the one a
# paper reports.
#
# This file is deterministic mathematics. No model output ever reaches a
# verdict here: every branch below is explicit, because a confident wrong
# verdict is worse than no tool at all. The same behaviour exists in Python
# and in JavaScript; a change here is a change to all three.

CONSISTENT <- "consistent"
INCONSISTENT <- "inconsistent"      # reported and computed p disagree
DECISION_ERROR <- "decision_error"  # they disagree about significance
UNDECIDABLE <- "undecidable"        # not enough information to judge

# What each kind of test needs before a p-value can be recomputed. A z test
# needs no degrees of freedom, and an F test needs two; every other test
# defaults to one, `df1`, when the test name itself is not recognised.
REQUIRED_PARTS <- list(t = "df1", r = "df1", chi2 = "df1", q = "df1",
                       f = c("df1", "df2"), z = character(0))

# How to name a missing part to a reader. The wording says what the tool
# observed, never why: it read the text and did not find the part. It cannot
# know whether the author omitted it, the PDF conversion destroyed it, or the
# extraction failed. Saying "the paper does not report a p-value" claims the
# first, and a reader would act on that claim.
PART_NAMES <- c(statistic = "no test statistic", df1 = "no degrees of freedom",
                df2 = "no second degrees of freedom", p_value = "no p-value",
                p_operator = "no operator before the p-value")

#' Recompute the p-value a test statistic implies
#'
#' The p-value is two-tailed by default, which is what statcheck assumes. An
#' `r` statistic is converted to a t value on `df1` degrees of freedom before
#' the tail is read. A non-positive degree of freedom, a correlation at or
#' beyond 1, or a test name this function does not recognise all return `NA`
#' rather than the `NaN` R's own distribution functions would otherwise pass
#' through.
#'
#' @param test_type One of `"t"`, `"f"`, `"r"`, `"z"`, `"chi2"`, `"q"`.
#' @param statistic The reported test statistic.
#' @param df1,df2 Degrees of freedom. `df2` matters only for `"f"`.
#' @param one_tailed Halve the p-value. An F test ignores this: it is
#'   one-tailed by construction, and the flag refers to the hypothesis, not to
#'   the distribution.
#' @return The p-value, or `NA_real_` when it cannot be computed.
#' @examples
#' sc_compute_p("f", 4.11, 2, 87)
#' sc_compute_p("t", 2.87, 28)
#' @export
sc_compute_p <- function(test_type, statistic, df1 = NA_real_, df2 = NA_real_,
                         one_tailed = FALSE) {
  test <- if (is.null(test_type) || is.na(test_type)) "" else test_type
  test <- tolower(trimws(test))

  if (is.na(statistic)) return(NA_real_)
  if ((!is.na(df1) && df1 <= 0) || (!is.na(df2) && df2 <= 0)) return(NA_real_)

  # F and chi-square/Q return directly: `one_tailed` describes the
  # hypothesis, and neither distribution's own tail is halved for it.
  p <- switch(test,
    t = {
      if (is.na(df1)) return(NA_real_)
      2 * stats::pt(abs(statistic), df1, lower.tail = FALSE)
    },
    f = {
      if (is.na(df1) || is.na(df2)) return(NA_real_)
      return(stats::pf(statistic, df1, df2, lower.tail = FALSE))
    },
    r = {
      if (is.na(df1)) return(NA_real_)
      if (abs(statistic) >= 1) return(NA_real_)
      tval <- statistic * sqrt(df1 / (1 - statistic^2))
      2 * stats::pt(abs(tval), df1, lower.tail = FALSE)
    },
    z = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
    chi2 = ,
    q = {
      if (is.na(df1)) return(NA_real_)
      return(stats::pchisq(statistic, df1, lower.tail = FALSE))
    },
    return(NA_real_)
  )

  if (one_tailed) p <- p / 2
  if (is.na(p)) NA_real_ else p
}

# The parts a check needs and a result does not carry. More than one
# published result in five carries no p-value at all, measured over 323
# results of the corpus. Nothing can recover a number the author did not
# print, so the caller says which part is absent instead of returning a bare
# "undecidable".
missing_parts <- function(result) {
  get_part <- function(name) if (is.null(result[[name]])) NA else result[[name]]
  statistic <- get_part("statistic")
  df1 <- get_part("df1")
  df2 <- get_part("df2")
  p_value <- get_part("p_value")
  p_operator <- get_part("p_operator")
  test <- tolower(trimws(if (is.null(result$test_type) || is.na(result$test_type))
    "" else result$test_type))

  absent <- character(0)
  if (is.na(statistic)) absent <- c(absent, "statistic")
  parts <- REQUIRED_PARTS[[test]]
  if (is.null(parts)) parts <- "df1"
  values <- list(df1 = df1, df2 = df2)
  for (part in parts) if (is.na(values[[part]])) absent <- c(absent, part)
  if (is.na(p_value)) {
    absent <- c(absent, "p_value")
  } else if (!(p_operator %in% c("=", "<", ">"))) {
    absent <- c(absent, "p_operator")
  }
  absent
}

# Write the missing parts as a sentence a reader can act on. The sentence
# reports an observation, not a cause: whether the author omitted a number,
# the font destroyed it, or the extraction missed it is a separate question,
# answered by the quote beside the result, not by this text.
describe_missing <- function(parts) {
  if (length(parts) == 0) return("")
  names <- vapply(parts, function(p) {
    nm <- unname(PART_NAMES[p])
    if (is.na(nm)) p else nm
  }, character(1))
  if (length(names) == 1) return(paste0(names[1], " found beside this result"))
  paste0(paste(names[-length(names)], collapse = ", "), " and ",
        names[length(names)], " found beside this result")
}

# The number of digits after the decimal point in `text`, read as written
# rather than by counting on the parsed value: `.001` and `0.0010` round the
# same reported value to a different number of decimals.
p_decimals <- function(text) {
  s <- as.character(text)
  dot <- regexpr(".", s, fixed = TRUE)
  if (dot[1] == -1) return(0L)
  nchar(substring(s, dot[1] + 1L), type = "chars")
}

sc_is_significant <- function(p, alpha = 0.05, p_equal_alpha_sig = TRUE) {
  if (p_equal_alpha_sig) p <= alpha else p < alpha
}

#' Compare a reported p-value with the one its statistic implies
#'
#' NAME NOTE: this is the arithmetic check, not the pipeline's PDF entry
#' point (that one is [sc_check_text()]).
#'
#' `result` is a list with `test_type`, `statistic`, `df1`, `df2`,
#' `p_operator` (one of `"="`, `"<"`, `">"`), `p_value`, and optionally
#' `one_tailed`. A field the caller leaves out is treated as absent, the same
#' as `NA`.
#'
#' `reported_p_text` is the p-value exactly as written, for example `".03"`.
#' It is used to learn how many decimals were reported, so the comparison
#' allows for the rounding the author applied: a reported `.03` stands for
#' any value that rounds to `.03` at the same number of decimals. When it is
#' not given, the decimals are read from `result$p_value` instead.
#'
#' @param result A list describing one reported result.
#' @param alpha The significance threshold used to judge a decision error.
#' @param p_equal_alpha_sig Whether a p-value exactly at `alpha` counts as
#'   significant.
#' @param reported_p_text The p-value as written in the source text.
#' @return A list with `verdict` (one of `"consistent"`, `"inconsistent"`,
#'   `"decision_error"`, `"undecidable"`), `computed_p`, `reported_p`,
#'   `reason`, and `missing` (the parts the result did not carry).
#' @examples
#' sc_verdict(list(test_type = "f", statistic = 4.11, df1 = 2, df2 = 87,
#'                 p_operator = "=", p_value = 0.03))
#' @export
sc_verdict <- function(result, alpha = 0.05, p_equal_alpha_sig = TRUE,
                       reported_p_text = NULL) {
  # A list may omit a field instead of setting it to NA; both mean absent.
  or_na <- function(x) if (is.null(x)) NA_real_ else x
  computed <- sc_compute_p(result$test_type, or_na(result$statistic), or_na(result$df1),
                           or_na(result$df2), isTRUE(result$one_tailed))
  absent <- missing_parts(result)
  p_value <- if (is.null(result$p_value)) NA_real_ else result$p_value
  p_operator <- if (is.null(result$p_operator)) NA_character_ else result$p_operator

  if (is.na(computed)) {
    reason <- if (length(absent)) describe_missing(absent) else
      "the statistic and its degrees of freedom give no p-value"
    return(list(verdict = UNDECIDABLE, computed_p = NA_real_, reported_p = p_value,
                reason = reason, missing = absent))
  }
  if (is.na(p_value) || !(p_operator %in% c("=", "<", ">"))) {
    reason <- describe_missing(absent)
    if (!nzchar(reason)) reason <- "there is no reported p-value to compare against"
    return(list(verdict = UNDECIDABLE, computed_p = computed, reported_p = p_value,
                reason = reason, missing = absent))
  }

  reported <- p_value
  op <- p_operator

  if (op == "=") {
    nd <- p_decimals(if (!is.null(reported_p_text)) reported_p_text else reported)
    tol <- if (nd > 0) 0.5 * 10 ^ (-nd) else 0.5
    agrees <- abs(computed - reported) <= tol
  } else if (op == "<") {
    agrees <- computed < reported
  } else {
    agrees <- computed > reported
  }

  if (agrees) {
    return(list(verdict = CONSISTENT, computed_p = computed, reported_p = reported,
                reason = "", missing = character(0)))
  }

  # The values disagree. A disagreement that also flips the conclusion is
  # reported separately, because it changes what the paper claims.
  reported_sig <- if (op == "=") {
    sc_is_significant(reported, alpha, p_equal_alpha_sig)
  } else if (op == "<") {
    reported <= alpha
  } else {
    FALSE
  }
  computed_sig <- sc_is_significant(computed, alpha, p_equal_alpha_sig)

  if (reported_sig != computed_sig) {
    return(list(verdict = DECISION_ERROR, computed_p = computed, reported_p = reported,
                reason = "the reported and computed p-values disagree about significance",
                missing = character(0)))
  }
  list(verdict = INCONSISTENT, computed_p = computed, reported_p = reported,
      reason = "the reported and computed p-values disagree", missing = character(0))
}

#' Parse a number written in the text of a paper
#'
#' Strips a leading comparison operator (`<`, `>`, `=`), restores a missing
#' leading zero (`".03"` becomes `"0.03"`), and accepts the Unicode minus and
#' en dash this corpus shows in place of a hyphen.
#'
#' @param text The text to parse, such as `"<.001"` or a Unicode minus sign
#'   in place of a hyphen, `"-2.87"`.
#' @return A number, or `NA_real_` when `text` is empty or is not a number.
#' @examples
#' sc_parse_number("<.001")
#' sc_parse_number(".03")
#' @export
sc_parse_number <- function(text) {
  if (is.null(text) || (length(text) == 1 && is.na(text)) || identical(text, "")) {
    return(NA_real_)
  }
  s <- trimws(as.character(text))
  s <- gsub("\u2212", "-", s, fixed = TRUE)
  s <- gsub("\u2013", "-", s, fixed = TRUE)
  s <- trimws(sub("^[<>=]+", "", s))
  if (startsWith(s, ".")) {
    s <- paste0("0", s)
  } else if (startsWith(s, "-.")) {
    s <- paste0("-0", substring(s, 2))
  }
  val <- suppressWarnings(as.numeric(s))
  if (is.na(val)) NA_real_ else val
}
