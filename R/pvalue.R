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

#' The p-values a statistic could imply, given how it was rounded
#'
#' A paper writes `t(67) = 1.48`. The true statistic is anywhere in
#' `[1.475, 1.485]`, and each end implies a different p-value. statcheck
#' compares the reported p against that whole interval (`error_test` in
#' statcheck 1.5.0), and this port must do the same or it calls a correctly
#' reported result an error.
#'
#' @param result A list describing one reported result, as in [sc_verdict()].
#' @param statistic_text The statistic exactly as printed, such as `"1.48"`.
#'   When it is not given, the decimals are read from `result$statistic`
#'   itself, formatted with `format(x, scientific = FALSE)` -- `2.45` gives
#'   2 decimals and `5.1` gives 1, which is right whenever the number was
#'   parsed from the text it was printed as. A whole number loses its
#'   trailing zero this way (`5` reads as 0 decimals, not the 1 a paper's own
#'   "5.0" would have printed), so a caller that has the printed text should
#'   always pass it.
#' @return A list with `low_p` and `up_p`, or both `NA_real_` when no p can
#'   be computed for either end of the interval.
#' @examples
#' sc_rounding_interval(list(test_type = "t", statistic = 1.48, df1 = 67),
#'                      statistic_text = "1.48")
#' @export
sc_rounding_interval <- function(result, statistic_text = NULL) {
  statistic <- result$statistic
  if (is.null(statistic) || is.na(statistic)) return(list(low_p = NA_real_, up_p = NA_real_))

  text <- if (!is.null(statistic_text)) statistic_text else format(statistic, scientific = FALSE)
  decimals <- p_decimals(text)
  half <- 0.5 / (10 ^ decimals)

  or_na <- function(x) if (is.null(x)) NA_real_ else x
  df1 <- or_na(result$df1)
  df2 <- or_na(result$df2)
  one_tailed <- isTRUE(result$one_tailed)

  # The end nearer zero implies the larger p-value, so a negative statistic
  # swaps which end is which.
  if (statistic >= 0) {
    near <- statistic - half
    far <- statistic + half
  } else {
    near <- statistic + half
    far <- statistic - half
  }
  up_p <- sc_compute_p(result$test_type, near, df1, df2, one_tailed)
  low_p <- sc_compute_p(result$test_type, far, df1, df2, one_tailed)
  if (is.na(up_p) || is.na(low_p)) return(list(low_p = NA_real_, up_p = NA_real_))
  list(low_p = low_p, up_p = up_p)
}

#' Compare a reported p-value with the one its statistic implies
#'
#' NAME NOTE: this is the arithmetic check, not the pipeline's PDF entry
#' point (that one is [sc_check_text()]).
#'
#' `result` is a list with `test_type`, `statistic`, `df1`, `df2`,
#' `p_operator` (one of `"="`, `"<"`, `">"`, or `"ns"`), `p_value`, and
#' optionally `one_tailed`. A field the caller leaves out is treated as
#' absent, the same as `NA`.
#'
#' The rule is statcheck's own (`error_test` and `decision_error_test` in
#' statcheck 1.5.0), because statcheck is the baseline this project is
#' measured against and its convention is what a reader expects. Both
#' numbers in a paper are rounded, and the comparison allows for both:
#' `reported_p_text` gives the decimals of the p-value, `statistic_text` the
#' decimals of the statistic (see [sc_rounding_interval()]). Without them
#' the decimals are read from the numbers themselves, which is right
#' whenever they were parsed from the text they were printed as.
#'
#' `"ns"` is a claim about alpha, not a number: the paper says the result was
#' not significant. statcheck reads it as `p > alpha`, and so does this. A
#' reported p at or below zero is always an error, whatever the computed
#' value is, because no test gives exactly zero.
#'
#' An inconsistency is not the same as a wrong conclusion. The verdict is
#' `"decision_error"` when the reported and the computed p-value fall on
#' opposite sides of `alpha` -- decided on the computed value itself, not on
#' the rounding interval, because the interval says whether the two numbers
#' can agree and alpha says what the paper concluded -- and `"inconsistent"`
#' when they disagree without changing what the paper claims.
#'
#' @param result A list describing one reported result.
#' @param alpha The significance threshold used to judge a decision error.
#' @param p_equal_alpha_sig Whether a p-value exactly at `alpha` counts as
#'   significant.
#' @param reported_p_text The p-value as written in the source text.
#' @param statistic_text The test statistic as written in the source text.
#' @param p_zero_error Whether a reported p-value at or below zero is always
#'   an error. statcheck always treats it this way; the flag exists so a
#'   caller can turn the rule off for a study of its own.
#' @return A list with `verdict` (one of `"consistent"`, `"inconsistent"`,
#'   `"decision_error"`, `"undecidable"`), `computed_p`, `reported_p`,
#'   `reason`, and `missing` (the parts the result did not carry).
#' @examples
#' sc_verdict(list(test_type = "f", statistic = 4.11, df1 = 2, df2 = 87,
#'                 p_operator = "=", p_value = 0.03))
#' @export
sc_verdict <- function(result, alpha = 0.05, p_equal_alpha_sig = TRUE,
                       reported_p_text = NULL, statistic_text = NULL,
                       p_zero_error = TRUE) {
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

  if (identical(p_operator, "ns")) {
    reported <- alpha
    op <- ">"
  } else if (is.na(p_value) || !(p_operator %in% c("=", "<", ">"))) {
    reason <- describe_missing(absent)
    if (!nzchar(reason)) reason <- "there is no reported p-value to compare against"
    return(list(verdict = UNDECIDABLE, computed_p = computed, reported_p = p_value,
                reason = reason, missing = absent))
  } else {
    reported <- p_value
    op <- p_operator
  }

  interval <- sc_rounding_interval(result, statistic_text)
  low_p <- interval$low_p
  up_p <- interval$up_p
  if (is.na(low_p)) {
    low_p <- computed
    up_p <- computed
  }

  if (p_zero_error && reported <= 0) {
    # No test gives a p-value of exactly zero, so the paper reports a number
    # that cannot be right, however small the computed value is.
    error <- TRUE
  } else if (op == "=") {
    nd <- p_decimals(if (!is.null(reported_p_text)) reported_p_text else reported)
    error <- reported > round(up_p, nd) || reported < round(low_p, nd)
  } else if (op == "<") {
    error <- reported < low_p
  } else {
    error <- reported > up_p
  }

  if (!error) {
    return(list(verdict = CONSISTENT, computed_p = computed, reported_p = reported,
                reason = "", missing = character(0)))
  }

  # The values disagree. statcheck decides significance on the computed
  # value itself, not on the interval, so a disagreement that also flips the
  # conclusion is reported separately, because it changes what the paper
  # claims.
  computed_sig <- sc_is_significant(computed, alpha, p_equal_alpha_sig)
  if (op == "=") {
    reported_sig <- sc_is_significant(reported, alpha, p_equal_alpha_sig)
    decision_error <- reported_sig != computed_sig
  } else if (op == "<") {
    decision_error <- reported <= alpha && !computed_sig
  } else {
    decision_error <- reported >= alpha && computed_sig
  }

  if (decision_error) {
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
