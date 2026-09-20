# The pattern-based extractor. This is the baseline, not the product.
#
# It reproduces what the R package statcheck finds: a test statistic
# reported in APA style, with its degrees of freedom and its p-value. It
# keeps every fault of the Python port it mirrors, because improvement is
# measured against this baseline and the baseline must stay faithful rather
# than become better.

# A number, with an optional sign and an optional leading decimal point.
NUM <- "-?\\d*\\.?\\d+"

# One pattern for each family of test, keyed by the name that becomes
# `test_type`. Spacing is permissive, because typesetting varies between
# publishers.
extract_patterns <- function() {
  list(
    t = sprintf(
      "\\bt\\s*\\(\\s*(?<df1>%s)\\s*\\)\\s*(?<sop>[=<>])\\s*(?<stat>%s)\\s*,\\s*p\\s*(?<pop>[=<>])\\s*(?<p>%s)",
      NUM, NUM, NUM),
    f = sprintf(
      "\\bF\\s*\\(\\s*(?<df1>%s)\\s*,\\s*(?<df2>%s)\\s*\\)\\s*(?<sop>[=<>])\\s*(?<stat>%s)\\s*,\\s*p\\s*(?<pop>[=<>])\\s*(?<p>%s)",
      NUM, NUM, NUM, NUM),
    r = sprintf(
      "\\br\\s*\\(\\s*(?<df1>%s)\\s*\\)\\s*(?<sop>[=<>])\\s*(?<stat>%s)\\s*,\\s*p\\s*(?<pop>[=<>])\\s*(?<p>%s)",
      NUM, NUM, NUM),
    z = sprintf(
      "\\bz\\s*(?<sop>[=<>])\\s*(?<stat>%s)\\s*,\\s*p\\s*(?<pop>[=<>])\\s*(?<p>%s)",
      NUM, NUM),
    chi2 = sprintf(
      "\\b(?:\u03c7\\s*2|\u03c72|chi2|X2|c2)\\s*\\(\\s*(?<df1>%s)(?:\\s*,\\s*N\\s*[=<>]\\s*(?<n>[\\d,]+))?\\s*\\)\\s*(?<sop>[=<>])\\s*(?<stat>%s)\\s*,\\s*p\\s*(?<pop>[=<>])\\s*(?<p>%s)",
      NUM, NUM, NUM),
    q = sprintf(
      "\\bQ(?:w|b)?\\s*\\(\\s*(?<df1>%s)\\s*\\)\\s*(?<sop>[=<>])\\s*(?<stat>%s)\\s*,\\s*p\\s*(?<pop>[=<>])\\s*(?<p>%s)",
      NUM, NUM, NUM)
  )
}

# One capture group's text at one match, or NA when the group did not
# participate in the match (an optional group, or a group the pattern for
# this test type never defines).
capture_value <- function(text, name, starts, lengths, group_names) {
  j <- match(name, group_names)
  if (is.na(j) || starts[j] <= 0) return(NA_character_)
  substr(text, starts[j], starts[j] + lengths[j] - 1)
}

extract_matches <- function(text, test_type, pattern) {
  m <- gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE, useBytes = FALSE)[[1]]
  if (m[1] == -1) return(list())

  group_names <- attr(m, "capture.names")
  cap_starts <- attr(m, "capture.start")
  cap_lengths <- attr(m, "capture.length")
  match_lengths <- attr(m, "match.length")

  lapply(seq_along(m), function(i) {
    starts <- cap_starts[i, ]
    lengths <- cap_lengths[i, ]
    list(
      test_type = test_type,
      statistic = capture_value(text, "stat", starts, lengths, group_names),
      df1 = capture_value(text, "df1", starts, lengths, group_names),
      df2 = capture_value(text, "df2", starts, lengths, group_names),
      p_operator = capture_value(text, "pop", starts, lengths, group_names),
      p_value = capture_value(text, "p", starts, lengths, group_names),
      start = m[i],
      end = m[i] + match_lengths[i]
    )
  })
}

# A z pattern can also match inside a longer result. Drop any extraction
# that sits wholly inside another one.
drop_contained <- function(found) {
  starts <- vapply(found, `[[`, numeric(1), "start")
  ends <- vapply(found, `[[`, numeric(1), "end")
  keep <- vapply(seq_along(found), function(i) {
    !any(starts[-i] <= starts[i] & ends[i] <= ends[-i])
  }, logical(1))
  found[keep]
}

empty_extraction <- function() {
  data.frame(test_type = character(0), statistic = character(0), df1 = character(0),
             df2 = character(0), p_operator = character(0), p_value = character(0),
             stringsAsFactors = FALSE)
}

#' Find every reported test result a pattern can read
#'
#' Reproduces what the R package statcheck finds: a test statistic reported
#' in APA style, with its degrees of freedom and its p-value. This is the
#' pattern-based baseline, not the trained model, and it keeps the faults of
#' the version it mirrors so that improvement can be measured against it.
#'
#' A result whose degrees of freedom or p-value the text does not report,
#' such as a z test's `df1`, comes back as `NA` in that column. `df1`,
#' `df2`, and `p_value` hold the raw digits as they were written, so a
#' leading zero or its absence survives (`.03` next to `0.010` is expected).
#'
#' @param text A window of text, already normalised with [sc_normalize()].
#' @param kit A loaded [sc_kit()]. Reserved for a future rule that reads the
#'   kit; the pattern itself is fixed and ships with the package.
#' @return A data frame with one row per result found, ordered by where it
#'   starts in `text`, and the columns `test_type`, `statistic`, `df1`,
#'   `df2`, `p_operator`, `p_value`, all character. Zero rows when nothing
#'   matches.
#' @examples
#' kit <- sc_kit()
#' sc_extract("The main effect was significant, F(2, 87) = 4.11, p = .03.", kit)
#' @export
sc_extract <- function(text, kit) {
  patterns <- extract_patterns()
  found <- do.call(c, lapply(names(patterns), function(name) {
    extract_matches(text, name, patterns[[name]])
  }))
  if (length(found) == 0) return(empty_extraction())

  ord <- order(vapply(found, `[[`, numeric(1), "start"),
               vapply(found, `[[`, numeric(1), "end"))
  found <- found[ord]
  kept <- drop_contained(found)
  if (length(kept) == 0) return(empty_extraction())

  data.frame(
    test_type = vapply(kept, `[[`, character(1), "test_type"),
    statistic = vapply(kept, `[[`, character(1), "statistic"),
    df1 = vapply(kept, `[[`, character(1), "df1"),
    df2 = vapply(kept, `[[`, character(1), "df2"),
    p_operator = vapply(kept, `[[`, character(1), "p_operator"),
    p_value = vapply(kept, `[[`, character(1), "p_value"),
    stringsAsFactors = FALSE
  )
}

# Reading a PDF is its own concern, kept here because it was ported from the
# same version 1 file. R has no maintained PDF reader besides pdftools,
# which bundles poppler; pdftools hangs on rare files, so each document gets
# a time limit and returns "" rather than stopping the caller.

has_timeout <- function() requireNamespace("R.utils", quietly = TRUE)

#' Read one PDF and return normalised text
#'
#' R has no maintained PDF reader besides pdftools, which bundles poppler.
#' Poppler returns a whole column as one line, which is why every result of
#' [sc_read_pdf()] passes through [sc_normalize()] before it comes back.
#'
#' pdftools hangs on rare files. When R.utils is installed, a document that
#' does not finish within `timeout` seconds returns `""` instead of
#' stopping the caller; without R.utils no limit applies, and this is
#' reported once with a warning.
#'
#' @param path A PDF file.
#' @param kit A loaded [sc_kit()].
#' @param timeout Seconds to allow one document. Ignored when R.utils is
#'   not installed.
#' @return Normalised text, or `""` when the document could not be read.
#' @examples
#' \dontrun{
#' kit <- sc_kit()
#' sc_read_pdf("article.pdf", kit)
#' }
#' @export
sc_read_pdf <- function(path, kit, timeout = 60) {
  read_it <- function() paste(pdftools::pdf_text(path), collapse = "")

  text <- tryCatch({
    if (has_timeout()) {
      R.utils::withTimeout(read_it(), timeout = timeout, onTimeout = "silent")
    } else {
      if (!isTRUE(getOption("statcheckml.timeout.warned"))) {
        warning("R.utils is not installed, so no time limit applies to pdftools. ",
                "Install R.utils to stop one document halting a run.", call. = FALSE)
        options(statcheckml.timeout.warned = TRUE)
      }
      read_it()
    }
  }, error = function(e) "")

  if (is.null(text) || length(text) == 0 || is.na(text[1])) return("")
  sc_normalize(text, kit)
}
