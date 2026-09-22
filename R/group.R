# Turn a tagged character sequence into the results it describes.
#
# Two steps, ported from two different places in the mother repository:
# `labels.tags_to_spans` reads spans back out of a BIOES tag sequence, and
# `evalutil.group_spans` + `evalutil.build_result` group those spans into one
# result per test. `sc_check_text()` uses the same grouping decision for its
# own model stage, with a different number parser -- see `pipeline.R`.

OUTSIDE <- "O"

# The operator is not a separate model output: it is part of the tag itself,
# because a font missing a ToUnicode map often leaves nothing at the
# operator's position for a separate tag to mark.
ENTITY_OPERATOR <- list(POP_EQ = "=", POP_LT = "<", POP_GT = ">")

#' Read spans back out of a BIOES tag sequence
#'
#' The decoding is forgiving: a model can emit an I or E tag with no B
#' before it, and dropping such a span would hide a near miss during
#' evaluation, so a stray continuation opens a span instead.
#'
#' @param tags A character vector of BIOES tags, such as `"B-TEST"`,
#'   `"I-TEST"`, `"O"`.
#' @return A list of `(start, end, entity)` spans, 0-based and half-open, in
#'   the order they close.
#' @examples
#' sc_tags_to_spans(c("S-TEST", "O", "B-DF1", "E-DF1"))
#' @export
sc_tags_to_spans <- function(tags) {
  all_tags <- c(tags, OUTSIDE)
  spans <- list()
  start <- NA_integer_
  entity <- NA_character_

  for (i0 in seq_along(all_tags) - 1L) {
    tag <- all_tags[i0 + 1L]
    if (identical(tag, OUTSIDE)) {
      prefix <- OUTSIDE
      ent <- NA_character_
    } else {
      prefix <- substr(tag, 1, 1)
      ent <- substr(tag, 3, nchar(tag, type = "chars"))
    }

    if (prefix %in% c("B", "S") || (prefix %in% c("I", "E") && !identical(entity, ent))) {
      if (!is.na(start)) {
        spans[[length(spans) + 1L]] <- list(start = start, end = i0, entity = entity)
      }
      start <- i0
      entity <- ent
      if (identical(prefix, "S")) {
        spans[[length(spans) + 1L]] <- list(start = start, end = i0 + 1L, entity = entity)
        start <- NA_integer_
        entity <- NA_character_
      }
    } else if (identical(prefix, "E") && identical(entity, ent)) {
      spans[[length(spans) + 1L]] <- list(start = start, end = i0 + 1L, entity = entity)
      start <- NA_integer_
      entity <- NA_character_
    } else if (identical(prefix, OUTSIDE)) {
      if (!is.na(start)) {
        spans[[length(spans) + 1L]] <- list(start = start, end = i0, entity = entity)
      }
      start <- NA_integer_
      entity <- NA_character_
    }
  }

  Filter(function(s) s$end > s$start, spans)
}

# Group spans into one result per test. A new result starts at a TEST tag,
# or at a second STAT tag with no TEST between them, matching the boundary
# the annotators used. Returns a list of `(parts, spans)` pairs: `parts` is
# a named list of the raw text captured for each entity, in the order the
# entities were first seen.
sc_group_spans <- function(text, spans) {
  if (length(spans) == 0) return(list())
  starts <- vapply(spans, `[[`, integer(1), "start")
  ends <- vapply(spans, `[[`, integer(1), "end")
  entities <- vapply(spans, `[[`, character(1), "entity")
  spans <- spans[order(starts, ends, entities)]

  results <- list()
  current <- NULL
  current_spans <- list()

  for (sp in spans) {
    label <- sp$entity
    if (identical(label, "TEST") ||
        (identical(label, "STAT") && !is.null(current) && !is.null(current[["STAT"]]))) {
      if (!is.null(current)) {
        results[[length(results) + 1L]] <- list(parts = current, spans = current_spans)
      }
      current <- list()
      current_spans <- list()
    }
    if (is.null(current)) {
      current <- list()
      current_spans <- list()
    }
    if (is.null(current[[label]])) current[[label]] <- substr(text, sp$start + 1L, sp$end)
    current_spans[[length(current_spans) + 1L]] <- c(sp$start, sp$end)
  }
  if (!is.null(current)) {
    results[[length(results) + 1L]] <- list(parts = current, spans = current_spans)
  }
  results
}

# `align.as_number`: parse a number the way `evalutil.build_result` does,
# tolerant of a thousands comma and of a control character standing in for a
# damaged minus sign or decimal point. Rounds to 4 decimals, which
# `sc_parse_number()` (used everywhere else a raw capture becomes a number)
# does not.
group_parse_clean <- function(s) {
  minus_chars <- c("-", "\u2212", "\u2013")
  is_negative <- FALSE
  if (nchar(s, type = "chars") > 0 && substr(s, 1, 1) %in% minus_chars) {
    is_negative <- TRUE
    s <- trimws(substring(s, 2))
  }
  # A thousands separator sits between a digit and exactly three digits not
  # followed by a fourth: "1,234.5" is 1234.5, "2,45" is left alone, because
  # this corpus writes decimals with a period.
  s <- gsub("(?<=\\d),(?=\\d{3}(?:\\D|$))", "", s, perl = TRUE)
  if (!nzchar(s)) return(NA_real_)
  val <- suppressWarnings(as.numeric(s))
  if (is.na(val)) return(NA_real_)
  if (is_negative) -val else val
}

sc_as_number <- function(text) {
  if (is.null(text) || (length(text) == 1 && is.na(text)) || identical(text, "")) {
    return(NA_real_)
  }
  s <- trimws(as.character(text))
  if (!nzchar(s)) return(NA_real_)

  val <- group_parse_clean(s)
  if (!is.na(val)) return(round(val, 4))

  # A leading or trailing control character is a common stand-in for a
  # damaged sign or decimal point. Strip one from each end and retry; the
  # sign such a character carried is not recoverable, so the retry always
  # returns the positive magnitude.
  n <- nchar(s, type = "chars")
  control <- "[\\x00-\\x1f]"
  trimmed <- paste0(
    gsub(control, "", substr(s, 1, 1), perl = TRUE),
    if (n > 2) substr(s, 2, n - 1L) else "",
    gsub(control, "", substr(s, n, n), perl = TRUE)
  )
  trimmed <- trimws(trimmed)
  if (nzchar(trimmed) && !identical(trimmed, s)) {
    val <- group_parse_clean(trimmed)
    if (!is.na(val)) return(round(abs(val), 4))
  }
  NA_real_
}

# Build one result from a group's parts, with `number_fn` doing the parsing.
# `sc_group()` uses `sc_as_number`, matching `evalutil.build_result`.
# `sc_check_text()`'s own model stage uses `sc_parse_number` instead,
# because that is what `Pipeline.find_with_model` calls in the mother
# repository -- the grouping decision above is the only part the two share.
group_result_from_parts <- function(parts, number_fn) {
  stat <- number_fn(parts[["STAT"]])
  if (is.na(stat)) return(NULL)
  pop_key <- Find(function(k) startsWith(k, "POP_"), names(parts))
  operator <- if (is.null(pop_key)) NA_character_ else ENTITY_OPERATOR[[pop_key]]
  test_type <- tolower(trimws(if (is.null(parts[["TEST"]])) "" else parts[["TEST"]]))
  # A span with no test name stays nameless: assuming t turned correct
  # chi-squares into reported errors.
  if (!nzchar(test_type)) test_type <- NA_character_
  list(test_type = test_type, statistic = stat,
      df1 = number_fn(parts[["DF1"]]), df2 = number_fn(parts[["DF2"]]),
      p_operator = operator, p_value = number_fn(parts[["PVAL"]]))
}

#' Group tagged spans into reported results
#'
#' Ports `evalutil.group_spans` and `evalutil.build_result` together: reads
#' spans out of `tags`, groups them into one result per test, and parses
#' each part's raw text into a number.
#'
#' @param text The text `tags` was produced over -- the same text a
#'   [sc_tag()] case stores, one tag per character.
#' @param tags A character vector of BIOES tags, as long as `text` has
#'   characters.
#' @return A data frame with one row per result found: `test_type`,
#'   `statistic`, `df1`, `df2`, `p_operator`, `p_value`, `start`, `end`
#'   (0-based, half-open). Zero rows when no result groups.
#' @examples
#' kit <- sc_kit()
#' tags <- sc_tag("F(2, 30) = 4.11, p = .03", kit)[[1]]
#' sc_group("F(2, 30) = 4.11, p = .03", tags)
#' @export
sc_group <- function(text, tags) {
  spans <- sc_tags_to_spans(tags)
  groups <- sc_group_spans(text, spans)

  rows <- list()
  for (g in groups) {
    res <- group_result_from_parts(g$parts, sc_as_number)
    if (is.null(res)) next
    starts <- vapply(g$spans, `[`, integer(1), 1)
    ends <- vapply(g$spans, `[`, integer(1), 2)
    res$start <- min(starts)
    res$end <- max(ends)
    rows[[length(rows) + 1L]] <- res
  }

  if (length(rows) == 0) {
    return(data.frame(test_type = character(0), statistic = numeric(0),
                      df1 = numeric(0), df2 = numeric(0), p_operator = character(0),
                      p_value = numeric(0), start = integer(0), end = integer(0),
                      stringsAsFactors = FALSE))
  }

  data.frame(
    test_type = vapply(rows, `[[`, character(1), "test_type"),
    statistic = vapply(rows, `[[`, numeric(1), "statistic"),
    df1 = vapply(rows, `[[`, numeric(1), "df1"),
    df2 = vapply(rows, `[[`, numeric(1), "df2"),
    p_operator = vapply(rows, `[[`, character(1), "p_operator"),
    p_value = vapply(rows, `[[`, numeric(1), "p_value"),
    start = vapply(rows, `[[`, integer(1), "start"),
    end = vapply(rows, `[[`, integer(1), "end"),
    stringsAsFactors = FALSE
  )
}
