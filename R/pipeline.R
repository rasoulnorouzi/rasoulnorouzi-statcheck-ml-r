# The whole pipeline: a document goes in, checked results come out.
#
# Five stages, in this order: normalise, repair, prefilter, find (pattern
# first, then model for what the pattern did not find), check. Every stage
# is its own file; this one only calls them in order and reduces what they
# return to one row per result, the way `Pipeline.run_text` does in the
# mother repository.

# The Unicode "Zs" (space separator) category, hardcoded because it has been
# stable for well over a decade and R has no built-in category lookup.
# Mirrors `statcheck_ml.data.normalise`: every character in this set becomes
# a plain space before the model reads it, and everything else -- including
# a control character that may be a damaged operator -- is left exactly as
# it is. This is not `sc_normalize()` again: that stage makes a line's width
# and its operator characters engine-independent; this one only folds the
# handful of Unicode spaces a font can produce into the one space the model
# was trained on.
MODEL_SPACE_POINTS <- c(0x20L, 0xa0L, 0x1680L, 0x2000L:0x200aL, 0x202fL, 0x205fL, 0x3000L)

sc_model_normalise <- function(text) {
  points <- utf8ToInt(enc2utf8(text))
  if (length(points) == 0) return(text)
  points[points %in% MODEL_SPACE_POINTS] <- 0x20L
  intToUtf8(points, multiple = FALSE)
}

# `Pipeline.find_with_model`'s own inline grouping: the same decision as
# `sc_group_spans()`, but parsed with `sc_parse_number` rather than
# `sc_as_number`, because that is what the mother repository's pipeline
# calls at this stage. Returns a list of result lists, `df1`/`df2`/`p_value`
# included even when `NA`.
#
# `g$parts` still holds each entity's raw captured text at this point, so
# `statistic_text` and `p_value_text` are carried alongside the parsed
# numbers: the p-value check ([sc_verdict()]) needs the text as printed, not
# the number `sc_parse_number()` made of it, to know how many decimals the
# paper rounded to.
# A number the PDF conversion split with spaces, such as ". 05" or "2 .37":
# digits and points only, with whitespace between them. The model marks such a
# span as one value, so it is read as one (`split_number_rule` in the kit's
# parity cases). The pattern never matches one.
join_split_number <- function(text) {
  if (is.null(text) || is.na(text)) return(text)
  trimmed <- trimws(text)
  if (grepl("^-?[0-9.]+(\\s+[0-9.]+)+$", trimmed, perl = TRUE)) gsub("\\s+", "", trimmed, perl = TRUE) else text
}

pipeline_group_model <- function(text, tags) {
  spans <- sc_tags_to_spans(tags)
  groups <- sc_group_spans(text, spans)
  rows <- list()
  for (g in groups) {
    g$parts <- lapply(g$parts, join_split_number)
    res <- group_result_from_parts(g$parts, sc_parse_number)
    if (!is.null(res)) {
      res$statistic_text <- g$parts[["STAT"]]
      res$p_value_text <- g$parts[["PVAL"]]
      res$statistic_span <- g$stat_span
      rows[[length(rows) + 1L]] <- res
    }
  }
  rows
}

# Counts the newlines of the unit text before the statistic's relative
# start; the unit text is the blanked lines joined by "\n", so this is the
# same line index as in the repaired document.
statistic_line <- function(unit_text, unit_first_line, relative_start) {
  before <- substr(unit_text, 1L, relative_start)
  unit_first_line + lengths(regmatches(before, gregexpr("\n", before, fixed = TRUE)))
}

empty_check_result <- function() {
  data.frame(source = character(0), test_type = character(0), statistic = numeric(0),
            df1 = numeric(0), df2 = numeric(0), p_operator = character(0),
            p_value = numeric(0), computed_p = numeric(0), verdict = character(0),
            line = integer(0), reason = character(0),
            statistic_start = integer(0), statistic_end = integer(0),
            stringsAsFactors = FALSE)
}

empty_fragments <- function() {
  data.frame(source = character(0), test_type = character(0), statistic = numeric(0),
            df1 = numeric(0), df2 = numeric(0), p_operator = character(0),
            p_value = numeric(0), line = integer(0),
            statistic_start = integer(0), statistic_end = integer(0),
            stringsAsFactors = FALSE)
}

# A fragment is a find with no test name. It has the columns of a result
# except the three the check fills in, and it never reaches `sc_verdict()`.
fragment_frame <- function(fragments) {
  if (length(fragments) == 0) return(empty_fragments())
  rows <- lapply(fragments, function(f) {
    data.frame(source = f$source, test_type = NA_character_, statistic = f$statistic,
              df1 = f$df1, df2 = f$df2, p_operator = f$p_operator, p_value = f$p_value,
              line = f$line, statistic_start = f$statistic_span[1],
              statistic_end = f$statistic_span[2], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

#' Read a document and check every statistical result in it
#'
#' Runs the whole pipeline: [sc_normalize()], [sc_repair()], [sc_units()],
#' then for each unit, the pattern extractor ([sc_extract()]) followed
#' by the trained model ([sc_tag()] plus [sc_group()]'s grouping) for
#' whatever the pattern did not already find, and finally [sc_verdict()] on
#' every result.
#'
#' A unit is a passage under spec version 2: overlapping windows merge, so
#' the model reads each character once.
#'
#' Deduplication. Every find carries the interval `[start, end)` of its
#' statistic value, counted in characters over the repaired document with
#' reference lines blanked. Units are visited in document order, the pattern
#' before the model in each unit. A find whose interval overlaps one already
#' emitted is a duplicate and the first stays. Two results with the same
#' value at different places are both kept. The mother repository defines
#' the rule in `Pipeline.run_text`. Its fallback for a find with no interval
#' is not ported, because both finders here always give one.
#'
#' Every unit is tagged in one [sc_tag()] call, because the model is the
#' slow stage and tagging a batch costs little more than tagging one text.
#'
#' @param text A document's text.
#' @param kit A loaded [sc_kit()].
#' @param model A model from [sc_load_model()]. Loading it costs about a
#'   second, so a caller checking many documents should load it once.
#' @return A data frame with one row per result that has a test name:
#'   `source` (`"pattern"` or `"model"`), `test_type`, `statistic`, `df1`,
#'   `df2`, `p_operator`, `p_value`, `computed_p`, `verdict`, `line`,
#'   `reason`, then `statistic_start` and `statistic_end` (the interval of
#'   the statistic in the document, 0-based, half-open; the reference's
#'   `statistic_span`). `line` follows the `line_rule` of the parity file in
#'   the kit. Zero rows when the document holds nothing checkable.
#'
#'   Two attributes are attached. `"stages"` holds the counts: `lines`,
#'   `windows_kept`, `units_kept`, `by_pattern`, `by_model` and
#'   `fragments`. `"fragments"` is a data frame of the finds with no test
#'   name, which are not checked and get no verdict. It has the columns of
#'   the result except `computed_p`, `verdict` and `reason`, in emission
#'   order. Deduplication runs over results and fragments together, so
#'   `by_pattern + by_model` equals `nrow(out) + nrow(fragments)`.
#' @examples
#' kit <- sc_kit()
#' sc_check_text("The effect was significant, t(28) = 2.87, p = .006.", kit)
#' @export
sc_check_text <- function(text, kit = sc_kit(), model = sc_load_model(kit)) {
  normalised <- sc_normalize(text, kit)
  repaired <- sc_repair(normalised, kit)
  windows <- sc_prefilter(repaired$text, kit)
  units <- sc_units(repaired$text, kit)

  if (nrow(units) == 0) {
    model_texts <- character(0)
    tagged <- list()
  } else {
    model_texts <- vapply(units$text, sc_model_normalise, character(1), USE.NAMES = FALSE)
    tagged <- sc_tag(model_texts, kit, model)
  }

  found <- list()
  taken <- matrix(integer(0), ncol = 2)  # statistic intervals already emitted
  n_pattern <- 0L
  n_model <- 0L

  is_duplicate <- function(span) {
    any(span[1] < taken[, 2] & taken[, 1] < span[2])
  }

  for (i in seq_len(nrow(units))) {
    shift <- units$char_start[i]

    pattern_hits <- sc_extract(units$text[i], kit)
    for (r in seq_len(nrow(pattern_hits))) {
      span <- c(pattern_hits$stat_start[r], pattern_hits$stat_end[r]) + shift
      if (is_duplicate(span)) next
      taken <- rbind(taken, span)
      # `sc_extract()` returns the raw captured text (character columns), so
      # the statistic and the p-value as they were printed are free to carry
      # alongside the parsed numbers, for the rounding rule in `sc_verdict()`.
      found[[length(found) + 1L]] <- list(
        source = "pattern", test_type = pattern_hits$test_type[r],
        statistic = sc_parse_number(pattern_hits$statistic[r]),
        statistic_text = pattern_hits$statistic[r],
        df1 = sc_parse_number(pattern_hits$df1[r]), df2 = sc_parse_number(pattern_hits$df2[r]),
        p_operator = pattern_hits$p_operator[r], p_value = sc_parse_number(pattern_hits$p_value[r]),
        p_value_text = pattern_hits$p_value[r],
        line = statistic_line(units$text[i], units$start_line[i], span[1] - shift),
        statistic_span = span)
      n_pattern <- n_pattern + 1L
    }

    for (mr in pipeline_group_model(model_texts[i], tagged[[i]])) {
      span <- mr$statistic_span + shift
      if (is_duplicate(span)) next
      taken <- rbind(taken, span)
      mr$statistic_span <- span
      found[[length(found) + 1L]] <- c(list(source = "model"), mr, list(line = statistic_line(units$text[i], units$start_line[i], span[1] - shift)))
      n_model <- n_model + 1L
    }
  }

  nameless <- vapply(found, function(f) is.na(f$test_type), logical(1))
  fragments <- fragment_frame(found[nameless])
  found <- found[!nameless]

  if (length(found) == 0) {
    out <- empty_check_result()
  } else {
    rows <- lapply(found, function(f) {
      # Prefer the text as printed (carried on `f` from the pattern's raw
      # captures or the model's raw span text); fall back to the parsed
      # value's own text only when no printed text survived, matching
      # `sc_verdict()`'s own fallback for a caller with no text at all.
      p_text <- if (!is.null(f$p_value_text)) f$p_value_text else
        (if (is.na(f$p_value)) NULL else as.character(f$p_value))
      v <- sc_verdict(f, reported_p_text = p_text, statistic_text = f$statistic_text)
      data.frame(source = f$source, test_type = f$test_type, statistic = f$statistic,
                df1 = f$df1, df2 = f$df2, p_operator = f$p_operator, p_value = f$p_value,
                computed_p = v$computed_p, verdict = v$verdict, line = f$line,
                reason = v$reason, statistic_start = f$statistic_span[1],
                statistic_end = f$statistic_span[2], stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    row.names(out) <- NULL
  }

  attr(out, "fragments") <- fragments
  attr(out, "stages") <- list(
    lines = length(py_split_lines(repaired$text)),
    windows_kept = nrow(windows),
    units_kept = nrow(units),
    by_pattern = n_pattern,
    by_model = n_model,
    fragments = nrow(fragments)
  )
  out
}

#' Read a PDF and check every statistical result in it
#'
#' Runs [sc_read_pdf()] then [sc_check_text()]: the whole pipeline from a
#' PDF file on disk to a table of checked results. Every result carries the
#' file it came from in `source_file`, so results from several documents can
#' be row-bound into one table without losing where each came from.
#'
#' @param path A PDF file.
#' @param kit A loaded [sc_kit()].
#' @param model A model from [sc_load_model()]. Loading it costs about a
#'   second, so a caller checking many documents should load it once and
#'   pass it to every call.
#' @param timeout Seconds to allow reading the PDF. Passed to
#'   [sc_read_pdf()]; ignored when R.utils is not installed.
#' @return The [sc_check_text()] result for the document's text, with
#'   `source_file` added as the first column, holding `path` in every row.
#'   Zero rows when the document holds nothing checkable or could not be
#'   read. The `"stages"` and `"fragments"` attributes are kept.
#' @examples
#' \dontrun{
#' kit <- sc_kit()
#' sc_check("article.pdf", kit)
#' }
#' @export
sc_check <- function(path, kit = sc_kit(), model = sc_load_model(kit), timeout = 60) {
  text <- sc_read_pdf(path, kit, timeout = timeout)
  out <- sc_check_text(text, kit, model)
  stages <- attr(out, "stages")
  fragments <- attr(out, "fragments")

  source_file <- if (nrow(out) == 0) character(0) else rep(path, nrow(out))
  out <- cbind(data.frame(source_file = source_file, stringsAsFactors = FALSE), out)

  attr(out, "stages") <- stages
  attr(out, "fragments") <- fragments
  out
}
