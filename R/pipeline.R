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
pipeline_group_model <- function(text, tags) {
  spans <- sc_tags_to_spans(tags)
  groups <- sc_group_spans(text, spans)
  rows <- list()
  for (g in groups) {
    res <- group_result_from_parts(g$parts, sc_parse_number)
    if (!is.null(res)) rows[[length(rows) + 1L]] <- res
  }
  rows
}

empty_check_result <- function() {
  data.frame(source = character(0), test_type = character(0), statistic = numeric(0),
            df1 = numeric(0), df2 = numeric(0), p_operator = character(0),
            p_value = numeric(0), computed_p = numeric(0), verdict = character(0),
            line = integer(0), reason = character(0), stringsAsFactors = FALSE)
}

#' Read a document and check every statistical result in it
#'
#' Runs the whole pipeline: [sc_normalize()], [sc_repair()], [sc_prefilter()],
#' then for each kept window, the pattern extractor ([sc_extract()]) followed
#' by the trained model ([sc_tag()] plus [sc_group()]'s grouping) for
#' whatever the pattern did not already find, and finally [sc_verdict()] on
#' every result. A result found by the pattern in one window and again by the
#' model in another is kept once, by its rounded statistic, the same rule the
#' mother repository's pipeline uses across the whole document.
#'
#' Every window is tagged in one [sc_tag()] call, because the model is the
#' slow stage and tagging a batch costs little more than tagging one text.
#'
#' @param text A document's text.
#' @param kit A loaded [sc_kit()].
#' @param model A model from [sc_load_model()]. Loading it costs about a
#'   second, so a caller checking many documents should load it once.
#' @return A data frame with one row per result: `source` (`"pattern"` or
#'   `"model"`), `test_type`, `statistic`, `df1`, `df2`, `p_operator`,
#'   `p_value`, `computed_p`, `verdict`, `line`, `reason`. Zero rows when the
#'   document holds nothing checkable. The stage counts (lines seen, windows
#'   kept, results by source) are attached as the `"stages"` attribute.
#' @examples
#' kit <- sc_kit()
#' sc_check_text("The effect was significant, t(28) = 2.87, p = .006.", kit)
#' @export
sc_check_text <- function(text, kit = sc_kit(), model = sc_load_model(kit)) {
  normalised <- sc_normalize(text, kit)
  repaired <- sc_repair(normalised, kit)
  windows <- sc_prefilter(repaired$text, kit)

  if (nrow(windows) == 0) {
    model_texts <- character(0)
    tagged <- list()
  } else {
    model_texts <- vapply(windows$text, sc_model_normalise, character(1), USE.NAMES = FALSE)
    tagged <- sc_tag(model_texts, kit, model)
  }

  found <- list()
  seen <- numeric(0)  # rounded statistics already emitted, across the whole document
  n_pattern <- 0L
  n_model <- 0L

  for (i in seq_len(nrow(windows))) {
    line_no <- windows$line[i]

    pattern_hits <- sc_extract(windows$text[i], kit)
    for (r in seq_len(nrow(pattern_hits))) {
      stat <- sc_parse_number(pattern_hits$statistic[r])
      key <- if (is.na(stat)) NA_real_ else round(stat, 3)
      if (key %in% seen) next
      seen <- c(seen, key)
      found[[length(found) + 1L]] <- list(
        source = "pattern", test_type = pattern_hits$test_type[r], statistic = stat,
        df1 = sc_parse_number(pattern_hits$df1[r]), df2 = sc_parse_number(pattern_hits$df2[r]),
        p_operator = pattern_hits$p_operator[r], p_value = sc_parse_number(pattern_hits$p_value[r]),
        line = line_no)
      n_pattern <- n_pattern + 1L
    }

    for (mr in pipeline_group_model(model_texts[i], tagged[[i]])) {
      key <- round(mr$statistic, 3)  # always non-NA: pipeline_group_model() drops the rest
      if (key %in% seen) next
      seen <- c(seen, key)
      found[[length(found) + 1L]] <- c(list(source = "model"), mr, list(line = line_no))
      n_model <- n_model + 1L
    }
  }

  if (length(found) == 0) {
    out <- empty_check_result()
  } else {
    rows <- lapply(found, function(f) {
      p_text <- if (is.na(f$p_value)) NULL else as.character(f$p_value)
      v <- sc_verdict(f, reported_p_text = p_text)
      data.frame(source = f$source, test_type = f$test_type, statistic = f$statistic,
                df1 = f$df1, df2 = f$df2, p_operator = f$p_operator, p_value = f$p_value,
                computed_p = v$computed_p, verdict = v$verdict, line = f$line,
                reason = v$reason, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    row.names(out) <- NULL
  }

  attr(out, "stages") <- list(
    lines = length(py_split_lines(repaired$text)),
    windows_kept = nrow(windows),
    by_pattern = n_pattern,
    by_model = n_model
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
#'   read. The `"stages"` attribute is kept.
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

  source_file <- if (nrow(out) == 0) character(0) else rep(path, nrow(out))
  out <- cbind(data.frame(source_file = source_file, stringsAsFactors = FALSE), out)

  attr(out, "stages") <- stages
  out
}
