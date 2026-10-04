# Select the passages of text that could hold a statistical result.
#
# The corpus holds 3.66 million lines, and only 0.1% of them contain a
# statistic. Tagging all of them wastes almost all of the work. The filter is
# tuned for recall alone: text it discards can never be recovered by the
# model, so the filter sets the recall ceiling of the whole system. It
# answers "could this hold a result", never "does this look like a result".
#
# The rules live in `spec/prefilter.json`, so that the Python, JavaScript and
# R ports read one definition instead of three copies.

prefilter_spec <- function(kit) {
  spec <- kit$spec$prefilter
  list(
    min_density = spec$min_non_letter_density,
    min_length = spec$min_line_length,
    context_lines = spec$context_lines,
    digit_pattern = spec$require_digit_pattern,
    ref_head_pattern = spec$reference_heading_pattern,
    ref_line_patterns = unlist(spec$reference_line_patterns, use.names = FALSE),
    # `max_reference_line` belongs to the normalisation spec: it exists only
    # to survive the line breaks a PDF engine chooses, and prefilter.json
    # does not restate it.
    max_reference_line = if (!is.null(spec$max_reference_line)) spec$max_reference_line
      else kit$spec$normalize$max_reference_line,
    # A window grows until it holds this much text, so a window means the
    # same amount of context whichever engine read the PDF. Zero turns the
    # rule off and restores a fixed count of lines.
    target_characters = if (!is.null(spec$target_window_characters))
      spec$target_window_characters else 0,
    max_context_lines = if (!is.null(spec$max_context_lines)) spec$max_context_lines else 8L,
    # "passage" merges overlapping windows into one unit; anything else, or
    # no key, keeps one unit per window.
    unit = if (!is.null(spec$unit)) spec$unit else "window"
  )
}

# `strsplit` drops the empty piece after a final separator and Python's
# `split` keeps it (the same difference `sc_normalize()`'s `reflow` works
# around). A prefilter window is indexed by line number, so this port needs
# the same line count Python gets from `text.split("\n")`.
py_split_lines <- function(text) {
  if (identical(text, "")) return("")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (grepl("\n$", text)) lines <- c(lines, "")
  lines
}

# The share of characters in `line` that are not letters. `\p{L}` is PCRE's
# Unicode letter class, the nearest match to Python's `str.isalpha()`.
prefilter_density <- function(line) {
  n <- nchar(line, type = "chars")
  if (n == 0) return(0)
  letters <- gregexpr("\\p{L}", line, perl = TRUE)[[1]]
  count <- if (letters[1] == -1) 0 else length(letters)
  1 - count / n
}

# A line longer than the cap is a column a PDF engine joined, and such a
# line can hold a result AND a citation. Judging it as one entry would
# discard the result along with the citation, so length rules it out first.
prefilter_is_reference <- function(line, spec) {
  if (nchar(line, type = "chars") > spec$max_reference_line) return(FALSE)
  any(vapply(spec$ref_line_patterns, function(p) grepl(p, line, perl = TRUE), logical(1)))
}

# Blank the reference section and stray reference lines. Lines are blanked
# rather than removed, which keeps every line number correct.
strip_references <- function(lines, spec) {
  n <- length(lines)
  cut_count <- n
  start_i <- floor(n * 0.55) + 1L
  if (start_i <= n) {
    for (i in start_i:n) {
      if (grepl(spec$ref_head_pattern, lines[i], perl = TRUE, ignore.case = TRUE)) {
        cut_count <- i - 1L
        break
      }
    }
  }
  out <- lines
  if (cut_count > 0) {
    keep <- seq_len(cut_count)
    is_ref <- vapply(lines[keep], prefilter_is_reference, logical(1), spec = spec)
    out[keep][is_ref] <- ""
  }
  if (cut_count < n) out[(cut_count + 1L):n] <- ""
  out
}

keeps_line <- function(line, spec) {
  if (nchar(line, type = "chars") < spec$min_length) return(FALSE)
  if (!grepl(spec$digit_pattern, line, perl = TRUE)) return(FALSE)
  prefilter_density(line) >= spec$min_density
}

# Choose how many lines of context one window needs. A fixed line count is
# not a fixed amount of context: it was tuned on PyMuPDF text, whose lines
# run about 57 characters, while a shorter-lined engine would cut a result
# off at the same line count. The window grows until it holds about as much
# text as the window the model trained on, and it never shrinks below the
# line count. `i`, `lo` and `hi` are 0-based, matching `text.split("\n")`.
prefilter_span <- function(lines, i, spec) {
  n_lines <- length(lines)
  n <- spec$context_lines
  lo <- max(0L, i - n)
  hi <- min(n_lines, i + n + 1L)
  if (spec$target_characters == 0) return(c(lo, hi))

  while (n < spec$max_context_lines) {
    span_chars <- sum(nchar(lines[(lo + 1L):hi], type = "chars") + 1L)
    if (span_chars >= spec$target_characters) break
    n <- n + 1L
    new_lo <- max(0L, i - n)
    new_hi <- min(n_lines, i + n + 1L)
    if (new_lo == lo && new_hi == hi) break  # the document has no more text
    lo <- new_lo
    hi <- new_hi
  }
  c(lo, hi)
}

#' Select the windows of text a model or pattern should read
#'
#' Applies the shared prefilter rules: a line survives only if it is long
#' enough, holds a digit, and is dense enough in non-letter characters, and
#' the reference section is blanked out first. Each surviving line becomes a
#' window that carries the lines around it, because a result can be split by
#' a line break.
#'
#' @param text A document, already normalised and repaired.
#' @param kit A loaded [sc_kit()].
#' @param drop_references Blank the reference section before filtering.
#' @return A data frame with one row per kept window: `start` and `end`
#'   (character offsets into `text`), `line` (the 0-based index of the line
#'   that triggered the window), `text` (the window's own text, built
#'   from the reference-blanked lines), `start_line` and `end_line` (the
#'   0-based first and last line of the window), and `char_start` (the
#'   offset of the window's first character counted over the
#'   reference-blanked lines, the coordinate [sc_units()] and the pipeline
#'   use). Zero rows when nothing survives.
#' @examples
#' kit <- sc_kit()
#' sc_prefilter("The effect was significant, t(28) = 2.87, p = .006.", kit)
#' @export
sc_prefilter <- function(text, kit, drop_references = TRUE) {
  spec <- prefilter_spec(kit)
  prefilter_windows(text, spec, drop_references)
}

# Character offset of the start of each (0-based) line.
line_offsets <- function(lines) cumsum(c(0L, nchar(lines, type = "chars") + 1L))

prefilter_windows <- function(text, spec, drop_references = TRUE) {
  raw_lines <- py_split_lines(text)
  lines <- if (drop_references) strip_references(raw_lines, spec) else raw_lines

  # `start` and `end` are offsets in the ORIGINAL lines: blanking a reference
  # line changes its content, not the document the offsets describe.
  # `char_start` is counted over the blanked lines, as the Python
  # `Window.char_start` is, because the pipeline's statistic intervals use it.
  offsets <- line_offsets(raw_lines)
  blank_offsets <- line_offsets(lines)

  starts <- integer(0); ends <- integer(0); lns <- integer(0); texts <- character(0)
  los <- integer(0); his <- integer(0); cstarts <- integer(0)
  for (i0 in seq_along(lines) - 1L) {
    line <- lines[i0 + 1L]
    if (!keeps_line(line, spec)) next
    span <- prefilter_span(lines, i0, spec)
    lo <- span[1]; hi <- span[2]
    starts <- c(starts, offsets[lo + 1L])
    ends <- c(ends, offsets[hi] + nchar(raw_lines[hi], type = "chars"))
    lns <- c(lns, i0)
    texts <- c(texts, paste(lines[(lo + 1L):hi], collapse = "\n"))
    los <- c(los, lo)
    his <- c(his, hi - 1L)
    cstarts <- c(cstarts, blank_offsets[lo + 1L])
  }

  data.frame(start = starts, end = ends, line = lns, text = texts,
            start_line = los, end_line = his, char_start = cstarts,
            stringsAsFactors = FALSE)
}

#' Select the units of text the model reads
#'
#' Reads the `unit` key of the prefilter spec. With `"passage"` (spec
#' version 2) windows that overlap or touch merge into one passage, so the
#' model reads each character once. With `"window"` every window is a unit.
#'
#' @inheritParams sc_prefilter
#' @return A data frame with one row per unit: `start` and `end` (character
#'   offsets into `text`), `line` (the 0-based first line of a passage, or
#'   the triggering line of a window), `text`, `start_line`, `end_line`,
#'   and `char_start` as in [sc_prefilter()]. Zero rows when nothing
#'   survives.
#' @examples
#' kit <- sc_kit()
#' sc_units("The effect was significant, t(28) = 2.87, p = .006.", kit)
#' @export
sc_units <- function(text, kit, drop_references = TRUE) {
  spec <- prefilter_spec(kit)
  windows <- prefilter_windows(text, spec, drop_references)
  if (!identical(spec$unit, "passage") || nrow(windows) == 0) return(windows)

  raw_lines <- py_split_lines(text)
  lines <- if (drop_references) strip_references(raw_lines, spec) else raw_lines
  offsets <- line_offsets(raw_lines)
  blank_offsets <- line_offsets(lines)

  # Windows come out in line order, but a window grown by the target length
  # can start before the previous one, so sort as the Python reference does.
  windows <- windows[order(windows$start_line), ]
  first <- integer(0); last <- integer(0)
  for (k in seq_len(nrow(windows))) {
    if (length(last) > 0 && windows$start_line[k] <= last[length(last)] + 1L) {
      last[length(last)] <- max(last[length(last)], windows$end_line[k])
    } else {
      first <- c(first, windows$start_line[k])
      last <- c(last, windows$end_line[k])
    }
  }

  data.frame(
    start = offsets[first + 1L],
    end = offsets[last + 1L] + nchar(raw_lines[last + 1L], type = "chars"),
    line = first,
    text = vapply(seq_along(first), function(k) {
      paste(lines[(first[k] + 1L):(last[k] + 1L)], collapse = "\n")
    }, character(1)),
    start_line = first, end_line = last, char_start = blank_offsets[first + 1L],
    stringsAsFactors = FALSE
  )
}
