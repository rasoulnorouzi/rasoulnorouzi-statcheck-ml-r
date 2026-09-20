# The kit is the shared material a port needs: the prefilter, the p-value
# constants, the character vocabulary, and one or more trained models. The
# mother repository writes it; this file only reads and verifies it.

TEXT_SUFFIXES <- c("json", "jsonl", "csv", "txt", "md")

# Fold every CRLF pair in a raw byte vector down to LF, so a text file hashes
# the same whether git checked it out on Windows or on Linux.
fold_crlf <- function(raw) {
  if (length(raw) < 2) return(raw)
  cr <- as.raw(0x0d)
  lf <- as.raw(0x0a)
  is_cr <- raw == cr
  is_next_lf <- c(raw[-1] == lf, FALSE)
  raw[!(is_cr & is_next_lf)]
}

kit_hash_file <- function(path) {
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% TEXT_SUFFIXES) raw <- fold_crlf(raw)
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}

# Stop at the first file in the manifest whose hash does not match, naming
# it, rather than collecting every mismatch. A stale kit is a stop condition,
# not a report.
kit_verify <- function(path, manifest) {
  for (rel in names(manifest$files)) {
    full <- file.path(path, rel)
    if (!file.exists(full)) {
      stop(sprintf("kit verification failed: %s is missing", rel), call. = FALSE)
    }
    actual <- kit_hash_file(full)
    if (!identical(actual, manifest$files[[rel]])) {
      stop(sprintf("kit verification failed: %s does not match manifest.json", rel),
           call. = FALSE)
    }
  }
}

kit_read_spec <- function(spec_dir) {
  files <- list.files(spec_dir, pattern = "\\.json$")
  names <- tools::file_path_sans_ext(files)
  spec <- lapply(files, function(f) {
    jsonlite::fromJSON(file.path(spec_dir, f), simplifyVector = FALSE)
  })
  stats::setNames(spec, names)
}

#' Load and verify a port kit
#'
#' A kit holds the shared rules and a trained model that a statcheck-ml port
#' needs: the prefilter, the normalisation rules, the character vocabulary,
#' and the p-value constants, all in `spec/`, plus one or more models in
#' `model/`. The mother repository writes the kit; a port only reads it.
#'
#' Every file the kit's `manifest.json` lists is hash-checked before
#' [sc_kit()] returns, so a copy that has drifted from the mother repository
#' is refused rather than silently trusted.
#'
#' @param path Directory holding `manifest.json`, `spec/`, and `model/`.
#'   Defaults to the kit shipped inside the installed package.
#' @return An `sc_kit`: a list with `spec` (one parsed JSON per file in
#'   `spec/`, named after the file), `model_dir` (the default model's
#'   directory), `manifest` (the parsed `manifest.json`), and `path`.
#' @examples
#' kit <- sc_kit()
#' kit$spec$normalize$target_line_width
#' @export
sc_kit <- function(path = system.file("kit", package = "statcheckml")) {
  manifest <- jsonlite::fromJSON(file.path(path, "manifest.json"), simplifyVector = FALSE)
  kit_verify(path, manifest)

  structure(
    list(
      spec = kit_read_spec(file.path(path, "spec")),
      model_dir = file.path(path, "model", manifest$model_default),
      manifest = manifest,
      path = path
    ),
    class = "sc_kit"
  )
}

#' Summarise a loaded kit
#'
#' Prints the kit version, the mother repository commit it was exported
#' from, and the models it ships.
#'
#' @param kit An [sc_kit()].
#' @return `kit`, invisibly.
#' @examples
#' sc_kit_info(sc_kit())
#' @export
sc_kit_info <- function(kit) {
  cat(sprintf("statcheck-ml kit %s\n", kit$manifest$kit_version))
  cat(sprintf("mother commit %s\n", kit$manifest$mother_commit))
  cat(sprintf("models: %s\n", paste(unlist(kit$manifest$models), collapse = ", ")))
  invisible(kit)
}

#' @export
print.sc_kit <- function(x, ...) {
  cat(sprintf("<sc_kit> %s\n", x$path))
  cat(sprintf("  kit %s, models: %s\n", x$manifest$kit_version,
              paste(unlist(x$manifest$models), collapse = ", ")))
  invisible(x)
}
