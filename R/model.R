# The character tagger, run from the numbers instead of from a runtime.
#
# The kit ships `tagger.onnx` for the ports that have an ONNX runtime. R has
# none, so this file reads `weights.json` -- the same checkpoint written out
# as plain arrays -- and runs the forward pass itself. It mirrors
# `src/statcheck_ml/rnn_numpy.py` in the mother repository step for step:
# PyTorch's gate order, PyTorch's bias placement, and the mask that keeps
# padding out of the backward direction. A silent difference in any of the
# three moves every tag, so the parity cases in `tests/testthat/test-model.R`
# are what says this port is right.

# One tensor entry of weights.json: base64 of little-endian float32, written
# in C (row-major) order. R fills a matrix down its columns, so filling the
# transpose and transposing it back recovers PyTorch's own orientation.
model_tensor <- function(entry) {
  shape <- unlist(entry$shape)
  values <- readBin(jsonlite::base64_dec(entry$data), what = "double",
                    size = 4L, n = prod(shape), endian = "little")
  if (length(shape) == 1L) {
    values
  } else {
    t(matrix(values, nrow = shape[2], ncol = shape[1]))
  }
}

# A lookup from Unicode code point to character id. Code points, rather than
# the characters themselves, because string comparison in R goes through the
# locale and the three ports must agree on every machine.
model_charmap <- function(chars) {
  keys <- names(chars)
  # The map's first key is U+0000, the pad character, which an R string
  # cannot hold at all: `names` returns "" for it. It is the pad id, and no
  # text a port reads contains it, so the lookup does without it.
  named <- nzchar(keys)
  points <- vapply(keys[named], utf8ToInt, integer(1), USE.NAMES = FALSE)
  map <- rep(NA_integer_, max(points) + 1L)
  map[points + 1L] <- as.integer(unlist(chars[named], use.names = FALSE))
  map
}

#' Load a trained tagger from a kit
#'
#' Reads the model's `weights.json` -- the embedding table, both directions
#' of both recurrent layers, the tag projection, and the CRF -- and returns
#' everything [sc_tag()] needs. Loading a model costs a second or so, so a
#' caller that tags many documents should load it once and pass it on.
#'
#' @param kit A loaded [sc_kit()].
#' @param config Which model in the kit to load. Defaults to the kit's own
#'   default model.
#' @return An `sc_model`.
#' @examples
#' model <- sc_load_model(sc_kit())
#' model
#' @export
sc_load_model <- function(kit, config = kit$manifest$model_default) {
  weights <- jsonlite::fromJSON(
    file.path(kit$path, "model", config, "weights.json"), simplifyVector = FALSE)

  embedding <- model_tensor(weights$embedding)
  layers <- vector("list", weights$layers)
  for (entry in weights$rnn) {
    # A step is `x %*% t(W_ih) + h %*% t(W_hh)`, so both matrices are
    # transposed once here and never inside the recurrence.
    side <- list(W_ih_t = t(model_tensor(entry$W_ih)),
                 W_hh_t = t(model_tensor(entry$W_hh)),
                 b_ih = model_tensor(entry$b_ih),
                 b_hh = model_tensor(entry$b_hh))
    # The first layer reads one of 183 embedding rows and nothing else, so
    # its input projection takes only 183 distinct values. Projecting the
    # whole table once turns that product into a row lookup.
    if (entry$layer == 0L) side$table <- embedding %*% side$W_ih_t
    layers[[entry$layer + 1L]][[entry$direction]] <- side
  }

  crf <- NULL
  if (!is.null(weights$crf)) {
    # The JSON holds the transition matrix as a list of rows, and
    # `transitions[i, j]` scores tag j following tag i.
    crf <- list(
      transitions = matrix(unlist(weights$crf$transitions),
                           nrow = length(weights$crf$transitions), byrow = TRUE),
      start = unlist(weights$crf$start),
      end = unlist(weights$crf$end))
  }

  structure(
    list(
      config = config,
      unit = weights$unit,
      layers = weights$layers,
      hidden = weights$hidden,
      embedding = embedding,
      rnn = layers,
      out_W_t = t(model_tensor(weights$out$W)),
      out_b = model_tensor(weights$out$b),
      tags = unlist(weights$tags),
      pad_id = weights$pad_id,
      unk_id = weights$unk_id,
      charmap = model_charmap(kit$spec$charmap$chars),
      crf = crf
    ),
    class = "sc_model"
  )
}

#' @export
print.sc_model <- function(x, ...) {
  cat(sprintf("<sc_model> %s: %d bidirectional %s layers, %d hidden, %d tags%s\n",
              x$config, x$layers, toupper(x$unit), x$hidden, length(x$tags),
              if (is.null(x$crf)) ", softmax" else ", CRF"))
  invisible(x)
}

# Characters to ids, padded to the longest text in the batch. The ids are
# the model's own, counted from zero, as `weights.json` and the Python
# reference count them.
sc_encode <- function(texts, model) {
  points <- lapply(enc2utf8(texts), utf8ToInt)
  lengths <- vapply(points, length, integer(1))
  ids <- matrix(model$pad_id, nrow = length(texts), ncol = max(lengths))
  for (b in seq_along(points)) {
    row <- model$charmap[points[[b]] + 1L]
    row[is.na(row)] <- model$unk_id
    ids[b, seq_len(lengths[b])] <- row
  }
  list(ids = ids, lengths = lengths)
}

# One direction of one layer, over the whole batch at once. `x` is the
# layer's input, one row per position, laid out time by time: position
# (t, b) sits in row (t - 1) * batch + b. The return has the same layout.
rnn_direction <- function(side, x, ids, mask, backward, batch, width, model) {
  hidden <- model$hidden
  gate <- lapply(seq_len(if (model$unit == "gru") 3L else 4L),
                 function(k) ((k - 1L) * hidden + 1L):(k * hidden))
  # The biases are held as a matrix of the batch's shape, because adding a
  # vector to a matrix recycles down the columns and these must land on the
  # rows. The two cannot be added together first: the GRU's n gate scales
  # the hidden half by r and leaves the input half alone.
  b_ih <- matrix(side$b_ih, batch, length(side$b_ih), byrow = TRUE)
  b_hh <- matrix(side$b_hh, batch, length(side$b_hh), byrow = TRUE)

  h <- matrix(0, batch, hidden)
  cell <- matrix(0, batch, hidden)
  out <- matrix(0, width * batch, hidden)
  for (t in if (backward) seq.int(width, 1L) else seq_len(width)) {
    rows <- ((t - 1L) * batch + 1L):(t * batch)
    gx <- if (is.null(side$table)) {
      x[rows, , drop = FALSE] %*% side$W_ih_t
    } else {
      side$table[ids[rows] + 1L, , drop = FALSE]
    }
    gx <- gx + b_ih
    gh <- h %*% side$W_hh_t + b_hh

    if (model$unit == "gru") {
      r <- 1 / (1 + exp(-(gx[, gate[[1]]] + gh[, gate[[1]]])))
      z <- 1 / (1 + exp(-(gx[, gate[[2]]] + gh[, gate[[2]]])))
      n <- tanh(gx[, gate[[3]]] + r * gh[, gate[[3]]])
      h <- (1 - z) * n + z * h
    } else {
      i <- 1 / (1 + exp(-(gx[, gate[[1]]] + gh[, gate[[1]]])))
      f <- 1 / (1 + exp(-(gx[, gate[[2]]] + gh[, gate[[2]]])))
      g <- tanh(gx[, gate[[3]]] + gh[, gate[[3]]])
      o <- 1 / (1 + exp(-(gx[, gate[[4]]] + gh[, gate[[4]]])))
      cell <- f * cell + i * g
      h <- o * tanh(cell)
    }

    # The caller masks the backward direction alone, and only when the batch
    # holds padding. The backward pass walks right to left, so it reaches the
    # last real position carrying the state it built from the padding beyond
    # it, and PyTorch never resets that state. Forcing a padding column's
    # state to zero leaves the last real position with the same zero state
    # that a window tagged on its own would start from. The forward direction
    # needs none of this: padding always sits after the text it follows.
    if (!is.null(mask)) {
      h <- h * mask[, t]
      cell <- cell * mask[, t]
    }
    out[rows, ] <- h
  }
  out
}

# Tag logits for a padded batch of ids: a list of one `length` by tag matrix
# per row of `ids`.
sc_logits <- function(model, ids, lengths) {
  batch <- nrow(ids)
  width <- ncol(ids)
  # `ids` is batch by width and R stores a matrix down its columns, so the
  # plain vector already runs time by time, which is the order the
  # recurrence reads it in.
  flat <- as.vector(ids)
  # A batch of windows of one length carries no padding, and multiplying
  # every state by a column of ones is then pure cost.
  mask <- if (any(lengths < width)) {
    matrix(as.numeric(ids != model$pad_id), batch, width)
  }

  x <- NULL
  for (layer in seq_len(model$layers)) {
    pair <- model$rnn[[layer]]
    forward <- rnn_direction(pair$forward, x, flat, NULL, FALSE, batch, width, model)
    backward <- rnn_direction(pair$backward, x, flat, mask, TRUE, batch, width, model)
    # The next layer reads, at each position, the forward and the backward
    # state of this one.
    x <- cbind(forward, backward)
  }

  logits <- x %*% model$out_W_t +
    matrix(model$out_b, nrow(x), length(model$out_b), byrow = TRUE)
  lapply(seq_len(batch), function(b) {
    logits[seq.int(b, by = batch, length.out = lengths[b]), , drop = FALSE]
  })
}

# The best tag path for one sequence, mirroring `viterbi_decode` in
# `statcheck_ml.onnx_runtime`. Returns tag indices, counted from one.
sc_viterbi <- function(emissions, crf) {
  n_tags <- ncol(emissions)
  time <- nrow(emissions)
  score <- crf$start + emissions[1, ]
  if (time == 1L) return(which.max(score + crf$end))

  history <- matrix(0L, time - 1L, n_tags)
  for (t in 2:time) {
    # R recycles a vector down the columns, so this adds the running score
    # of tag i to row i, which is where the transitions out of tag i are.
    step <- crf$transitions + score
    # numpy's argmax takes the first of several equal maxima, and
    # `max.col` picks among them at random unless it is told not to.
    best <- max.col(t(step), ties.method = "first")
    history[t - 1L, ] <- best
    score <- step[cbind(best, seq_len(n_tags))] + emissions[t, ]
  }

  path <- integer(time)
  path[time] <- which.max(score + crf$end)
  for (t in seq.int(time - 1L, 1L)) path[t] <- history[t, path[t + 1L]]
  path
}

#' Tag the characters of a text with the trained model
#'
#' Returns one tag per character, in the BIOES scheme the mother repository
#' defines. The text must be the text the model reads: `sc_tag()` runs no
#' normalisation of its own, because the caller decides whether a window
#' comes from [sc_normalize()] or from somewhere else.
#'
#' Every text given in one call is tagged in one batch, so tagging a whole
#' document's windows together costs little more than tagging one of them.
#' A shorter text is padded to the longest, and the padding changes no
#' text's tags.
#'
#' @param texts A character vector.
#' @param kit A loaded [sc_kit()].
#' @param model A model from [sc_load_model()]. Loaded from `kit` when it is
#'   not given, which is worth avoiding in a loop.
#' @return A list of character vectors, one per text, each as long as its
#'   text has characters.
#' @examples
#' kit <- sc_kit()
#' sc_tag("F(2, 30) = 4.11, p = .03", kit)[[1]]
#' @export
sc_tag <- function(texts, kit, model = sc_load_model(kit)) {
  tags <- rep(list(character(0)), length(texts))
  wanted <- which(nzchar(texts))
  if (length(wanted) == 0L) return(tags)

  encoded <- sc_encode(texts[wanted], model)
  logits <- sc_logits(model, encoded$ids, encoded$lengths)
  paths <- if (is.null(model$crf)) {
    lapply(logits, max.col, ties.method = "first")
  } else {
    lapply(logits, sc_viterbi, crf = model$crf)
  }
  tags[wanted] <- lapply(paths, function(path) model$tags[path])
  tags
}
