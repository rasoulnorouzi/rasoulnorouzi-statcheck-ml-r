kit <- sc_kit()
model <- sc_load_model(kit)
model_dir <- file.path(kit$path, "model", kit$manifest$model_default)

cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)

test_that("a tensor decodes in the order weights.json wrote it", {
  weights <- jsonlite::fromJSON(file.path(model_dir, "weights.json"),
                                simplifyVector = FALSE)
  shape <- unlist(weights$embedding$shape)
  flat <- readBin(jsonlite::base64_dec(weights$embedding$data), "double",
                  size = 4L, n = prod(shape), endian = "little")

  expect_equal(dim(model$embedding), shape)
  expect_identical(as.numeric(model$embedding[1, ]), flat[seq_len(shape[2])])
  # The first row is the pad character and is all zeros, so it would agree
  # whichever way the values were folded into a matrix. Row 50 is the one
  # that says the rows of the file are the rows of the matrix.
  expect_identical(as.numeric(model$embedding[50, ]),
                   flat[49L * shape[2] + seq_len(shape[2])])
  expect_false(all(model$embedding[50, ] == 0))
})

test_that("the CRF in weights.json is the CRF the decoder ships", {
  decoder <- jsonlite::fromJSON(file.path(model_dir, "decoder.json"))
  expect_true(decoder$has_crf)
  expect_equal(model$crf$transitions, decoder$transitions)
  expect_equal(model$crf$start, decoder$start)
  expect_equal(model$crf$end, decoder$end)
  expect_identical(model$tags, decoder$tags)
})

# The logits are the whole forward pass in one number per character per tag:
# a wrong gate order, a bias on the wrong side of the r gate, or a backward
# pass that runs the wrong way shows up here before it reaches a tag.
for (case in cases$sections$model_logits) {
  test_that(paste0("model logits: ", case$name), {
    expected <- matrix(unlist(case$expected), nrow = length(case$expected),
                       byrow = TRUE)
    encoded <- sc_encode(case$text, model)
    got <- sc_logits(model, encoded$ids, encoded$lengths)[[1]]

    expect_identical(dim(got), dim(expected))
    expect_lt(max(abs(got - expected)), cases$logit_tolerance)
    expect_identical(max.col(got, ties.method = "first"),
                     max.col(expected, ties.method = "first"))
  })
}

test_that("padding a batch moves no text's logits", {
  texts <- vapply(cases$sections$model_logits, function(case) case$text,
                  character(1))
  encoded <- sc_encode(texts, model)
  together <- sc_logits(model, encoded$ids, encoded$lengths)

  # The three texts are of three lengths, so two rows of this batch carry
  # padding that the backward direction would otherwise read.
  expect_gt(max(encoded$lengths), min(encoded$lengths))
  for (i in seq_along(texts)) {
    alone <- sc_encode(texts[i], model)
    expect_lt(max(abs(together[[i]] -
                        sc_logits(model, alone$ids, alone$lengths)[[1]])), 1e-6)
  }
})

# Every case of the section is tagged in one call, the way a document's
# windows are, and then read back case by case.
tag_cases <- cases$sections$model
tagged <- sc_tag(vapply(tag_cases, function(case) case$text, character(1)),
                 kit, model)

for (i in seq_along(tag_cases)) {
  test_that(paste0("model tags: ", tag_cases[[i]]$name), {
    expect_identical(tagged[[i]], unlist(tag_cases[[i]]$expected))
  })
}

test_that("an empty text has no tags and leaves the batch alone", {
  both <- sc_tag(c("", "F(2, 30) = 4.11, p = .03"), kit, model)
  expect_identical(both[[1]], character(0))
  expect_identical(both[[2]], sc_tag("F(2, 30) = 4.11, p = .03", kit, model)[[1]])
})

test_that("a document's worth of windows tags in one batch", {
  sentence <- "The effect was significant, t(28) = 2.87, p = .006. "
  window <- substr(paste(rep(sentence, 12), collapse = ""), 1, 300)
  windows <- rep(window, 100)

  elapsed <- system.time(tagged <- sc_tag(windows, kit, model))[["elapsed"]]
  cat(sprintf("\n100 windows of 300 characters in one batch: %.1f s\n", elapsed))

  expect_length(tagged, 100)
  expect_length(tagged[[1]], 300)
  expect_identical(tagged[[1]][29:32], c("S-TEST", "O", "B-DF1", "E-DF1"))
  # A shared CI runner took 10.9 s where this machine takes 8; the bound is a
  # local benchmark, not a correctness check.
  skip_on_ci()
  expect_lt(elapsed, 10)
})
