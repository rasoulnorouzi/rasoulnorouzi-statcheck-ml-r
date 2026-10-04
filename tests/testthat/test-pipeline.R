kit <- sc_kit()
model <- sc_load_model(kit)
pipeline_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$pipeline

pipeline_columns <- c("source", "test_type", "statistic", "df1", "df2",
                      "p_operator", "p_value", "computed_p", "verdict")

for (case in pipeline_cases) {
  test_that(paste0("pipeline: ", case$name), {
    got <- sc_check_text(case$text, kit, model)
    expect_equal(nrow(got), length(case$expected))
    for (i in seq_along(case$expected)) {
      want <- case$expected[[i]]
      for (col in pipeline_columns) {
        if (is.null(want[[col]])) {
          expect_true(is.na(got[[col]][i]))
        } else if (is.character(got[[col]])) {
          expect_identical(got[[col]][i], want[[col]])
        } else {
          expect_equal(got[[col]][i], want[[col]], tolerance = 1e-9)
        }
      }
      expect_identical(got$line[i], as.integer(want$line))
      expect_identical(c(got$statistic_start[i], got$statistic_end[i]),
                       as.integer(unlist(want$statistic_span)))
    }

    fragments <- attr(got, "fragments")
    expect_equal(nrow(fragments), length(case$expected_fragments))
    for (i in seq_along(case$expected_fragments)) {
      want <- case$expected_fragments[[i]]
      expect_true(is.na(fragments$test_type[i]))
      expect_equal(fragments$statistic[i], want$statistic, tolerance = 1e-9)
      expect_identical(fragments$source[i], want$source)
      expect_identical(fragments$line[i], as.integer(want$line))
      expect_identical(c(fragments$statistic_start[i], fragments$statistic_end[i]),
                       as.integer(unlist(want$statistic_span)))
    }
  })
}

test_that("the pipeline parity cases carry the dedup rule and a fragment case", {
  expect_true(any(vapply(pipeline_cases, function(c) c$name == "dedup-same-statistic", logical(1))))
  expect_true(any(vapply(pipeline_cases, function(c) c$name == "fragment-bare-statistic", logical(1))))
})

test_that("a find with no test name becomes a fragment with no verdict", {
  frame <- fragment_frame(list(
    list(source = "model", statistic = 3.1, df1 = NA_real_, df2 = NA_real_,
         p_operator = "=", p_value = 0.02, line = 4L, statistic_span = c(10L, 13L))))
  expect_identical(names(frame), names(empty_fragments()))
  expect_true(is.na(frame$test_type))
  expect_false("verdict" %in% names(frame))
  expect_identical(frame$statistic_end, 13L)
})

test_that("sc_check_text reports the stage counts as an attribute", {
  out <- sc_check_text(pipeline_cases[[2]]$text, kit, model)
  stages <- attr(out, "stages")
  expect_true(stages$windows_kept > 0)
  expect_identical(stages$by_pattern + stages$by_model,
                   nrow(out) + nrow(attr(out, "fragments")))
  expect_identical(stages$fragments, nrow(attr(out, "fragments")))
  expect_true(stages$units_kept > 0 && stages$units_kept <= stages$windows_kept)
})
