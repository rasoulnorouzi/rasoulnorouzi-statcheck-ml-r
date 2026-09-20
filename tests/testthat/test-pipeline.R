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
    }
  })
}

test_that("sc_check_text reports the stage counts as an attribute", {
  out <- sc_check_text(pipeline_cases[[2]]$text, kit, model)
  stages <- attr(out, "stages")
  expect_true(stages$windows_kept > 0)
  expect_identical(stages$by_pattern + stages$by_model, nrow(out))
})
