group_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$group

group_columns <- c("test_type", "statistic", "df1", "df2", "p_operator", "p_value")

for (case in group_cases) {
  test_that(paste0("group: ", case$name), {
    got <- sc_group(case$text, unlist(case$tags))
    expect_equal(nrow(got), length(case$expected))
    for (i in seq_along(case$expected)) {
      want <- case$expected[[i]]
      for (col in group_columns) {
        if (is.null(want[[col]])) {
          expect_true(is.na(got[[col]][i]))
        } else if (is.character(got[[col]])) {
          expect_identical(got[[col]][i], want[[col]])
        } else {
          expect_equal(got[[col]][i], want[[col]], tolerance = 1e-9)
        }
      }
      expect_identical(got$start[i], as.integer(want$span[[1]]))
      expect_identical(got$end[i], as.integer(want$span[[2]]))
    }
  })
}

test_that("sc_tags_to_spans opens a span for a stray continuation with no B tag", {
  spans <- sc_tags_to_spans(c("I-TEST", "I-TEST", "O"))
  expect_length(spans, 1)
  expect_identical(spans[[1]], list(start = 0L, end = 2L, entity = "TEST"))
})

test_that("sc_tags_to_spans reads a single-character span from an S tag", {
  spans <- sc_tags_to_spans(c("O", "S-DF1", "O"))
  expect_identical(spans[[1]], list(start = 1L, end = 2L, entity = "DF1"))
})
