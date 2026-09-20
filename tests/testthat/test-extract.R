kit <- sc_kit()
extract_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$extract

extract_columns <- c("test_type", "statistic", "df1", "df2", "p_operator", "p_value")

# A parity case writes "no value reported" as JSON null. Read with
# simplifyVector = FALSE that becomes R NULL, and sc_extract reports the
# same absence as NA_character_ in the data frame. A NULL in `expected` is
# therefore compared against `is.na()`, everything else with identical().
for (case in extract_cases) {
  test_that(paste0("extract: ", case$name), {
    got <- sc_extract(case$text, kit)
    expect_equal(nrow(got), length(case$expected))
    for (i in seq_along(case$expected)) {
      want <- case$expected[[i]]
      for (col in extract_columns) {
        if (is.null(want[[col]])) {
          expect_true(is.na(got[[col]][i]))
        } else {
          expect_identical(got[[col]][i], want[[col]])
        }
      }
    }
  })
}
