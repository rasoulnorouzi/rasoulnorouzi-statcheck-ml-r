kit <- sc_kit()
prefilter_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$prefilter

for (case in prefilter_cases) {
  test_that(paste0("prefilter: ", case$name), {
    got <- sc_prefilter(case$text, kit)
    expect_equal(nrow(got), length(case$expected))
    for (i in seq_along(case$expected)) {
      want <- case$expected[[i]]
      expect_identical(got$start[i], as.integer(want$start))
      expect_identical(got$end[i], as.integer(want$end))
      expect_identical(got$line[i], as.integer(want$line))
    }
  })
}
