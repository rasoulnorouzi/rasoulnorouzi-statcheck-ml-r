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

units_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$units

for (case in units_cases) {
  test_that(paste0("units: ", case$name), {
    got <- sc_units(case$text, kit)
    expect_equal(nrow(got), length(case$expected))
    for (i in seq_along(case$expected)) {
      want <- case$expected[[i]]
      expect_identical(got$start_line[i], as.integer(want$start_line))
      expect_identical(got$end_line[i], as.integer(want$end_line))
      expect_identical(got$start[i], as.integer(want$char_start))
      expect_identical(got$end[i], as.integer(want$char_end))
      expect_identical(digest::digest(got$text[i], algo = "sha256", serialize = FALSE),
                       want$sha256)
    }
  })
}

test_that("sc_units reads the spec: window unit gives one unit per window", {
  windowed <- kit
  windowed$spec$prefilter$unit <- "window"
  text <- units_cases[[1]]$text
  expect_equal(nrow(sc_units(text, windowed)), nrow(sc_prefilter(text, kit)))
  expect_true(nrow(sc_units(text, kit)) <= nrow(sc_prefilter(text, kit)))
})
