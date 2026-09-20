kit <- sc_kit()
normalise_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$normalise

for (case in normalise_cases) {
  test_that(paste0("normalise: ", case$engine, "/", case$name), {
    expect_identical(sc_normalize(case$text, kit), case$expected)
  })
}
