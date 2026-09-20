kit <- sc_kit()
repair_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$repair

for (case in repair_cases) {
  test_that(paste0("repair: ", case$name), {
    got <- sc_repair(case$text, kit)
    expect_identical(got$text, case$expected$text)
    expect_identical(got$replacements, as.integer(case$expected$replacements))
  })
}
