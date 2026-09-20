pvalue_cases <- jsonlite::fromJSON(
  system.file("kit/parity/cases.json", package = "statcheckml"),
  simplifyVector = FALSE
)$sections$pvalue

nz <- function(x, default = NA) if (is.null(x)) default else x

for (case in pvalue_cases) {
  test_that(paste0("pvalue: ", case$name), {
    result <- list(
      test_type = case$test_type,
      statistic = nz(case$statistic, NA_real_),
      df1 = nz(case$df1, NA_real_),
      df2 = nz(case$df2, NA_real_),
      p_operator = nz(case$p_operator, NA_character_),
      p_value = sc_parse_number(case$p_text)
    )
    got <- sc_verdict(result, reported_p_text = case$p_text)

    expect_identical(got$verdict, case$expected$verdict)
    if (is.null(case$expected$computed_p)) {
      expect_true(is.na(got$computed_p))
    } else {
      expect_equal(got$computed_p, case$expected$computed_p, tolerance = 1e-9)
    }
  })
}

test_that("sc_parse_number strips an operator, a leading dot, and a unicode dash", {
  expect_identical(sc_parse_number("<.001"), 0.001)
  expect_identical(sc_parse_number(">.05"), 0.05)
  expect_identical(sc_parse_number("=.03"), 0.03)
  expect_identical(sc_parse_number("\u22122.87"), -2.87)
  expect_identical(sc_parse_number("-.5"), -0.5)
  expect_true(is.na(sc_parse_number(NULL)))
  expect_true(is.na(sc_parse_number("")))
  expect_true(is.na(sc_parse_number("not a number")))
})

test_that("sc_compute_p never returns NaN", {
  expect_true(is.na(sc_compute_p("chi2", 5, 0)))
  expect_true(is.na(sc_compute_p("f", 5, 2, NA_real_)))
  expect_true(is.na(sc_compute_p("r", 1.0, 15)))
  expect_true(is.na(sc_compute_p("bogus", 5, 2)))
})
