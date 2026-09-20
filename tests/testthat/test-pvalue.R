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
    got <- sc_verdict(result, reported_p_text = case$p_text,
                      statistic_text = nz(case$statistic_text, NULL))

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

test_that("a field left out of the result list counts as absent", {
   v <- sc_verdict(list(test_type = "t", statistic = 2.45, df1 = 34, p_operator = "=", p_value = 0.02))
   expect_identical(v$verdict, "consistent")
   v <- sc_verdict(list(test_type = "chi2", statistic = 8.69, p_operator = "=", p_value = 0.003))
   expect_identical(v$verdict, "undecidable")
 })

# The four cases below mirror `test_pvalue.py`'s new `test_*` functions,
# added when this project's p-value check was brought in line with
# statcheck's own rule for a rounded statistic (statcheck 1.5.0's
# `error_test`). Before that fix this project allowed only for the rounding
# of the reported p-value, and called some of statcheck's own consistent
# results errors.

test_that("the statistic's rounding widens the comparison", {
  # t(67) = 1.48 implies p = .1436, and a paper that writes p = .143 is not
  # wrong: the statistic itself was rounded, so the true p lies in a range.
  # statcheck 1.5.0 accepts this row; before the rule matched statcheck this
  # project called it inconsistent.
  res <- list(test_type = "t", statistic = 1.48, df1 = 67, p_operator = "=", p_value = 0.143)
  v <- sc_verdict(res, reported_p_text = ".143", statistic_text = "1.48")
  expect_identical(v$verdict, "consistent")

  # A statistic printed to more decimals leaves less room, and the same
  # reported p is then an error.
  res <- list(test_type = "t", statistic = 1.4800, df1 = 67, p_operator = "=", p_value = 0.143)
  v <- sc_verdict(res, reported_p_text = ".143", statistic_text = "1.4800")
  expect_identical(v$verdict, "inconsistent")
})

test_that("a reported p of zero is an error", {
  # No test gives exactly zero, whatever the computed value is.
  res <- list(test_type = "t", statistic = 12.0, df1 = 30, p_operator = "=", p_value = 0.0)
  v <- sc_verdict(res, reported_p_text = ".000", statistic_text = "12.0")
  expect_identical(v$verdict, "inconsistent")
})

test_that("ns reads as greater than alpha", {
  res <- list(test_type = "t", statistic = 0.54, df1 = 178, p_operator = "ns", p_value = NULL)
  v <- sc_verdict(res, statistic_text = "0.54")
  expect_identical(v$verdict, "consistent")

  # A statistic that is significant contradicts the claim of no significance.
  res <- list(test_type = "t", statistic = 5.0, df1 = 178, p_operator = "ns", p_value = NULL)
  v <- sc_verdict(res, statistic_text = "5.0")
  expect_identical(v$verdict, "decision_error")
})

test_that("the < and > operators use the whole interval", {
  # p < .01 with a computed .0099: the paper's bound holds.
  res <- list(test_type = "t", statistic = 2.70, df1 = 90, p_operator = "<", p_value = 0.01)
  v <- sc_verdict(res, reported_p_text = ".01", statistic_text = "2.70")
  expect_identical(v$verdict, "consistent")

  # p < .001 with a computed .008 does not. Both sit under alpha, so the
  # paper's conclusion still holds and the verdict is not a decision error.
  res <- list(test_type = "t", statistic = 2.70, df1 = 90, p_operator = "<", p_value = 0.001)
  v <- sc_verdict(res, reported_p_text = ".001", statistic_text = "2.70")
  expect_identical(v$verdict, "inconsistent")

  # p < .05 claimed where the statistic gives .38 flips the conclusion.
  res <- list(test_type = "t", statistic = 0.88, df1 = 90, p_operator = "<", p_value = 0.05)
  v <- sc_verdict(res, reported_p_text = ".05", statistic_text = "0.88")
  expect_identical(v$verdict, "decision_error")
})
