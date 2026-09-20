test_that("sc_kit loads the shipped kit and verifies every file", {
  kit <- sc_kit()
  expect_s3_class(kit, "sc_kit")
  expect_setequal(names(kit$spec),
                   c("charmap", "font_table", "normalize", "prefilter", "repair"))
  expect_true(dir.exists(kit$model_dir))
  expect_identical(kit$manifest$kit_version, "2.0.0")
  expect_identical(kit$manifest$model_default, "gru-crf")
})

test_that("sc_kit_info and print.sc_kit report the kit version and models", {
  kit <- sc_kit()
  expect_output(sc_kit_info(kit), "statcheck-ml kit 2.0.0")
  expect_output(print(kit), "<sc_kit>")
})

test_that("sc_kit refuses a kit with a changed spec file, naming it", {
  tmp <- file.path(tempdir(), "statcheckml-bad-kit")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.copy(system.file("kit", package = "statcheckml"), tmp, recursive = TRUE)
  bad_kit <- file.path(tmp, "kit")

  target <- file.path(bad_kit, "spec", "normalize.json")
  raw <- readBin(target, "raw", n = file.info(target)$size)
  raw[1] <- as.raw(bitwXor(as.integer(raw[1]), 1L))
  writeBin(raw, target)

  expect_error(sc_kit(bad_kit), "spec/normalize\\.json")
})

test_that("sc_kit refuses a kit that is missing a manifest file", {
  tmp <- file.path(tempdir(), "statcheckml-missing-kit")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.copy(system.file("kit", package = "statcheckml"), tmp, recursive = TRUE)
  bad_kit <- file.path(tmp, "kit")
  unlink(file.path(bad_kit, "spec", "repair.json"))

  expect_error(sc_kit(bad_kit), "spec/repair\\.json")
})
