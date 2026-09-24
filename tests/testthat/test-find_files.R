test_that("find_files matches fuzzy/abbreviated tokens", {
  tmp <- tempfile("impacttest")

  contract_dir <- file.path(
    tmp, "Data", "MA_Plans", "MA_ctrct",
    "Monthly_Report_By_Contract_2023_01_updated"
  )
  dir.create(contract_dir, recursive = TRUE)
  contract_file <- file.path(contract_dir, "Monthly_Report_By_Contract_2023_01.xls")
  file.create(contract_file)

  ideon_dir <- file.path(tmp, "Data", "Ideon", "2024 Medicare Advantage Network Data")
  dir.create(ideon_dir, recursive = TRUE)
  ideon_file <- file.path(ideon_dir, "network_providers.csv")
  file.create(ideon_file)

  on.exit(unlink(tmp, recursive = TRUE))

  res1 <- find_files("2023", "MA_contracts", root = tmp)
  expect_true(normalizePath(contract_file) %in% normalizePath(res1$path))

  res2 <- find_files("Ideon", "2024", "network providers", root = tmp)
  expect_true(normalizePath(ideon_file) %in% normalizePath(res2$path))

  # unrelated term should return nothing
  res3 <- find_files("zzz_nonexistent_term", root = tmp)
  expect_equal(nrow(res3), 0)
})

test_that("match_all = FALSE broadens results", {
  tmp <- tempfile("impacttest")
  dir.create(file.path(tmp, "Data"), recursive = TRUE)
  file.create(file.path(tmp, "Data", "alpha.csv"))
  file.create(file.path(tmp, "Data", "beta.csv"))
  on.exit(unlink(tmp, recursive = TRUE))

  strict <- find_files("alpha", "beta", root = tmp, match_all = TRUE)
  loose <- find_files("alpha", "beta", root = tmp, match_all = FALSE)

  expect_equal(nrow(strict), 0)
  expect_equal(nrow(loose), 2)
})

test_that("get_impact_root respects the option override", {
  old <- getOption("findIMPACT.root")
  options(findIMPACT.root = "/some/explicit/path")
  on.exit(options(findIMPACT.root = old))

  expect_equal(get_impact_root(), "/some/explicit/path")
})

test_that("get_impact_root respects the env var override", {
  old <- Sys.getenv("IMPACT_ROOT", unset = NA)
  Sys.setenv(IMPACT_ROOT = "/another/explicit/path")
  on.exit({
    if (is.na(old)) Sys.unsetenv("IMPACT_ROOT") else Sys.setenv(IMPACT_ROOT = old)
  })

  expect_equal(get_impact_root(), "/another/explicit/path")
})
