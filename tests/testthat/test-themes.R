context("Theme files")

themecolourspec <- c("accent1", "accent2", "accent3",
                     "accent4", "accent5", "accent6",
                     "hlink", "hlinkfol")

themefiles <- list.files(here::here("themes"), pattern = "*.json")
otherfiles <- setdiff(list.files(here::here("themes")), themefiles)

test_that("all themes are JSON", {
  expect_gte(length(themefiles), 1, label="Number of themes")
  expect_true(length(otherfiles)==0, label="All themes are JSON")
})

test_that("all themes load and have metadata", {
  for (json in themefiles){
    theme <- jsonlite::read_json(here::here("themes", json))
    expect_true(length(theme$name)==1, label=paste0(json, " has a `name` attribute"))
    expect_true(length(theme$updated)==1, label=paste0(json, " has an `updated` attribute"))
    expect_error(as.Date(theme$updated), NA, label=paste0("in ", json, ", `updated` attribute is a date"))
  }
})

test_that("all themes have all accent specified as hex", {
  for (json in themefiles){
    theme <- jsonlite::read_json(here::here("themes", json))
    hexlist <- unlist(lapply(theme$palette, "[[", "hex"))
    expect_setequal(names(theme$palette), themecolourspec)
    expect_true(length(hexlist)==length(themecolourspec), label = "All colours have a hex")
    expect_equal(stringr::str_detect(hexlist, "^\\#[0-9a-zA-Z]{6}$"), rep(TRUE, length(themecolourspec)),
                 label = paste0("Hexadecimal specs of all ", json, " colours"), expected.label = "`\\#[:alphanum:]{6}`")
  }
})

test_that("dummy theme follows same rules as real themes", {
    theme <- jsonlite::read_json(here::here("tests", "testthat", "dummy-theme.json"))
    expect_true(length(theme$name)==1, label="Dummy has a `name` attribute")
    expect_true(length(theme$updated)==1, label="Dummy has an `updated` attribute")
    expect_error(as.Date(theme$updated), NA, label="in dummy theme, `updated` attribute is a date")
    hexlist <- unlist(lapply(theme$palette, "[[", "hex"))
    expect_setequal(names(theme$palette), themecolourspec)
    expect_true(length(hexlist)==length(themecolourspec), label = "All colours have a hex")
    expect_equal(stringr::str_detect(hexlist, "^\\#[0-9a-zA-Z]{6}$"), rep(TRUE, length(themecolourspec)),
                 label = "Hexadecimal specs of all dummy colours", expected.label = "`\\#[:alphanum:]{6}`")
})

