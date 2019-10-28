context("Theme-unpacking functions")

test_that("default theme is set", {
  expect_error(default_theme(), NA)
})

test_that("unpack_pal retrieves the dummy theme palette", {
  dummy_theme_pal <- c("dummy1" = "#111AAA",
                       "dummy2" = "#222BBB",
                       "dummy3" = "#333CCC",
                       "dummy4" = "#444DDD",
                       "dummy5" = "#555EEE",
                       "dummy6" = "#666FFF",
                       "dummyhlink" = "#777GGG",
                       "dummyhlinkfol" = "#888HHH")
  expect_equal(unpack_pal(here::here("tests/testthat/dummy-theme.json")),
               dummy_theme_pal, label = "Parsed dummy theme colors")
})

test_that("moj_colours extracts from dummy theme", {
  expect_equal(moj_colours("dummy1", theme="dummy"),
               c("dummy1" = "#111AAA"),
               label = "Extracted colours ", expected.label = "dummy set colours")
  expect_equal(moj_colours("dummy2", "dummy3", "dummy4", theme="dummy"),
               c("dummy2" = "#222BBB", "dummy3" = "#333CCC", "dummy4" = "#444DDD"),
               label = "Extracted colours ", expected.label = "dummy set colours")
  expect_equal(moj_colours(c("dummy5", "dummy6"), theme="dummy"),
               c("dummy5" = "#555EEE", "dummy6" = "#666FFF"),
               label = "Extracted colours ", expected.label = "dummy set colours")
})

test_that("moj_colours extracts from the default theme", {
  expect_gte(length(moj_colours()), 7L, label = "Number of extracted colours from default theme")
})
