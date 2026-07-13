test_that("get_meta extracts and cleans YAML front-matter fields", {
  lines <- c('title: "Data Model"', 'description: A short summary.', 'author: Someone')
  expect_equal(get_meta(lines, "title"), "Data Model")
  expect_equal(get_meta(lines, "description"), "A short summary.")
})

test_that("get_meta returns NA when the field is absent", {
  expect_true(is.na(get_meta(c("title: X"), "description")))
  expect_true(is.na(get_meta(character(0), "title")))
})

test_that("get_meta returns NA for empty values and block-scalar markers", {
  expect_true(is.na(get_meta('title: ""', "title")))
  expect_true(is.na(get_meta("description: >", "description")))
  expect_true(is.na(get_meta("description: |", "description")))
  expect_true(is.na(get_meta("description: >-", "description")))
})

test_that("norm_home adds a trailing slash only when needed", {
  expect_equal(norm_home("https://x.io/pkg"), "https://x.io/pkg/")
  expect_equal(norm_home("https://x.io/pkg/"), "https://x.io/pkg/")
  expect_true(is.na(norm_home(NA)))
  expect_null(norm_home(NULL))
  expect_equal(norm_home(""), "")
})

test_that("article_html maps a vignette path to its pkgdown html file", {
  expect_equal(article_html("Foo.Rmd"), "Foo.html")
  expect_equal(article_html("articles/Bar.Rmd"), "Bar.html")
})

test_that("html_title extracts the <title> text, NA when absent", {
  expect_equal(html_title(c("<head>", "<title>Site KRI Overview</title>", "</head>")),
               "Site KRI Overview")
  expect_true(is.na(html_title(c("<head>", "</head>"))))
})
