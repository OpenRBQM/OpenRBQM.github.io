test_that("order_packages puts curated names first (in order), then the rest", {
  names   <- c("workr", "open.gismo", "gsm.core", "qcthat")
  curated <- c("gsm.core", "workr", "gsm.mapping")
  expect_equal(order_packages(names, curated),
               c("gsm.core", "workr", "open.gismo", "qcthat"))
})

test_that("order_packages is case-insensitive and preserves original casing", {
  expect_equal(order_packages(c("Workr", "GSM.core"), c("gsm.core", "workr")),
               c("GSM.core", "Workr"))
})

test_that("vignette_markdown renders a heading and bullets per section", {
  sections <- list(
    list(repo = "gsm.core", articles = data.frame(
      title = c("KRI Method", "Data Model"),
      url = c("https://x/articles/KRIMethod.html", "https://x/articles/DataModel.html"),
      description = c("Stats methods.", NA),
      stringsAsFactors = FALSE
    ))
  )
  md <- vignette_markdown(sections)
  expect_true(grepl("## gsm.core", md))
  expect_true(grepl("\\[KRI Method\\]\\(https://x/articles/KRIMethod.html\\) — Stats methods.", md))
  # NA description -> no em dash appended
  expect_true(grepl("\\[Data Model\\]\\(https://x/articles/DataModel.html\\)\n", md) ||
              endsWith(md, "[Data Model](https://x/articles/DataModel.html)\n"))
  expect_false(grepl("DataModel.html\\) —", md))
})

test_that("vignette_markdown skips sections with no articles", {
  sections <- list(
    list(repo = "empty", articles = data.frame()),
    list(repo = "gsm.core", articles = data.frame(
      title = "KRI Method", url = "https://x/articles/KRIMethod.html",
      description = NA, stringsAsFactors = FALSE))
  )
  md <- vignette_markdown(sections)
  expect_false(grepl("## empty", md))
  expect_true(grepl("## gsm.core", md))
})
