fixture <- function() {
  data.frame(
    name = c("gsm.core", "workr"),
    full_name = c("Gilead-BioStats/gsm.core", "Gilead-BioStats/workr"),
    html_url = c("https://github.com/Gilead-BioStats/gsm.core",
                 "https://github.com/Gilead-BioStats/workr"),
    homepage = c("https://gilead-biostats.github.io/gsm.core", ""),
    description = c("Analytics engine.", "Workflow runtime."),
    stringsAsFactors = FALSE
  )
}

test_that("render_cards builds grid cards with source, website, and description", {
  md <- render_cards(fixture())
  expect_true(grepl("::: \\{.grid\\}", md))
  expect_true(grepl("\\[gsm.core\\]\\(https://github.com/Gilead-BioStats/gsm.core\\)", md))
  expect_true(grepl("\\[Website\\]\\(https://gilead-biostats.github.io/gsm.core\\)", md))
  expect_true(grepl("Analytics engine.", md))
})

test_that("render_cards omits the Website link when homepage is blank", {
  md <- render_cards(fixture()[2, ])          # workr has empty homepage
  expect_false(grepl("Website", md))
})

test_that("render_cards omits the Website link when homepage is NA", {
  df <- fixture()[1, ]
  df$homepage <- NA_character_
  expect_false(grepl("Website", render_cards(df)))
})

test_that("render_cards returns empty string for an empty data frame", {
  expect_equal(render_cards(fixture()[0, ]), "")
})

test_that("render_section adds a heading and respects pkg_names order", {
  md <- render_section(fixture(), "Core Packages", c("workr", "gsm.core"))
  expect_true(grepl("## Core Packages", md))
  # workr requested first, so its card should appear before gsm.core's
  expect_lt(regexpr("workr", md), regexpr("gsm.core", md))
})

test_that("render_section returns empty string when no packages match", {
  expect_equal(render_section(fixture(), "Nope", c("does.not.exist")), "")
})

test_that("remaining_packages returns names not already used (case-insensitive)", {
  expect_equal(remaining_packages(fixture(), c("GSM.CORE")), "workr")
})
