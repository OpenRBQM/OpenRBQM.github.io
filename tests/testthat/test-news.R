# ── quarter_label ─────────────────────────────────────────────────────────────

test_that("quarter_label returns correct label for each quarter", {
  expect_equal(quarter_label(as.Date("2025-01-15")), "Q1 2025")
  expect_equal(quarter_label(as.Date("2025-04-01")), "Q2 2025")
  expect_equal(quarter_label(as.Date("2025-07-31")), "Q3 2025")
  expect_equal(quarter_label(as.Date("2025-10-10")), "Q4 2025")
  expect_equal(quarter_label(as.Date("2026-03-31")), "Q1 2026")
})

test_that("quarter_label handles boundary months correctly", {
  expect_equal(quarter_label(as.Date("2025-03-31")), "Q1 2025")  # last day Q1
  expect_equal(quarter_label(as.Date("2025-04-01")), "Q2 2025")  # first day Q2
  expect_equal(quarter_label(as.Date("2025-06-30")), "Q2 2025")  # last day Q2
  expect_equal(quarter_label(as.Date("2025-09-30")), "Q3 2025")  # last day Q3
  expect_equal(quarter_label(as.Date("2025-12-31")), "Q4 2025")  # last day Q4
})

# ── extract_release_desc ───────────────────────────────────────────────────────

test_that("extract_release_desc returns NA for empty/NULL/NA body", {
  expect_true(is.na(extract_release_desc(NULL)))
  expect_true(is.na(extract_release_desc(NA_character_)))
  expect_true(is.na(extract_release_desc("")))
  expect_true(is.na(extract_release_desc("   \n  ")))
})

test_that("extract_release_desc returns NA when body has only noise lines", {
  expect_true(is.na(extract_release_desc("## What's Changed\n**Full Changelog**: https://github.com/x/y/compare/v1...v2")))
  expect_true(is.na(extract_release_desc("---\n<!-- comment -->")))
})

test_that("extract_release_desc returns the first meaningful sentence", {
  body <- "## What's Changed\nThis release adds new features. More details follow."
  expect_equal(extract_release_desc(body), "This release adds new features.")
})

test_that("extract_release_desc strips leading bullet/header markers", {
  expect_equal(extract_release_desc("- Fixed a bug in the pipeline."), "Fixed a bug in the pipeline.")
  expect_equal(extract_release_desc("* Added support for new workflows."), "Added support for new workflows.")
})

test_that("extract_release_desc truncates at word boundary when no sentence end found", {
  long <- paste(rep("word", 40), collapse = " ")  # 200+ chars, no punctuation
  result <- extract_release_desc(long)
  expect_true(endsWith(result, "..."))
  expect_lte(nchar(result), 163)  # 160 + "..."
})

test_that("extract_release_desc returns short text unchanged when under 160 chars", {
  short <- "A brief description with no period"
  expect_equal(extract_release_desc(short), short)
})

# ── insert_releases ────────────────────────────────────────────────────────────

make_rel <- function(pkg, tag, url, date, desc = NA_character_) {
  data.frame(pkg = pkg, tag = tag, url = url,
             date = as.Date(date), desc = desc,
             stringsAsFactors = FALSE)
}

test_that("insert_releases returns lines unchanged when new_rels is empty", {
  lines <- c("## Releases Q1 2025", "", "- [pkg v1.0](url) — 2025-01-01", "")
  expect_equal(insert_releases(lines, data.frame()), lines)
  expect_equal(insert_releases(lines, NULL), lines)
})

test_that("insert_releases adds bullet under existing quarter header (newest first)", {
  lines <- c("## Releases Q1 2025", "", "- [pkg v1.0](https://github.com/x/pkg/releases/tag/v1.0) — 2025-01-01", "")
  new_rel <- make_rel("pkg", "v1.1", "https://github.com/x/pkg/releases/tag/v1.1", "2025-02-01", "New features.")
  result <- insert_releases(lines, new_rel)
  # New bullet should appear before the old one (newest first)
  v11_pos <- which(grepl("v1.1", result))[1]
  v10_pos <- which(grepl("v1.0", result))[1]
  expect_true(length(v11_pos) > 0)
  expect_true(v11_pos < v10_pos)
})

test_that("insert_releases creates a new quarter header when none exists", {
  lines <- c("## Releases Q2 2025", "", "- [pkg v2.0](https://github.com/x/pkg/releases/tag/v2.0) — 2025-04-01", "")
  new_rel <- make_rel("pkg", "v1.0", "https://github.com/x/pkg/releases/tag/v1.0", "2025-01-15")
  result <- insert_releases(lines, new_rel)
  expect_true(any(result == "## Releases Q1 2025"))
})

test_that("insert_releases places new quarter header above older existing header", {
  lines <- c("## Releases Q1 2025", "", "- [pkg v1.0](https://github.com/x/pkg/releases/tag/v1.0) — 2025-01-01", "")
  new_rel <- make_rel("pkg", "v2.0", "https://github.com/x/pkg/releases/tag/v2.0", "2025-05-01")
  result <- insert_releases(lines, new_rel)
  q2_pos <- which(result == "## Releases Q2 2025")[1]
  q1_pos <- which(result == "## Releases Q1 2025")[1]
  expect_true(q2_pos < q1_pos)
})

test_that("insert_releases appends new quarter at end when it is the oldest", {
  lines <- c("## Releases Q3 2025", "", "- [pkg v3.0](https://github.com/x/pkg/releases/tag/v3.0) — 2025-07-01", "")
  new_rel <- make_rel("pkg", "v1.0", "https://github.com/x/pkg/releases/tag/v1.0", "2025-01-01")
  result <- insert_releases(lines, new_rel)
  q1_pos <- which(result == "## Releases Q1 2025")[1]
  q3_pos <- which(result == "## Releases Q3 2025")[1]
  expect_true(q3_pos < q1_pos)
})

test_that("insert_releases includes description in bullet when present", {
  lines <- c("## Releases Q1 2025", "")
  new_rel <- make_rel("pkg", "v1.0", "https://github.com/x/pkg/releases/tag/v1.0", "2025-01-01", "A great release.")
  result <- insert_releases(lines, new_rel)
  expect_true(any(grepl("A great release\\.", result)))
})

test_that("insert_releases omits description separator when desc is NA", {
  lines <- c("## Releases Q1 2025", "")
  new_rel <- make_rel("pkg", "v1.0", "https://github.com/x/pkg/releases/tag/v1.0", "2025-01-01", NA_character_)
  result  <- insert_releases(lines, new_rel)
  bullet  <- result[grepl("v1.0", result)]
  expect_false(grepl(" — $| —$", bullet))
})

test_that("insert_releases handles header as last line without corrupting output", {
  # Header with no trailing blank line or content — the critical off-by-one case
  lines   <- c("## Releases Q1 2025")
  new_rel <- make_rel("pkg", "v1.0", "https://github.com/x/pkg/releases/tag/v1.0", "2025-01-01")
  result  <- insert_releases(lines, new_rel)
  expect_true(any(grepl("v1.0", result)))
  expect_true(result[1] == "## Releases Q1 2025")
})

test_that("extract_release_desc does not cut at decimal points in version numbers", {
  body <- "Updated to v1.0 release notes are here."
  result <- extract_release_desc(body)
  # Should cut at end of full sentence, not at v1.0
  expect_true(grepl("v1.0 release", result))
})
