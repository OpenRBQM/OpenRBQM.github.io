# GitHub API helpers shared by the site's .qmd pages.

library(httr)
library(jsonlite)
library(dplyr)

#' Authenticated GET against the GitHub API.
#' Uses GITHUB_PAT as a bearer token when set (higher rate limits); otherwise
#' falls back to unauthenticated requests so local renders still work.
gh_get <- function(url) {
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(pat)) {
    httr::GET(url, httr::add_headers(Authorization = paste("Bearer", pat)))
  } else {
    httr::GET(url)
  }
}

#' All public repositories for a GitHub org, as a data frame.
#' Fails loudly with the API's message on a non-200 / unexpected response
#' (e.g. rate limiting) instead of letting a bogus frame crash downstream.
get_repos <- function(org) {
  url <- paste0("https://api.github.com/orgs/", org, "/repos?type=public&per_page=100")
  response <- gh_get(url)
  parsed <- jsonlite::fromJSON(httr::content(response, as = "text"))
  if (httr::status_code(response) != 200 || !is.data.frame(parsed) || !"name" %in% names(parsed)) {
    msg <- if (is.list(parsed) && !is.null(parsed$message)) parsed$message else "unexpected response"
    stop(sprintf("GitHub API request for org '%s' failed (status %s): %s",
                 org, httr::status_code(response), msg), call. = FALSE)
  }
  parsed
}

#' pkgdown article HTML file name for a vignette source file.
#' Both vignettes/Foo.Rmd and vignettes/articles/Foo.Rmd render to articles/Foo.html.
article_html <- function(name) {
  sub("\\.Rmd$", ".html", basename(name))
}

#' Title text from an HTML page's <title> element (NA when absent).
html_title <- function(lines) {
  txt <- paste(lines, collapse = " ")
  m <- regmatches(txt, regexpr("<title>[^<]*</title>", txt, ignore.case = TRUE))
  if (length(m) == 0) return(NA_character_)
  trimws(gsub("</?title>", "", m, ignore.case = TRUE))
}

#' List a repo directory's files matching `pattern` via the contents API.
#' `ref` selects a branch (e.g. "gh-pages"). Returns a data frame
#' (name, download_url) — empty when the directory is missing or has no matches.
list_dir <- function(org, repo, path, pattern, ref = NULL) {
  empty <- data.frame(name = character(), download_url = character(), stringsAsFactors = FALSE)
  url <- paste0("https://api.github.com/repos/", org, "/", repo, "/contents/", path)
  if (!is.null(ref)) url <- paste0(url, "?ref=", ref)
  response <- gh_get(url)
  if (response$status_code != 200) return(empty)
  json <- jsonlite::fromJSON(httr::content(response, as = "text"))
  if (!is.data.frame(json) || !"name" %in% names(json)) return(empty)
  hit <- json[grepl(pattern, json$name), , drop = FALSE]
  if (nrow(hit) == 0) return(empty)
  data.frame(name = hit$name, download_url = hit$download_url, stringsAsFactors = FALSE)
}

#' All documentation for a repo:
#'   * vignettes:  vignettes/*.Rmd and vignettes/articles/*.Rmd (default branch)
#'   * examples:   examples/*.html (built site on the gh-pages branch)
#' Returns a data frame with columns:
#'   name, download_url, rel (path under the pkgdown homepage), kind ("rmd"/"html").
#' NULL when the repo has neither.
get_articles <- function(org, repo) {
  vign <- rbind(
    list_dir(org, repo, "vignettes", "\\.Rmd$"),
    list_dir(org, repo, "vignettes/articles", "\\.Rmd$")
  )
  ex <- list_dir(org, repo, "examples", "\\.html$", ref = "gh-pages")

  out <- rbind(
    if (nrow(vign) > 0) {
      data.frame(name = vign$name, download_url = vign$download_url,
                 rel = paste0("articles/", article_html(vign$name)),
                 kind = "rmd", stringsAsFactors = FALSE)
    },
    if (nrow(ex) > 0) {
      data.frame(name = ex$name, download_url = ex$download_url,
                 rel = paste0("examples/", ex$name),
                 kind = "html", stringsAsFactors = FALSE)
    }
  )
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out
}

#' Pull a YAML front-matter field ("title" / "description") from vignette lines.
#' Returns a trimmed, unquoted string, or NA when the field is absent, empty, or
#' only a block-scalar marker (`>`, `|`, `>-`, `|+`, ...) whose body lives on
#' following lines we don't read.
get_meta <- function(lines, field) {
  hit <- lines[grep(paste0("^", field, ": "), lines)[1]]
  if (length(hit) == 0 || is.na(hit)) {
    return(NA_character_)
  }
  val <- trimws(gsub('"', "", sub(paste0("^", field, ": "), "", hit)))
  if (!nzchar(val) || grepl("^[>|][+-]?$", val)) {
    return(NA_character_)
  }
  val
}

#' Ensure a homepage URL ends in a slash so "<homepage>articles/..." is valid.
#' Passes through NULL / NA / empty unchanged.
norm_home <- function(h) {
  if (!is.null(h) && !is.na(h) && nzchar(h) && !grepl("/$", h)) paste0(h, "/") else h
}
