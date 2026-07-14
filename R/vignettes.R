# Dynamic vignette listing for vignettes.qmd.
# `vignette_markdown` is pure (testable); `render_vignettes` orchestrates the
# GitHub fetches and delegates formatting to it.

library(dplyr)
library(purrr)

#' Order package names so `curated` come first (in curated order), then the rest.
order_packages <- function(names, curated) {
  names_lc   <- tolower(names)
  curated_lc <- tolower(curated)
  c(names[match(intersect(curated_lc, names_lc), names_lc)],
    names[!names_lc %in% curated_lc])
}

#' Build markdown from a list of sections.
#' Each section: list(repo = "gsm.core", articles = data.frame(title, url, description)).
#' Sections with no articles are skipped; NA/blank descriptions are omitted.
vignette_markdown <- function(sections) {
  blocks <- purrr::map_chr(sections, function(s) {
    arts <- s$articles
    if (is.null(arts) || nrow(arts) == 0) return("")
    lines <- purrr::pmap_chr(arts, function(title, url, description, ...) {
      if (!is.na(description) && nzchar(description) &&
          !description %in% c("NA", "<>", ">") &&
          !grepl("^<<.*>>$", trimws(description))) {
        paste0("- [", title, "](", url, ") — ", description)
      } else {
        paste0("- [", title, "](", url, ")")
      }
    })
    paste0("\n## ", s$repo, "\n\n", paste(lines, collapse = "\n"), "\n")
  })
  paste(blocks[nzchar(blocks)], collapse = "\n")
}

#' Fetch every package's documentation and return markdown, grouped by package
#' and ordered like the Packages tab. Vignettes are listed per package first;
#' all fetched examples follow in a single "# Examples" section at the bottom.
#' `exclude` drops repos entirely; packages with nothing to show are omitted.
render_vignettes <- function(orgs, exclude = character(), curated = character()) {
  packages <- get_package_table(orgs, exclude)

  ordered <- order_packages(packages$name, curated)

  vignette_sections <- list()
  example_sections  <- list()

  for (repo in ordered) {
    row  <- packages[packages$name == repo, ][1, ]
    home <- norm_home(row$homepage)
    # Without a pkgdown homepage we can't build valid article/example links.
    if (is.null(home) || is.na(home) || !nzchar(home)) next

    org  <- strsplit(row$full_name, "/")[[1]][1]
    arts <- get_articles(org, repo)
    if (is.null(arts) || nrow(arts) == 0) next
    # Read only the head of each file — enough for the YAML block (.Rmd) or the
    # <title> in <head> (.html) — so we don't download large built reports whole.
    contents <- purrr::map(arts$download_url, function(u) {
      tryCatch(readLines(u, n = 1000, warn = FALSE), error = function(e) character(0))
    })
    titles <- character(nrow(arts))
    descs  <- character(nrow(arts))
    for (i in seq_len(nrow(arts))) {
      if (arts$kind[i] == "rmd") {
        titles[i] <- get_meta(contents[[i]], "title")
        descs[i]  <- get_meta(contents[[i]], "description")
      } else {
        titles[i] <- html_title(contents[[i]])
        descs[i]  <- NA_character_
      }
    }
    titles[is.na(titles)] <- tools::file_path_sans_ext(arts$name[is.na(titles)])

    df <- data.frame(
      title = titles,
      url = paste0(home, arts$rel),
      description = descs,
      kind = arts$kind,
      stringsAsFactors = FALSE
    )
    vdf <- df[df$kind == "rmd", c("title", "url", "description")]
    edf <- df[df$kind == "html", c("title", "url", "description")]
    if (nrow(vdf) > 0) {
      vignette_sections[[length(vignette_sections) + 1]] <- list(repo = repo, articles = vdf)
    }
    if (nrow(edf) > 0) {
      example_sections[[length(example_sections) + 1]] <- list(repo = repo, articles = edf)
    }
  }

  md <- vignette_markdown(vignette_sections)
  if (length(example_sections) > 0) {
    md <- paste0(md, "\n\n# Examples\n", vignette_markdown(example_sections))
  }
  md
}
