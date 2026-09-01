# News tab update logic.
# Fetches releases for all non-deprecated packages and inserts any that are not
# already listed in news.qmd. Called by the news_update GHA workflow.

library(httr)
library(jsonlite)

#' Extract a one-line description from a GitHub release body.
#' Strips markdown noise (headers, bullets, Full Changelog lines, HTML comments),
#' then cuts at the first sentence boundary or word boundary at 160 chars.
#' Returns NA_character_ when no meaningful text can be found.
extract_release_desc <- function(body) {
  if (is.null(body) || is.na(body) || !nzchar(trimws(body))) return(NA_character_)
  lines <- strsplit(body, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- grep("^(#+|---|\\*\\*Full Changelog|<!--)", lines,
                value = TRUE, invert = TRUE)
  lines <- gsub("^[#*>-]+\\s*", "", lines)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(NA_character_)
  s <- trimws(lines[1])
  # Cut at first sentence boundary (requires capital letter or end-of-string
  # after the punctuation to avoid cutting at decimals/version numbers like v1.0).
  # Fall back to word boundary at 160 chars.
  m <- regexpr("[.!?](\\s+[A-Z]|\\s*$)", s, perl = TRUE)
  if (m > 0) {
    s <- trimws(substr(s, 1, m))
  } else if (nchar(s) > 160) {
    s <- paste0(sub("\\s+\\S+$", "", substr(s, 1, 160)), "...")
  }
  s
}

#' Quarter label for a Date, e.g. "Q2 2026".
quarter_label <- function(d) {
  paste0("Q", ceiling(as.integer(format(d, "%m")) / 3), " ", format(d, "%Y"))
}

#' Drop the org from a GitHub URL so releases stay identifiable across org moves.
strip_org <- function(u) sub("^https://github\\.com/[^/]+/", "", u)

#' Insert a data frame of new releases into existing news.qmd lines.
#' new_rels must have columns: pkg, tag, url, date (Date), desc (character).
#' Processes oldest-to-newest so each insertion lands in the correct position.
#' Returns the updated character vector of lines.
insert_releases <- function(lines, new_rels) {
  if (is.null(new_rels) || nrow(new_rels) == 0) return(lines)
  new_rels <- new_rels[order(new_rels$date), ]

  for (i in seq_len(nrow(new_rels))) {
    r      <- new_rels[i, ]
    q      <- quarter_label(r$date)
    header <- paste0("## Releases ", q)
    bullet <- paste0("- [", r$pkg, " ", r$tag, "](", r$url, ") — ",
                     format(r$date, "%Y-%m-%d"))
    if (!is.na(r$desc) && nzchar(r$desc))
      bullet <- paste0(bullet, " — ", r$desc)

    hpos <- which(lines == header)

    if (length(hpos) > 0) {
      # Insert after the header line, skipping all consecutive blank lines
      ins <- hpos[1] + 1L
      while (ins <= length(lines) && !nzchar(trimws(lines[ins]))) ins <- ins + 1L
      tail <- if (ins <= length(lines)) lines[ins:length(lines)] else character(0)
      lines <- c(lines[seq_len(ins - 1L)], bullet, tail)
    } else {
      # Find the first existing header that is older than this quarter
      header_pattern <- "^## Releases Q[1-4] [0-9]{4}$"
      hlines <- which(grepl(header_pattern, lines))
      insert_before <- NA_integer_

      for (h in hlines) {
        hq   <- sub("^## Releases ", "", lines[h])
        hyr  <- as.integer(sub("Q[1-4] ", "", hq))
        hqtr <- as.integer(sub("Q([1-4]).*", "\\1", hq))
        ryr  <- as.integer(format(r$date, "%Y"))
        rqtr <- ceiling(as.integer(format(r$date, "%m")) / 3)
        if (hyr < ryr || (hyr == ryr && hqtr < rqtr)) {
          insert_before <- h
          break
        }
      }

      new_section <- c(header, "", bullet, "")
      if (is.na(insert_before)) {
        lines <- c(lines, new_section)
      } else {
        lines <- c(lines[seq_len(insert_before - 1L)],
                   new_section,
                   lines[insert_before:length(lines)])
      }
    }
  }
  lines
}

#' Fetch all non-draft, non-prerelease, semver-tagged releases for a package
#' since 2025-01-01. Returns a data frame (pkg, tag, url, date, desc) or NULL.
fetch_releases <- function(org, pkg) {
  url  <- paste0("https://api.github.com/repos/", org, "/", pkg,
                 "/releases?per_page=100")
  resp <- gh_get(url)
  if (httr::status_code(resp) != 200) return(NULL)
  rels <- jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)
  if (!is.data.frame(rels) || nrow(rels) == 0) return(NULL)
  rels <- rels[!rels$draft & !rels$prerelease, ]
  rels <- rels[grepl("^v?[0-9]+\\.[0-9]", rels$tag_name), ]
  if (nrow(rels) == 0) return(NULL)
  rels$published_at <- as.POSIXct(rels$published_at,
                                   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rels <- rels[format(rels$published_at, "%Y") >= "2025", ]
  if (nrow(rels) == 0) return(NULL)
  desc <- sapply(rels$body, extract_release_desc, USE.NAMES = FALSE)
  data.frame(pkg = pkg, tag = rels$tag_name, url = rels$html_url,
             date = as.Date(rels$published_at), desc = desc,
             stringsAsFactors = FALSE)
}

#' Update news.qmd with any releases not already listed.
#' Returns the number of releases added (0 means no changes).
update_news <- function(path = "news.qmd") {
  source("R/config.R")
  source("R/github.R")

  all_pkgs <- c(core_pkgs, ext_pkgs, apps_pkgs, qual_pkgs, additional_pkgs)

  repo_lookup <- do.call(rbind, lapply(orgs, function(org) {
    tryCatch({
      df <- get_repos(org)
      data.frame(name = df$name, org = org, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  }))

  if (is.null(repo_lookup) || nrow(repo_lookup) == 0) {
    message("Could not retrieve any repositories (API outage or rate limit).")
    return(0L)
  }

  all_releases <- do.call(rbind, lapply(all_pkgs, function(pkg) {
    row <- repo_lookup[tolower(repo_lookup$name) == tolower(pkg), ]
    if (nrow(row) == 0) return(NULL)
    fetch_releases(row$org[1], pkg)
  }))

  if (is.null(all_releases) || nrow(all_releases) == 0) {
    message("No releases found.")
    return(0L)
  }

  existing    <- readLines(path)
  listed_urls <- unique(unlist(regmatches(existing,
    gregexpr("https://github\\.com/[^)]+/releases/tag/[^)]+", existing))))
  new_rels    <- all_releases[!strip_org(all_releases$url) %in% strip_org(listed_urls), ]

  if (nrow(new_rels) == 0) {
    message("No new releases.")
    return(0L)
  }

  lines <- insert_releases(existing, new_rels)
  writeLines(lines, path)
  added <- nrow(new_rels)
  message("Added ", added, " new release(s) to ", path)
  added
}

