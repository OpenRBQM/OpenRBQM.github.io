# Package-card rendering for packages.qmd.
# All functions RETURN markdown strings (no cat) so they can be unit-tested.

library(dplyr)
library(purrr)
library(glue)

#' Fetch and clean the package table for the given orgs.
#' Drops `exclude`d repos, applies `overrides` (named by repo name, case-insensitive)
#' for repos missing a GitHub description, then fills any still-missing with a placeholder.
get_package_table <- function(orgs, exclude = character(), overrides = character()) {
  packages <- orgs %>% purrr::map(get_repos) %>% dplyr::bind_rows()
  match_idx <- match(tolower(packages$name), tolower(names(overrides)))
  has_override <- is.na(packages$description) & !is.na(match_idx)
  packages$description[has_override] <- overrides[match_idx[has_override]]
  packages$description[is.na(packages$description)] <- "No Description Provided"
  packages %>% dplyr::filter(!tolower(name) %in% tolower(exclude))
}

#' Render a data frame of packages as a responsive grid of cards.
#' Each card: name -> GitHub source; a Website link when a homepage exists; the
#' description. Returns a markdown string (empty string for an empty data frame).
render_cards <- function(df) {
  if (nrow(df) == 0) return("")
  website <- ifelse(
    !is.na(df$homepage) & nzchar(df$homepage),
    glue::glue("[Website]({df$homepage})"),
    ""
  )
  cards <- glue::glue(
    "::: {{.g-col-12 .g-col-md-6 .g-col-lg-4}}\n",
    "::: {{.card .h-100 .nav-card}}\n",
    "::: {{.card-body}}\n",
    "#### [{df$name}]({df$html_url})\n\n",
    "{df$description}\n\n",
    "{website}\n\n",
    ":::\n:::\n:::"
  )
  paste0("\n::: {.grid}\n\n", paste(cards, collapse = "\n\n"), "\n\n:::\n\n")
}

#' Render a "## {heading}" section with cards for `pkg_names`, in that order.
#' Returns "" when none of the packages are present.
render_section <- function(packages, heading, pkg_names) {
  sub <- packages %>%
    dplyr::filter(tolower(name) %in% tolower(pkg_names)) %>%
    dplyr::arrange(match(tolower(name), tolower(pkg_names)))
  if (nrow(sub) == 0) return("")
  paste0("\n## ", heading, "\n", render_cards(sub))
}

#' Package names not in `used` (case-insensitive) — used for "Additional Packages".
remaining_packages <- function(packages, used) {
  packages$name[!tolower(packages$name) %in% tolower(used)]
}
