# Shared site configuration, used by both packages.qmd and vignettes.qmd so the
# two pages can never drift out of sync.

# GitHub orgs to pull packages from.
orgs <- c("openrbqm", "gilead-biostats", "gilead-public")

# Repos that are not packages and should never be listed.
# (Workshop repos are surfaced on the Outreach page instead.)
non_package_repos <- c(
  "openrbqm.github.io", "cluster", "openrbqm",
  "openrbqm-workshop", "openrbqm-workshop-eu25"
)

# Curated package groups, in the order shown on the Packages tab.
core_pkgs       <- c("gsm.core", "workr", "gsm.mapping", "gsm.kri", "gsm.reporting")
ext_pkgs        <- c("gsm.qtl", "gsm.viz", "clindata", "gsm.datasim")
apps_pkgs       <- c("gsm.app", "gsm.digitpref", "gsm.ae", "gsm.query", "gsm.pd")
qual_pkgs       <- c("qcthat", "gsm.qc", "gsm.utils")
deprecated_pkgs <- c("gsm")
additional_pkgs <- c("gh.dash", "open.gismo", "gsm.guide")

# Flat package order for the Vignettes tab (mirrors the Packages tab order).
curated_order <- c(core_pkgs, ext_pkgs, apps_pkgs, qual_pkgs)

# The Vignettes tab additionally drops the deprecated gsm package and gsm.qc.
vignette_exclude <- c(non_package_repos, "gsm", "gsm.qc")
