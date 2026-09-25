# Shared site configuration, used by both packages.qmd and vignettes.qmd so the
# two pages can never drift out of sync.

# GitHub orgs to pull packages from.
orgs <- c("openrbqm", "gilead-biostats", "gilead-public", "impala-consortium")

# Repos that are not packages and should never be listed.
# (Workshop repos are surfaced on the Outreach page instead.)
# (ctas/ctasapp/ctasval are IMPALA-Consortium anomaly-detection tools, not gsm packages.)
non_package_repos <- c(
  "openrbqm.github.io", "cluster", "openrbqm",
  "openrbqm-workshop", "openrbqm-workshop-eu25",
  "ctas", "ctasapp", "ctasval"
)

# Curated package groups, in the order shown on the Packages tab.
core_pkgs       <- c("gsm.core", "workr", "gsm.mapping", "gsm.kri", "gsm.reporting")
ext_pkgs        <- c(
  "gsm.qtl", "gsm.viz", "clindata", "gsm.datasim",
  "gsm.timez", "gsm.studykri", "gsm.simaerep", "gsm.simaerep.viz"
)
apps_pkgs       <- c("gsm.app", "gsm.digitpref", "gsm.ae", "gsm.query", "gsm.pd")
qual_pkgs       <- c("qcthat", "gsm.qc", "gsm.utils")
deprecated_pkgs <- c("gsm")
additional_pkgs <- c("gh.dash", "open.gismo", "gsm.guide")

# Fallback descriptions for repos with no "description" set on GitHub, taken
# from each package's own DESCRIPTION/README.
description_overrides <- c(
  gsm.timez = "GSM extension for longitudinal site monitoring; applies funnel plot scoring across sequential months to detect sites with unusual cumulative event trajectories.",
  gsm.studykri = "Bootstraps study-level KRI confidence intervals to compare a study's KRIs against a fixed expectation or against one or more reference studies.",
  gsm.simaerep.viz = "JavaScript visualization library for simaerep clinical trial monitoring data, used as an htmlwidget dependency by gsm.simaerep."
)


# Flat package order for the Vignettes tab (mirrors the Packages tab order).
curated_order <- c(core_pkgs, ext_pkgs, apps_pkgs, qual_pkgs, additional_pkgs)

# The Vignettes tab additionally drops the deprecated gsm package and gsm.qc.
vignette_exclude <- c(non_package_repos, "gsm", "gsm.qc")
