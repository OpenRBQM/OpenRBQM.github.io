# Test runner for the site's R helpers.
# Run from the project root:  Rscript tests/testthat.R
library(testthat)

# Load the reusable functions from R/.
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

test_dir("tests/testthat", stop_on_failure = TRUE)
