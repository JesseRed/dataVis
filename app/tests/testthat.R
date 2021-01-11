library(testthat)
# pathnames <- list.files(pattern="[.]R$", path="functions", full.names=TRUE);
# sapply(pathnames, FUN=source);
# pathnames <- list.files(pattern="[.]R$", path="modules", full.names=TRUE);
# sapply(pathnames, FUN=source);

test_dir(
  "./testthat",
  # Run in the app's environment containing all support methods.
  env = shiny::loadSupport(),
  # Display the regular progress output and throw an error if any test error is found
  reporter = c("progress", "fail")
)
