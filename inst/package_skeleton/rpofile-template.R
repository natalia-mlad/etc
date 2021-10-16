if (interactive()) {
  suppressMessages(require(magrittr))
  suppressMessages(require(devtools))
  suppressMessages(require(usethis))
}

options(
  scipen = 9999,
  crayon.enabled = TRUE,
  mc.cores = parallel::detectCores(logical = F) - 1
)
