#' cc_findFiles
#'
#' @param path path
#'
cc_findFiles <- function(path = NuLL) {
  if (is.null(path)) {
    path <- fs::path_wd()
  }

  fs::dir_info(path = path, glob = "*.Rmd|.R") %>%
    arrange(desc(modification_time)) %>%
    select(path, size, modification_time) %>%
    as.data.frame()
}

#' code.converter
#'
#' @param script_path script_path
#' @param ... dot
#'
#' @export
#'
code.converter <- function(script_path, ...) {
  stopifnot(is.character(script_path))

# packages:
  # require(fs)
  # require(tidyverse)
  # require(CodeDepends)
  # functions:
  # `%notin%` <- Negate(`%in%`)

# Step 1: specify code path. ####
  #script <- "untitled.R"
  script <- script_path

  # Step 2: extract info. ####
  src <- CodeDepends::readScript(script[1])
  cd <- CodeDepends::getInputs(src)
  libs <- unique(unlist(lapply(cd, slot, name = "libraries"))) # TODO: could the code below be done with purrr instead? & would it be faster?
  #map(cd, "libraries") %>% unlist() %>% unique()

  # Step 3: extract actual code. ####
  code <- CodeDepends::getDetailedTimelines(info = cd) %>%
    split(.$var) %>%
    map( ~ .x[which(.x$defined == TRUE), ]) %>%
    map( ~ c(
      paste0("# ", as.character(unique(.x$var)), " ####"),
      paste0("tar_target(", as.character(unique(.x$var)), ",", " {"),
      src[.x[, 1]] %>% paste(),
      paste0("return(", as.character(unique(.x$var)), ")", " }"),
      paste0("),")
    )) %>%
    flatten_chr()

  raw <- readLines(script)
  comments <- str_subset(raw, "#")
  report1 <- raw[!(raw %in% code)]
  report2 <- CodeDepends::getDetailedTimelines(info = cd) %>%
    group_by(step) %>%
    summarise(n.used = sum(used), n.defined = sum(defined)) %>%
    filter(n.defined == 0) %>% select(step) %>%
    map( ~ src[.x] %>% paste()) %>% flatten_chr()

  # Step 4: write to files ####
  dir_create("CLEANED")

  fileConn <- file(paste0("CLEANED/CLEANED-", script))
  writeLines(
    c("library(targets)",
      "library(tarchetypes)",
      "library(assertr)",
      "",
      "#---- Set Up ----",
      "# functions:",
      paste0('source(','\"','R/functions.R','\"',')'),
      "",
      "# options:",
      "options(scipen = 999)",
      "",
      "# packages:",
      paste0("list_of_packages <- c(", paste0(paste0('\"', paste0(libs, collapse = "\",\"")), '\"'), ")"),
      "tar_option_set(packages = list_of_packages)",
      #"new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]",
      #"list_of_packages[(list_of_packages %notin% installed.packages()[, \"Package\"])] %>% install.packages()",
      "",
      "#---- Pipeline ----",
      "list(", code,
      "# report.Rmd ####",
      "tar_render(report, \"report.Rmd\")",
      ")"
    ), fileConn
  )
  close(fileConn)

  fileConn <- file(paste0("CLEANED/REPORT-", script))
  writeLines(
    c(
      "---",
      "title: report",
      "output: html_document",
      "---",
      "library(targets)",
      "library(tarchetypes)",
      "tar_load(tar_objects())",
      report1,
      "#---- part 2 ----",
      #comments,
      report2
    ),
    fileConn
  )
  close(fileConn)
  knitr::spin(paste0("CLEANED/REPORT-", script), knit = FALSE)
  fs::file_delete(paste0("CLEANED/REPORT-", script))

  # Step 5: view the result! ####
  # file_show(paste0("CLEANED/CLEANED-", script))
  out0 <- CodeDepends::getDetailedTimelines(info = cd)
  out1 <- paste0("CLEANED/CLEANED-", script)

# BONUS INFO ####
  out2 <- unlist(lapply(cd, slot, name = "removes")) %>% unique()

  out3 <- CodeDepends::getDetailedTimelines(info = cd) %>% split(.$var) %>%
    map( ~ .x[which(.x$used == T & .x$defined == F), ]) %>%
    map( ~ src[.x[, 1]] %>% paste()) #%>% View()

  out4 <- names(unlist(lapply(cd, slot, name = "functions"))) %>% unique()
  unlist(lapply(cd, slot, name = "functions")) %>%
    map(~ which(.x == TRUE)) %>%
    unlist() %>% names() %>% unique()

# Step 6: Output ####
  output <- list(script, cd, out0, out1, out2, out3, out4)
  return(output)
}
