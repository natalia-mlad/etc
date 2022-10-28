#' code.converter
#'
#' Have you ever wanted to convert your normal R code into a
#' [targets::tar_target()] pipeline without doing it manually?
#' Well, you're in luck, because that's exactly what this function does.
#' It's probably some of the worst code I've written in my life
#' and I want to improve it one of these days, but honestly? It works!
#'
#' @param script code script path (e.g., "untitled.R")
#' @export
#'
code.converter <- function(script) {
  # TODO: add dot parameters for further functionalities?
  stopifnot(is.character(script))

  # Step 1: extract info. ####
  src <- CodeDepends::readScript(script[1])
  cd <- CodeDepends::getInputs(src)
  libs <- unique(unlist(lapply(cd, slot, name = "libraries"))) # TODO: could the code below be done with purrr instead? & would it be faster?
  #map(cd, "libraries") %>% unlist() %>% unique()

  # Step 2: extract actual code. ####
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
    dplyr::group_by(step) %>%
    dplyr::summarise(n.used = sum(used), n.defined = sum(defined)) %>%
    dplyr::filter(n.defined == 0) %>% dplyr::select(step) %>%
    map( ~ src[.x] %>% paste()) %>% flatten_chr()

  # Step 3: write to files ####
  fs::dir_create("CLEANED")

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

  # Step 4: view the result! ####
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

  # Step 5: Output ####
  output <- list(script, cd, out0, out1, out2, out3, out4)
  return(output)
}


# cc_findFiles ------------------------------------------------------------
#' cc_findFiles
#' Find files that we can use for the code converter function
#' @param path directory path of where to search (optional)
cc_findFiles <- function(path = NULL) {
  if (is.null(path)) path <- fs::path_wd()
  fs::dir_info(path = path, glob = "*.Rmd|.R") %>%
    dplyr::arrange(dplyr::desc(modification_time)) %>%
    dplyr::select(path, size, modification_time) %>%
    as.data.frame()
}
