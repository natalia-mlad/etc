
#' extract_my_zips
#' Used to be called sort_my_zips
#'
#' @param dirpath path
#' @param all logical (default = FALSE). All posible archive formats or just .zip?
#' @param recurse logical (default = FALSE). as used in {fs} pkg functions.
#' If TRUE, recurse fully. If a positive number, the number of levels to recurse.
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' out <- fs::path_wd() %>% extract_my_zips()
#' out$zips_to_check #fix these
#' out$problematic_extractions$name #check these
#' fs::file_delete(out$my_zips$path) #clean Up
#' }
#'
extract_my_zips <- function(dirpath, all = FALSE, recurse = FALSE) {
  # require(tidyverse); require(archive); require(fs)
  # wd <- fs::path_wd(); dirpath <- fs::path_wd()
  # dirpath <- path_home("OneDrive/PhD Psychology/01 - R Project")

  # TODO: add commentary re: # extracted etc.
  dirpath <- fs::path_real(dirpath)
  stopifnot(fs::dir_exists(dirpath))

  if(isTRUE(all)){
    my_extensions <- fs::dir_ls(
      path = dirpath,
      type = "file",
      recurse = recurse,
      all = TRUE
    ) %>%
      fs::path_ext() %>%
      unique() %>%
      sort()
    extensions_used <- all_zip_extensions %>%
      paste0("^", ., "$") %>%
      purrr::map(subset_my_stringr) %>%
      purrr::flatten_chr() %>%
      paste0("*.", .) %>%
      paste(., collapse = "|")
  } else {
    extensions_used <- "*.zip"
  }

  stopifnot(
    length(fs::dir_ls(path = dirpath, glob = extensions_used, recurse = recurse)) > 0
  )

  ## 1
  # table of zip file info:
  my_zips <- fs::dir_info(path = dirpath, glob = extensions_used, recurse = recurse) %>%
    janitor::remove_constant(quiet = T) %>% # rename(filename = path) %>%
    mutate(name = fs::path_ext_remove(path), .after = path)

  # check for existing dirs:
  existing_dir <- fs::dir_ls(path = dirpath, type = "directory", recurse = recurse)
  zips_to_check <- my_zips %>%
    filter(name %in% existing_dir) %>%
    pull(path)

  ## 2
  my_zips <- my_zips %>%
    filter(!(name %in% existing_dir)) %>%
    # mutate(my_dir = fs::path_wd(name), .after = name) %>%
    mutate(zip_contents = map(path, ~archive::archive(.x) %>% filter(size != 0)),
           .after = name) %>%
    mutate(n_zip = map_int(zip_contents, nrow),
           .after = zip_contents)

  # Create the directory that corresponds to the zip name:
  fs::dir_create(my_zips$name)
  # fs::dir_create(my_zips$my_dir)

  ## 3
  try({
    map2(my_zips$path,
         my_zips$name,
         ~ archive::archive_extract(.x, dir = .y))
  })

  ## Checks
  my_zips <- my_zips %>%
    mutate(extracted_contents = name %>%
             map(~ dir_info(.x, type = "file", all = T,
                            recurse = T, fail = F)),
           .after = n_zip) %>%
    mutate(n_extracted = map_int(extracted_contents, nrow),
           .after = extracted_contents)
  # dir_ls(my_dir, recurse = T, all = T) %>% path_file()
  #> [1] "code.R"     "data.Rdata" "__MACOSX"
  problematic_extractions <- my_zips %>% filter(n_extracted != n_zip)
  my_zips <- my_zips %>% filter(n_extracted == n_zip)

  ## return
  out <- lst(
    extensions_used,
    zips_to_check,
    problematic_extractions,
    my_zips
  )
  return(out)
}


#' @keywords internal
subset_my_stringr <- function(x) {
  stringr::str_subset(my_extensions, stringr::regex(x, ignore_case = T))
}

