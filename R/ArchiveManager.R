#' extract_my_zips
#'
#' Used to be called sort_my_zips
#'
#' Sometimes I end up accumilating a large collection of zip files
#' and I just want to quickly extract all of them, so (naturally)
#' I automated this process in R.
#'
#' @param dirpath directory path
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
    janitor::remove_constant(quiet = T) %>% # dplyr::rename(filename = path) %>%
    dplyr::mutate(name = fs::path_ext_remove(path), .after = path)

  # check for existing dirs:
  existing_dir <- fs::dir_ls(path = dirpath, type = "directory", recurse = recurse)
  zips_to_check <- my_zips %>%
    dplyr::filter(name %in% existing_dir) %>%
    dplyr::pull(path)

  ## 2
  my_zips <- my_zips %>%
    dplyr::filter(!(name %in% existing_dir)) %>%
    # mutate(my_dir = fs::path_wd(name), .after = name) %>%
    dplyr::mutate(zip_contents = purrr::map(path, ~archive::archive(.x) %>% dplyr::filter(size != 0)),
           .after = name) %>%
    dplyr::mutate(n_zip = purrr::map_int(zip_contents, nrow),
           .after = zip_contents)

  # Create the directory that corresponds to the zip name:
  fs::dir_create(my_zips$name)
  # fs::dir_create(my_zips$my_dir)

  ## 3
  try({
    purrr::map2(my_zips$path,
         my_zips$name,
         ~ archive::archive_extract(.x, dir = .y))
  })

  ## Checks
  my_zips <- my_zips %>%
    dplyr::mutate(extracted_contents = name %>%
             purrr::map(~ fs::dir_info(.x, type = "file", all = T,
                            recurse = T, fail = F)),
           .after = n_zip) %>%
    dplyr::mutate(n_extracted = purrr::map_int(extracted_contents, nrow),
           .after = extracted_contents)
  # dir_ls(my_dir, recurse = T, all = T) %>% path_file()
  #> [1] "code.R"     "data.Rdata" "__MACOSX"
  problematic_extractions <- my_zips %>% dplyr::filter(n_extracted != n_zip)
  my_zips <- my_zips %>% dplyr::filter(n_extracted == n_zip)

  ## return
  out <- dplyr::lst(
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

