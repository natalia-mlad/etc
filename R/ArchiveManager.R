
#' sort_my_zips
#'
#' @param all all posible archive formats or just .zip?
#' @param recurse used in {fs} pkg functions. If TRUE, recurse fully.
#' If a positive number, the number of levels to recurse.
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' out <- sort_my_zips()
#' out$zips_to_check #fix these
#' out$problematic_extractions$name #check these
#' fs::file_delete(out$my_zips$filename) #clean Up
#' }
#'
sort_my_zips <- function(all = FALSE, recurse = FALSE) {
  # require(tidyverse)
  # require(archive)
  # require(fs)
  #wd <- fs::path_wd()
  #dirpath <- fs::path_wd()
  if(isTRUE(all)){
    my_extensions <- #dirpath %>%
      fs::dir_ls(type = "file", recurse = recurse, all = TRUE) %>%
      fs::path_ext() %>%
      unique() %>%
      sort()
    subset_my_stringr <- function(x)
      stringr::str_subset(my_extensions, stringr::regex(x, ignore_case = T))
    extensions_used <- all_zip_extensions %>%
      paste0("^", ., "$") %>%
      purrr::map(subset_my_stringr) %>%
      purrr::flatten_chr() %>%
      paste0("*.", .) %>%
      paste(., collapse = "|")
  } else {
    extensions_used <- "*.zip"
  }

  ## 1
  existing_dir <- fs::dir_ls(type = "directory")
  my_zips <- fs::dir_info(glob = extensions_used) %>%
    janitor::remove_constant(quiet = T) %>%
    rename(filename = path) %>%
    mutate(name = fs::path_ext_remove(filename), .after = filename)
  zips_to_check <- my_zips %>%
    filter(name %in% existing_dir) %>%
    pull(filename)

  ## 2
  my_zips <- my_zips %>%
    filter(!(name %in% existing_dir)) %>%
    mutate(my_dir = fs::path_wd(name), .after = name) %>% #TODO
    mutate(zip_contents = filename %>%
             map(~ archive::archive(.x) %>%
                   filter(size != 0)),
           .after = my_dir) %>%
    mutate(n_zip = map_int(zip_contents, nrow),
           .after = zip_contents)

  fs::dir_create(my_zips$my_dir)

  ## 3
  try({
    map2(my_zips$filename,
         my_zips$my_dir,
         ~ archive::archive_extract(.x, dir = .y))
  })

  ## Checks
  my_zips <- my_zips %>%
    mutate(extracted_contents = my_dir %>%
             map(~ dir_info(.x, type = "file", all = T,
                            recurse = T, fail = F)),
           .after = n_zip) %>%
    mutate(n_extracted = map_int(extracted_contents, nrow),
           .after = extracted_contents)
  #dir_ls(my_dir, recurse = T, all = T) %>% path_file()
  #> [1] "code.R"     "data.Rdata" "__MACOSX"
  problematic_extractions <- my_zips %>%
    filter(n_extracted != n_zip)
  my_zips <- my_zips %>%
    filter(n_extracted == n_zip)

  ## return
  out <- list(
    extensions_used = extensions_used,
    zips_to_check = zips_to_check,
    problematic_extractions = problematic_extractions,
    my_zips = my_zips
  )
  return(out)
}


# notes -------------------------------------------------------------------
# library(fs) #fs::is_file_empty()
# library(tidyverse)
# library(archive)
#
# ## 1
# existing_dir <- dir_info(type = "directory")$path
# my_zips <- dir_info(glob = "*.zip") %>%
#   janitor::remove_constant(quiet = F) %>%
#   rename(filename = path) %>%
#   mutate(name = path_ext_remove(filename), .after = filename)
# zips_to_check <- my_zips %>%
#   filter(name %in% existing_dir) %>%
#   pull(filename)
# zips_to_check
#
# ## 2
# my_zips <- my_zips %>%
#   filter(!(name %in% existing_dir)) %>%
#   mutate(my_dir = path_wd(name), .after = name) %>%
#   mutate(zip_contents = filename %>%
#            map(~ archive(.x) %>%
#                  filter(size != 0)),
#          .after = my_dir) %>%
#   mutate(n_zip = map_int(zip_contents, nrow),
#          .after = zip_contents)
#
# dir_create(my_zips$my_dir)
#
# ## 3
# map2(my_zips$filename,
#      my_zips$my_dir,
#      ~ archive_extract(.x, dir = .y))
# #, files = c("iris.csv", "mtcars.csv")
#
# ## Error
# #Error: archive_extract.cpp:188 archive_write_header():
# #Can't create '\\?\C:\Users\Natalia\OneDrive\PhD Psychology\01 - R Project\ds-master\ds-master\website\static\assignments'
# #ds-master
# 30:33
# nrow(my_zips)
# map2(my_zips$filename[34:182],
#      my_zips$my_dir[34:182],
#      ~ archive_extract(.x, dir = .y))
#
# # 11 datascience-box-master                      1  2792        2791
# # 12 rbook-master                                3  2868        2865
# # 13 DSM_fixedMargin                            14    14           0
# # 14 ds-master                                 109  3331        3222
# # 15 PsycheReviewApp-master                   2700  2727          27
#
# try({
#   map2(my_zips$filename[125:182],
#        my_zips$my_dir[125:182],
#        ~ archive_extract(.x, dir = .y))
# })
# #PsycheReviewApp-master #124
# # '\\?\C:\'
# # > my_zips$filename[31]
# # dsbox-master.zip
# # Error: archive_extract.cpp:188 archive_write_header(): Can't create '\\?\C:\'
#
# ## Checks:
# # to compare against archive(filename)
# my_zips <- my_zips %>%
#   mutate(extracted_contents = my_dir %>%
#            map(~ dir_info(.x, type = "file", all = T,
#                           recurse = T, fail = F)),
#          .after = n_zip) %>%
#   mutate(n_extracted = map_int(extracted_contents, nrow),
#          .after = extracted_contents)
# #dir_ls(my_dir, recurse = T, all = T) %>% path_file()
# #> [1] "code.R"     "data.Rdata" "__MACOSX"
# problematic_extractions <- my_zips %>%
#   filter(n_extracted != n_zip)
# my_zips <- my_zips %>%
#   filter(n_extracted == n_zip)
#
#
# ## Clean-Up:
# file_delete(my_zips$filename)
#
#
# ###
# # dir_info("alr", all = T, recurse = T, fail = F) %>%
# #   select(path, size)
# ##
# x = problematic_extractions %>%
#   unnest_longer(col = c(zip_contents)) %>%
#   select(name, n_zip, zip_contents) %>%
#   unnest(cols = c(zip_contents)) %>%
#   mutate(path = paste0(path_wd(), "/", name, "/", path))
# y = problematic_extractions %>%
#   unnest_longer(col = c(extracted_contents)) %>%
#   select(name, n_extracted, extracted_contents) %>%
#   unnest(cols = c(extracted_contents)) %>%
#   janitor::remove_constant(quiet = F)
# z = full_join(x, y, by = c("name", "path"))
#
# ###
# View(z)
#
# z %>%
#   group_by(name) %>%
#   summarise(
#     size1 = sum(fs::as_fs_bytes(size.x), na.rm = T),
#     size2 = sum(size.y, na.rm = T)
#   ) %>%
#   mutate(size = size1 - size2) %>%
#   arrange(size)
#
# problematic_extractions %>%
#   mutate(n = n_zip - n_extracted) %>%
#   select(name, n, n_zip, n_extracted) %>%
#   arrange(n)
#
# ##
# path_home("Desktop/blog-master") %>%
#   dir_info(all = T, recurse = T, fail = F) %>%
#   select(path, size) %>%
#   mutate(path = gsub("C:/Users/Natalia/Desktop/blog-master/", "", path))
#
# problematic_extractions %>%
#   filter(name == "blog-master") %>%
#   select(name, n_zip)
#
# problematic_extractions %>%
#   filter(name == "osfstorage-archive (12)") %>%
#   select(zip_contents) %>%
#   unnest(cols = c(zip_contents))
#
# z %>%
#   filter(name == "osfstorage-archive (12)") %>%
#   filter(is.na(size.x)) %>%
#   pull(path)
# #StackedEnsemble_AllModels_AutoML_20210822_151505/models/GLM/
# #metalearner_AUTO_StackedEnsemble_AllModels_AutoML_20210822_151505/
# #experimental
# ##
# # archive_extract(
# #   archive,
# #   dir = ".",
# #   files = NULL,
# #   options = character(),
# #   strip_components = 0L
# # )
#
