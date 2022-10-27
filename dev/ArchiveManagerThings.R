library(tidyverse)
library(GrpString)
# library(stringdist)


# using it ----------------------------------------------------------------
out <- extract_my_zips(path_home("OneDrive/PDFs/ScienceDirectZips"))
out

###
path <- path_home(
  "OneDrive/PhD Psychology/01 - Papers, Books, Theses/01 - Journals/International Gambling Studies"
)
(out <- extract_my_zips(path))
file_show(path)
out$my_zips$path


##
path <- path_home("OneDrive/PhD Psychology/01 - Papers, Books, Theses/01 - Journals")
(out <- extract_my_zips(path, recurse = TRUE))
path_dir(out$my_zips$name) %>% suna() %>% file_show()
out$my_zips$path



##
path <- path_home(
  "OneDrive/PhD Psychology/01 - Papers, Books, Theses/01 - Journals/00-ScienceDirect"
)
# dir_tree(path)
out <- dir_ls(path, recurse = T, type = "file")
x <- path_file(out) %>% str_which("^Chapter") %>%
  out[.] %>% path_dir() %>% suna()
x

new_path <- path_home("OneDrive/PhD Psychology/01 - Papers, Books, Theses/02 - Books")
walk(x, ~dir_copy(.x, new_path = new_path))

# file_show(new_path)
check <- dir_ls(new_path, type = "directory") %>% path_file()
all(path_file(x) %in% check) #[1] TRUE
dir_delete(x)


dirs2rename <- dir_ls(new_path, type = "directory") %>%
  .[str_which(check, "^ScienceDirect")]
dirs2rename

# TODO: this whole process could be a useful fnc
out <- tibble(dir = dirs2rename) %>%
  mutate(
    files = map(
      dir, ~dir_ls(.x, type = "file") %>% path_file
    ),
    x = map_chr(
      files, ~CommonPatt(.x, 30) %>% slice(1) %>% pull(Pattern)
    ) %>%
      path_ext_remove() %>%
      str_extract("_([:alpha:]|[:punct:])+") %>%
      str_remove_all("_")
  ) %>% filter(!is.na(x))

out

##
# out <- out %>%
#   filter(!is.na(x)) %>%
#   mutate(new_name = paste(new_path, x, sep = "/")) # %>% pull(new_name)
# out$dir; out$new_name
##
# https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/ren
# normalizePath(out$dir[1]) %>%
#   paste0("\"", ., "\"") %>%
#   paste('rename', ., out$x[1]) %>%
#   shell()
# my_cmd <- normalizePath(out$dir[-1]) %>%
#   paste0("\"", ., "\"") %>%
#   paste('rename', ., out$x[-1])
##
my_cmd <- normalizePath(out$dir) %>%
  paste0("\"", ., "\"") %>%
  paste('rename', ., out$x)
my_cmd
walk(my_cmd, shell)

# A duplicate file name exists, or the file cannot be found.
# A duplicate file name exists, or the file cannot be found.
# A duplicate file name exists, or the file cannot be found.
# Warning messages:
# 1: In .f(.x[[i]], ...) :
#   'rename "C:\Users\Natalia\OneDrive\PhD Psychology\01 - Papers, Books, Theses\02 - Books\ScienceDirect_articles_04Jun2021_22-23-13.761" Emerging-Market-Bank-Lending-' execution failed with error code 1
# 2: In .f(.x[[i]], ...) :
#   'rename "C:\Users\Natalia\OneDrive\PhD Psychology\01 - Papers, Books, Theses\02 - Books\ScienceDirect_articles_06Jun2021_14-48-01.795" Emerging-Market-Bank-Lending-' execution failed with error code 1
# 3: In .f(.x[[i]], ...) :
#   'rename "C:\Users\Natalia\OneDrive\PhD Psychology\01 - Papers, Books, Theses\02 - Books\ScienceDirect_articles_18Jul2021_21-29-08.088" Contemporary-Financial-Intermediation' execution failed with error code 1


####

## dev ####
tidyverse::tidyverse_packages()
my_pkgs <- c("purrr", "dplyr", "tibble", "stringr")
originize_file("R/ArchiveManager.R", pkgs = my_pkgs)

## notes ####
CommonPatt(OBJECTS, low = 50) %>%
  slice(1) %>% pull(Pattern)

out$x

"_2016_Contemporary-Financial-Intermediation" %>%
  str_extract("_([:alpha:]|[:punct:])+") %>%
  str_remove_all("_")


# paste(new_path, out$x[1], sep = "/")

# shell(paste('rename',
#             sprintf("content/%s-content", pu_name),
#             sprintf("content/%s", other_name))
# )
# Warning message:
#   In
# file.rename(
#   from = sprintf("content/%s-content", pu_name),
#   to = sprintf("content/%s", other_name)
# )
#,  :
# cannot rename file
# 'content/pu.train2-content' to 'content/train2',
# reason 'Directory not empty

###
# 04Jun2021_22-07-53.446
# 04Jun2021_22-08-35.352
# 04Jun2021_22-08-54.202
#
# 04Jun2021_22-11-19.193
# 04Jun2021_22-13-17.850
# 04Jun2021_22-13-33.217
# 04Jun2021_22-16-06.563
#
# 04Jun2021_22-23-00.258
# 04Jun2021_22-23-13.761
#
# 04Jun2021_22-32-44.112
# 04Jun2021_22-33-36.045
#
# 04Jun2021_22-42-00.327
# 04Jun2021_22-42-32.683
# 04Jun2021_22-43-03.783
#
# 06Jun2021_14-45-02.980
# 06Jun2021_14-48-01.795
# 06Jun2021_14-48-21.813
# 06Jun2021_14-48-24.662
# 06Jun2021_14-48-37.290
# 06Jun2021_14-48-44.110
#
# 15May2021_01-17-05.944
# 15May2021_01-17-13.686
##


###

## [DONT USE!!] string tings ####
# [DOESNT QUITE PERFORM AS EXPECTED] !!

#' Find the common string
#' @param x a character vector that is longer than 1
#' @return the longest common string (a vector of length 1)
#' @export
#'
common_string <- function(x) {
  stopifnot(is.character(x) && length(x) > 1)
  strsplit(x, NULL) %>%
    Reduce(intersect2, .) %>%
    paste(collapse = "")
}

#' @keywords internal
intersect2 <- function(x, y) {
  y <- as.vector(y)
  y[match(as.vector(x), y, 0L)]
}

# example:
common_string(c(
  "abcSOMETHINGCOMMONegf",
  "xSOMETHINGCOMMONyz",
  "SOMETHINGCOMMONnme"
))

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
