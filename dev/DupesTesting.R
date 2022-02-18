# Cntrl+Shift+D
# Cntrl+Shift+B
library(fs); library(digest); library(tidyverse)
###
my_dir <- path_home("OneDrive/TOSORT/Downloads")
(out <- identify_duplicates(my_dir, glob = "*.pdf"))

summarise(out, n = n() - 1) %>% pull(n) %>% sum()
# 143


####
out <- identify_duplicates(my_dir, glob = "*.pdf")
out <- identify_duplicates(my_dir, glob = "*.doc")
out <- identify_duplicates(my_dir, glob = "*.docx")
out <- identify_duplicates(my_dir, glob = "*.mpg")
out <- identify_duplicates(my_dir, glob = "*.ppt")
out <- identify_duplicates(my_dir, glob = "*.pptx")
out <- identify_duplicates(my_dir, glob = "*.tmp")
out <- identify_duplicates(my_dir, glob = "*.xls")
out <- identify_duplicates(my_dir, glob = "*.xlsx")
out <- identify_duplicates(my_dir, glob = "*.zip")

143 + 25 + 17 + 32 + 2 + 13 + 2 + 1 + 5 + 1 #241

##
path_home("OneDrive") %>% dir_ls(glob = "*.csv")
dupe_guru <- path_home("OneDrive/dupeGuru-TOSORT-results.csv") %>%
  read_csv()
dupe_guru
#`Group ID` Filename              Folder `Size (KB)` Kind  Modification `Match %` `Words Used`
#     <dbl> <chr>                 <chr>        <dbl> <chr> <chr>            <dbl> <lgl>
#         0 (Advances in Creativ~ "C:\\~        3683 pdf   2017/04/02 ~       100 NA
#         0 (Advances in Creativ~ "C:\\~        3683 pdf   2017/04/04 ~       100 NA
#         1 05.pdf                "C:\\~         467 pdf   2017/03/12 ~       100 NA
#         1 05 (1).pdf            "C:\\~         467 pdf   2017/03/12 ~       100 NA
#         2 1105-18-Emotion.pdf   "C:\\~         380 pdf   2014/04/25 ~       100 NA
out <- identify_duplicates(my_dir, glob = "*.pdf")

out %>% filter(!name %in% dupe_guru$Filename) %>% pull(name)
dupe_guru %>% filter(!Filename %in% out$name) #0
# [1] "Diana_Whitney,_Amanda_Trosten-Bloom,_David_Cooperrider_The_Power_of_Appreciative_Inquiry_A_Practical_Guide_to_Positive_Change.pdf"
# [2] "Imaging the thinking brain2013 [Read-Only] (2).pdf"
# [3] "Imaging the thinking brain2013 [Read-Only].pdf"

##
#### Debugging ####

## SetUp:
glob <- "*.pdf"
algo <- "xxhash64"
excluded <- paste0(paste(LETTERS, collapse = ""), paste(1:10, collapse = ""))
#
(my_dir <- path_home("OneDrive/TOSORT/Downloads"))
(my_dir <- fs::path_abs(my_dir))

##
filelist <- my_dir %>%
  fs::dir_info(glob = glob, all = T, recurse = T, fail = F) %>%
  dplyr::filter(stringr::str_detect(path, excluded, negate = T)) %>%
  dplyr::filter(nchar(path) < 259) %>% #n < 260 < 259 < 250
  dplyr::filter(fs::file_exists(path)) %>%
  dplyr::filter(size > 0) %>%
  dplyr::select(-c(access_time, change_time))
filelist

# filelist$size %>% sort() %>% head()

# filelist$path %>% map_chr(~ digest(.x, file = T, algo = algo))
# filelist$path %>%
#   purrr::map_chr(~ digest::digest(.x, file = T, algo = algo)) %>%
#   split(filelist$path, .) %>%
#   purrr::keep(~ length(.x) > 1)

duplicate_files <- filelist$path %>%
  purrr::map_chr(~ digest::digest(.x, file = T, algo = algo)) %>%
  split(filelist$path, .) %>%
  purrr::keep(~ length(.x) > 1) %>%
  dplyr::tibble(duplicate_files = .) %>%
  dplyr::mutate(id = dplyr::row_number(), .before = duplicate_files) %>%
  tidyr::unnest_longer(col = duplicate_files) %>%
  dplyr::mutate(
    dir = fs::path_dir(duplicate_files),
    name = fs::path_file(duplicate_files)
  )
duplicate_files

(x <- filter(filelist, path %in% duplicate_files$duplicate_files))
duplicate_files <- duplicate_files %>%
  dplyr::full_join(x, by = c("duplicate_files" = "path")) %>%
  # dplyr::select(id:size, blocks, inode, modification_time, birth_time) %>%
  # dplyr::select(-c(path)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    n = nchar(name),
    max_n_group = max(n),
    min_n_group = min(n) #, my_dir = my_dir
  )
duplicate_files

mismatched_filesize <- duplicate_files %>%
  dplyr::summarise(nn = dplyr::n_distinct(size)) %>%
  dplyr::filter(nn > 1) %>% dplyr::pull(id)
mismatched_filesize #0

duplicate_files <- duplicate_files %>%
  dplyr::filter(!(id %in% mismatched_filesize))
duplicate_files
# return(duplicate_files)


#####
filelist$path %>%
  purrr::map_chr( ~ digest::digest(.x, file = T, algo = algo))
# Error: The file does not exist: C:/Users/Natalia/OneDrive/TOSORT/Downloads/Birgitte Snabe (auth.)-The Usage of System Dynamics in Organizational Interventions_ A Participative Modeling Approach Supporting Change Management Efforts-Deutscher UniversitÃ¤tsverlag (2007).pdf

filelist$path %>% file_exists() %>% summary()
length(filelist$path) #893
filelist %>% filter(fs::file_exists(path))

##
# filelist$path %>% digest::digest(file = T, algo = algo)
####

## origin: ####
(my_pkgs <- c(tidyverse::tidyverse_packages(), "digest", "fs"))

origin::originize_file(
  "C:/Users/Natalia/OneDrive/pkgs/etc/R/DuplicatesManager.R",
  pkgs = my_pkgs
)
