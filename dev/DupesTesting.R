# fs::file_show(
#   "C:/Users/Natalia/OneDrive/PhD Psychology/01 - R Project/duplicated-files.R"
#   )


##
# out %>%
#   #group_by(id) %>%
#   arrange(n) %>%
#   filter(row_number() == 1) %>%
#   # | row_number()==n()
#   #filter(id == 1)
#   arrange(id) %>%
#   pull(id) %>% unique() %>% length()
# out %>%
#   filter(any(duplicated(dir) & duplicated(id))) %>%
#   filter(n == n_group) %>%
#   filter(row_number() != 1)
# out %>%
#   filter(any(duplicate_files %in% to_remove)) %>%
#   select(id, duplicate_files) %>%
#   mutate(remove = if_else(duplicate_files %in% to_remove, 1, 0)) %>%
#   slice(3,6) %>%
#   filter(all(remove == 1))
##


# cli ---------------------------------------------------------------------
# > x <- cli_vec(names(mtcars), list(vec_trunc = 3))
# > cli_text("Column names: {x}.")
# months <- month.name[1:3]
# cli_text("{.val {months}}")
# ?cli_tick_reset()
# ?themes
# cli_h1("Header {.emph 1}")
# cli_h2("Header {.emph 2}")
# cli_h3("Header {.emph 3}")
# cli_bullets(c(
#   "noindent",
#   " " = "indent",
#   "*" = "bullet",
#   ">" = "arrow",
#   "v" = "success",
#   "x" = "danger",
#   "!" = "warning",
#   "i" = "info"
# ))
# debugonce(cli_bullets)
# cli_bullets(c("x" = to_remove))
#args$text$x1
# $str
# /03 - Data Science Sharepoint Copy/01 - Reports/TSB_Consumers_Matter_Loans (1).pdf
#
# $values
# <environment: 0x0000020e2c6088c0>
#
#args$text$x10
# $str
# /01 - Papers, Books, Theses/Literature on Forgiveness/Forgiveness_in_marriage_Current_status_a (2).pdf
#
# $values
# <environment: 0x0000020e2c50c2c8>
# #
# cli_bullets(c("x" = "danger"))
# cli_bullets("x" = "boop")
# cli_bullets(c("x" = "bunnies"))
# # args$text
# $x
# $str
# bunnies
#
# $values
# <environment: 0x0000020e2d4057c8>
#
# attr(,"class")
# [1] "cli_glue_delay"
#
#cli__message: function (type, args, .auto_close = TRUE, .envir = NULL, record = getOption("cli.record")) {
#   if ("id" %in% names(args) && is.null(args$id))
#     args$id <- new_uuid()
#   if (.auto_close && !is.null(.envir) && !identical(.envir, .GlobalEnv)) {
#     if (type == "status") {
#       defer(cli_status_clear(id = args$id, result = args$auto_result),
#             envir = .envir, priority = "first")
#     }
#     else {
#       defer(cli_end(id = args$id), envir = .envir, priority = "first")
#     }
#   }
#   cond <- cli__message_create(type, args)
#   if (is.null(record)) {
#     cli__message_emit(cond)
#     invisible(args$id)
#   }
#   else {
#     cli_recorded[[record]] <- c(cli_recorded[[record]],
#                                 list(cond))
#     invisible(cond)
#   }
# }
# args$text$x$str #danger


#### path length: ####
library(fs)
library(tidyverse)
##
# mypath <- "C:/Users/Natalia/OneDrive/PhD Psychology"
# dir_ls(mypath, type = "directory")
mypath <- paste0(#mypath,
  "C:/Users/Natalia/OneDrive/PhD Psychology",
  "/01 - Investigation - Developing Creditworthiness Measure"
)
out <- dir_ls(mypath, all = TRUE, recurse = TRUE)
nchar(out) %>% hist
nchar(out) %>% max #268

out[nchar(out) >= 259] %>%
  str_remove(paste0(mypath, "/")) %>%
  path_split()

out[nchar(out) >= 259] %>% tail(1) %>% nchar()


##
#### DupesTesting: #####
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
