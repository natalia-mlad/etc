# > oldpath <- "C:/Users/Natalia/OneDrive/R Functions/utilities.R"
# > newpath <- path_wd("R/utilities.R")
# > file_copy(oldpath, newpath)
##
use_description(
  fields = list(
    Title = "Extraneous Functions for Natalia's Benefit",
    Description = "see above.",
    Version = "0.0.0.1",
    `Authors@R` = 'person("Natalia", "Mladentseva", email = "natashka.ml@gmail.com", role = c("aut", "cre"))',
    Language = "en"
  )
)
use_mit_license()
use_testthat()
##
use_package("crayon", type = "Imports")
use_dev_package("bhappyr", type = "Imports", remote = "LudvigOlsen/bhappyr")
# use_package(c())
##
#testthat::test_file("C:/Users/Natalia/OneDrive/R Functions/etc/tests/testthat/test-HappyFeedback.R")
##
devtools::document()
devtools::check()
#file_show("C:/Users/Natalia/AppData/Local/Temp/Rtmp8cHFn4/etc.Rcheck/00check.log")
##
install()
library(etc)


# R CMD check: ####
##
# > checking dependencies in R code ... WARNING
# '::' or ':::' imports not declared from:
# 'CodeDepends' 'archive' 'cli' 'digest' 'janitor' 'knitr' 'purrr' 'qpdf' 'staplr' 'stringr'
#
# 'library' or 'require' calls not declared from:
#   'dplyr' 'usethis'
#
# 'library' or 'require' calls in package code:
#   'dplyr' 'usethis'
#
# Please use :: or requireNamespace() instead.
#
# See section 'Suggested packages' in the 'Writing R Extensions' manual.
##

##
# > checking R code for possible problems ... NOTE
# DevelopMyPackage: no visible global function definition for 'ui_todo'
# DevelopMyPackage: no visible global function definition for 'ui_done'
# DevelopMyPackage: no visible global function definition for 'ui_stop'
# DevelopMyPackage: no visible global function definition for 'path_wd'
# DevelopMyPackage: no visible global function definition for
# 'local_project'
# DevelopMyPackage: no visible global function definition for
# 'use_directory'
# DevelopMyPackage: no visible global function definition for
# 'use_description'
# DevelopMyPackage: no visible global function definition for
# 'use_proprietary_license'
# DevelopMyPackage: no visible global function definition for
# 'use_namespace'
# DevelopMyPackage: no visible global function definition for
# 'use_rstudio'
# DevelopMyPackage: no visible global function definition for
# 'proj_activate'
# DevelopMyPackage: no visible global function definition for 'proj_get'
# cc_findFiles: no visible binding for global variable 'NuLL'
# cc_findFiles: no visible global function definition for '%>%'
# cc_findFiles: no visible global function definition for 'arrange'
# cc_findFiles: no visible global function definition for 'desc'
# cc_findFiles: no visible binding for global variable
# 'modification_time'
# cc_findFiles: no visible global function definition for 'select'
# cc_findFiles: no visible binding for global variable 'size'
# code.converter: no visible binding for global variable 'slot'
# code.converter: no visible global function definition for '%>%'
# code.converter: no visible binding for global variable '.'
# code.converter: no visible global function definition for 'map'
# code.converter: no visible global function definition for 'flatten_chr'
# code.converter: no visible global function definition for 'str_subset'
# code.converter: no visible global function definition for 'group_by'
# code.converter: no visible binding for global variable 'step'
# code.converter: no visible global function definition for 'summarise'
# code.converter: no visible binding for global variable 'used'
# code.converter: no visible binding for global variable 'defined'
# code.converter: no visible global function definition for 'filter'
# code.converter: no visible binding for global variable 'n.defined'
# code.converter: no visible global function definition for 'select'
# combine_pdfs: no visible global function definition for 'file_exists'
# combine_pdfs: no visible binding for global variable 'input_pdf'
# combine_pdfs: no visible global function definition for 'file_show'
# copy_files_over: no visible global function definition for '%>%'
# copy_files_over: no visible binding for global variable '.'
# identify_duplicates: no visible global function definition for '%>%'
# identify_duplicates: no visible global function definition for
# 'str_detect'
# identify_duplicates: no visible global function definition for
# 'dir_info'
# identify_duplicates: no visible global function definition for 'filter'
# identify_duplicates: no visible global function definition for 'mutate'
# identify_duplicates: no visible binding for global variable 'n'
# identify_duplicates: no visible global function definition for 'select'
# identify_duplicates: no visible binding for global variable
# 'access_time'
# identify_duplicates: no visible binding for global variable
# 'change_time'
# identify_duplicates: no visible global function definition for
# 'map_chr'
# identify_duplicates: no visible binding for global variable '.'
# identify_duplicates: no visible global function definition for 'keep'
# identify_duplicates: no visible global function definition for 'map'
# identify_duplicates: no visible global function definition for 'tibble'
# identify_duplicates: no visible global function definition for
# 'row_number'
# identify_duplicates: no visible global function definition for
# 'unnest_auto'
# identify_duplicates: no visible global function definition for
# 'path_dir'
# identify_duplicates: no visible global function definition for
# 'path_split'
# identify_duplicates: no visible binding for global variable 'pluck'
# identify_duplicates: no visible global function definition for
# 'str_remove_all'
# identify_duplicates: no visible global function definition for
# 'full_join'
# identify_duplicates: no visible binding for global variable 'id'
# identify_duplicates: no visible binding for global variable 'size'
# identify_duplicates: no visible binding for global variable 'blocks'
# identify_duplicates: no visible binding for global variable 'inode'
# identify_duplicates: no visible binding for global variable
# 'modification_time'
# identify_duplicates: no visible binding for global variable
# 'birth_time'
# identify_duplicates: no visible global function definition for
# 'group_by'
# identify_duplicates: no visible binding for global variable 'name'
# identify_duplicates: no visible global function definition for
# 'summarise'
# identify_duplicates: no visible global function definition for
# 'n_distinct'
# identify_duplicates: no visible binding for global variable 'nn'
# identify_duplicates: no visible global function definition for 'pull'
# remove_duplicates: no visible global function definition for '%>%'
# remove_duplicates: no visible global function definition for 'filter'
# remove_duplicates: no visible binding for global variable 'id'
# remove_duplicates: no visible global function definition for 'arrange'
# remove_duplicates: no visible binding for global variable 'n'
# remove_duplicates: no visible global function definition for
# 'row_number'
# remove_duplicates: no visible global function definition for 'pull'
# remove_duplicates: no visible binding for global variable
# 'duplicate_files'
# remove_duplicates: no visible global function definition for 'select'
# remove_duplicates: no visible binding for global variable 'head_path'
# remove_duplicates: no visible global function definition for 'mutate'
# remove_duplicates: no visible global function definition for 'if_else'
# remove_duplicates: no visible binding for global variable 'ungroup'
# remove_duplicates: no visible global function definition for 'group_by'
# remove_duplicates: no visible global function definition for
# 'path_home'
# remove_duplicates: no visible global function definition for
# 'file_move'
# remove_duplicates: no visible global function definition for 'dir_info'
# rotate_pdf: no visible global function definition for 'file_exists'
# rotate_pdf: no visible global function definition for 'file_show'
# sort_my_zips: no visible global function definition for '%>%'
# sort_my_zips: no visible binding for global variable
# 'all_zip_extensions'
# sort_my_zips: no visible binding for global variable '.'
# sort_my_zips: no visible global function definition for 'rename'
# sort_my_zips: no visible global function definition for 'mutate'
# sort_my_zips: no visible binding for global variable 'filename'
# sort_my_zips: no visible global function definition for 'filter'
# sort_my_zips: no visible binding for global variable 'name'
# sort_my_zips: no visible global function definition for 'pull'
# sort_my_zips: no visible global function definition for 'map'
# sort_my_zips: no visible binding for global variable 'my_dir'
# sort_my_zips: no visible global function definition for 'map_int'
# sort_my_zips: no visible binding for global variable 'zip_contents'
# sort_my_zips: no visible global function definition for 'map2'
# sort_my_zips: no visible binding for global variable 'n_zip'
# sort_my_zips: no visible binding for global variable
# 'extracted_contents'
# sort_my_zips: no visible binding for global variable 'n_extracted'
##
# Undefined global functions or variables:
#   %>% . NuLL access_time all_zip_extensions arrange birth_time blocks
# change_time defined desc dir_info duplicate_files extracted_contents
# file_exists file_move file_show filename filter flatten_chr full_join
# group_by head_path id if_else inode input_pdf keep local_project map
# map2 map_chr map_int modification_time mutate my_dir n n.defined
# n_distinct n_extracted n_zip name nn path_dir path_home path_split
# path_wd pluck proj_activate proj_get pull rename row_number select
# size slot step str_detect str_remove_all str_subset summarise tibble
# ui_done ui_stop ui_todo ungroup unnest_auto use_description
# use_directory use_namespace use_proprietary_license use_rstudio used
# zip_contents
##
# Consider adding
# importFrom("methods", "slot")
# importFrom("stats", "filter", "step")
# to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
#                         contains 'methods').

###
# Error: R CMD check found WARNINGs
# Execution halted
