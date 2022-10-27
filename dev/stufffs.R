library(fs)
library(tidyverse)
##
mypath <- path_home("OneDrive/PhD Psychology/01 - R Project")
# dir_exists(mypath)
out <- dir_ls(mypath, all = TRUE, recurse = TRUE)
nchar(out) %>% max #267
out[nchar(out) >= 259] %>%
  str_remove(paste0(mypath, "/"))
# blog-master-zip
# DrakeModelling-master
# HTMLs
##
x <- out[nchar(out) < 259] %>% path_ext()
natalia::suna(x)
janitor::tabyl(x) %>%
  filter(percent >= 0.001) %>% #0.01
  arrange(desc(percent))
# Rmd, R
# py
# html
# pdf
# txt
