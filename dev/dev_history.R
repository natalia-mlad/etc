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
##
#testthat::test_file("C:/Users/Natalia/OneDrive/R Functions/etc/tests/testthat/test-HappyFeedback.R")
##
devtools::document()
devtools::check()
#file_show("C:/Users/Natalia/AppData/Local/Temp/Rtmp8cHFn4/etc.Rcheck/00check.log")
##
install()
library(etc)
