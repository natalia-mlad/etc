# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.

####
##
# Key steps that accelerate your R development workflow (details on how to do all this follow):
# - Make usethis available in interactive R sessions.
# - Provide usethis with info to use in all new R packages you create.
# - Use the “sitrep” functions to get a health check or gather info when you’re going to ask for help.
# - Configure your Git user.name and user.email.
# - If you use RStudio, make sure RStudio can find your Git executable. If you use GitHub, make sure you can pull/push from your local computer to GitHub.com, in general and from RStudio.
# - Get a personal access token from GitHub.com and make it available in R sessions.
# - Prepare your system to build R packages from source.
##
# Configuration - Configure the behaviour of R or RStudio, globally as a user or for a specific project.
#  use_blank_slate() #= Don't save/load user workspace between sessions
# Helpers to make useful changes to .Rprofile:
#  use_conflicted() use_reprex() use_usethis() use_devtools() use_partial_warnings()
# Open configuration files:
#  edit_r_profile() edit_r_environ() edit_r_buildignore() edit_r_makevars()
#  edit_rstudio_prefs() edit_git_config() edit_git_ignore()
###


##### copy any existing code? ####
# > oldpath <- "C:/Users/Natalia/OneDrive/R Functions/utilities.R"
# > newpath <- path_wd("R/utilities.R")
# > file_copy(oldpath, newpath)

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


## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "golem", # The Name of the package containing the App
  pkg_title = "PKG_TITLE", # The Title of the package containing the App
  pkg_description = "PKG_DESC.", # The Description of the package containing the App
  author_first_name = "AUTHOR_FIRST", # Your First Name
  author_last_name = "AUTHOR_LAST", # Your Last Name
  author_email = "AUTHOR@MAIL.COM", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional)
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
usethis::use_mit_license( "Golem User" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

# Use git ------------------------------------------------------------
usethis::use_git()
usethis::use_github(private = TRUE)
usethis::git_vaccinate()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
golem::remove_favicon()

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

