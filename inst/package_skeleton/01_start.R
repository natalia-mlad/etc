# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
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

## copy any existing code? ####
files <- c(
  fs::path_home("OneDrive/R Functions/utilities.R"),
  fs::path_home("OneDrive/R Functions/irt.R")
)
copy_files_over(files)


## Set options ----


## declare certain packages as imports ####
use_package("crayon", type = "Imports")
use_dev_package("bhappyr", type = "Imports", remote = "LudvigOlsen/bhappyr")

## Use git ------------------------------------------------------------
usethis::use_git()
usethis::use_github(private = TRUE)
usethis::git_vaccinate()

## Init Testing Infrastructure ----
use_testthat()
#testthat::test_file("C:/Users/Natalia/OneDrive/R Functions/etc/tests/testthat/test-HappyFeedback.R")

# You're now set! ----
# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )



## Fill the DESCRIPTION ----
# desc::cran_valid_fields #all the available fields
desc::desc_set("Title" = "...") #What the Package Does (One Line, Title Case)
desc::desc_set("Description" = "...") #What the package does (one paragraph)

## Create Common Files ----
usethis::use_readme_rmd( open = FALSE )
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_code_of_conduct()
usethis::use_news_md( open = FALSE )

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
golem::remove_favicon()
