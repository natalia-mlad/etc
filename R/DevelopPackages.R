#' Create a package just how I like it <3
#'
#' A personalised version of [usethis::create_package()]
#'
#' @param package_name Package name to use. By default, uses `basename(path)`.
#' If `path == '.'` & `package_name` is not explicitly set,
#' then `basename(getwd())` will be used.
#'
#' @param rstudio Boolean. In RStudio right now?
#' @param open Boolean. Open the created project?
#' @param roxygen Boolean. Use roxygen?
#' @param check_name Should we check that the package name is correct according to CRAN requirements.
#' @param use_email one from "personal", "work", or "city"
#'
#' @export
#' @return The path, invisibly.
#' @examples
#' \dontrun{
#' DevelopMyPackage("bunny")
#' }
#'
DevelopMyPackage <- function(package_name,
                             rstudio = rstudioapi::isAvailable(),
                             open = rlang::is_interactive(),
                             roxygen = TRUE, check_name = TRUE,
                             use_email = c("personal", "work", "city")) {
  stopifnot(is.character(package_name))

  ##~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 1. Check Name:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~
  usethis::ui_todo("Checking name availability & suitability")
  # TODO:
  #sendToConsole("available::available(name = package_name)")
  #available::available(name = package_name)
  usethis::ui_done("Valid package name")
  #ui_warn() / ui_stop()
  usethis::ui_line("{package_name} is a available!")
  if (usethis::ui_nope(
    "Would you like to continue with the creation of this package?",
    yes = "Yes", no = "No", shuffle = F)) {
    usethis::ui_stop("OK. Use some of the other packages to come up with different name")
  }
  #if (check_name) check_package_name(package_name)
  #invisible()

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 2. Create the Package Directories:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  path <- paste0(fs::path_expand("~/OneDrive/pkgs"), "/", package_name)
  ###
  #path <- paste0(fs::path_home("OneDrive/pkgs"), "/", package_name)
  #%>% fs::path_norm() #fs::path_tidy()
  ###
  if (path == '.' & package_name == fs::path_file(path)) {
    package_name <- fs::path_file(fs::path_wd())
  }
  usethis::ui_todo("Creating dir")
  fs::dir_create(path, recurse = TRUE)
  usethis::ui_done("Created package directory")
  # desired directories:
  #dir_names <- c("dev", "data", "code", "paper")
  dir_names <- c("dev", "inst/data", "inst/paper")
  # create the directories (silentrly ignoring existing ones)
  fs::dir_create(dir_names, recurse = TRUE)
  usethis::use_build_ignore("/dev")

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 3. Copying the Package Skeleton:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  usethis::ui_todo("Copying package skeleton")
  from <- fs::path_package("etc", "package_skeleton")
  to <- paste0(path, "/dev")
  # Copy over whole directory
  fs::dir_copy(path = from, new_path = to, overwrite = TRUE)
  usethis::ui_done("Copied package skeleton")

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 4. Creating the Package:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # initialize prjct + R dir:
  usethis::local_project(path, force = TRUE)
  usethis::use_directory("R")
  # populate the description:
  title <- "A package for ..." #What the Package Does (One Line, Title Case)
  description <- "I will add a description later." #What the package does (one paragraph)
  # which profile/email to use:
  use_email <- match.arg(use_email) #if(use_email == "work") { }
  authors <- switch(
    use_email,
    "personal" = 'person("Natalia", "Mladentseva", email = "natashka.ml@gmail.com", role = c("aut", "cre", "cph"))',
    "work" = 'person("Natalia", "Mladentseva", email = "Natalia.Mladentseva@loal.app", role = c("aut", "cre", "cph"))',
    "city" = 'person("Natalia", "Mladentseva", email = "natalia.mladentseva@city.ac.uk", role = c("aut", "cre", "cph"))',
    stop("Invalid `use_email` value")
    #,comment = c(ORCID = "YOUR-ORCID-ID"))
  )
  desc <- list(
    Title = title,
    Description = description,
    Version = "0.0.0.1",
    `Authors@R` = authors
  )
  usethis::use_description(desc, check_name = FALSE, roxygen = roxygen)
  usethis::use_proprietary_license("Natalia Mladentseva")
  usethis::use_namespace(roxygen = roxygen)

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 5. Returning the Package:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if (open & rstudioapi::isAvailable()) rstudioapi::openProject(path = path)
  if (rstudio) usethis::use_rstudio()
  if (open) {
    usethis::ui_done("Done!")
    usethis::ui_line("A new package named {package_name} was created at {path}.\n") #,fs::path_abs(path),
    usethis::ui_todo("Go to the `dev/01_start.R` file to continue working on your package.")
    if (usethis::proj_activate(usethis::proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
      #rstudioapi::navigateToFile("dev/01_start.R")
    }
  }

  invisible(usethis::proj_get())
}


# copy over files ---------------------------------------------------------
#' Copy files over for creating new packages
#'
#' @param ... filepaths of the files
#' @return list of files copied
#' @export
#' @examples
#' if(FALSE) {
#' copy_files_over(
#' fs::path_home("OneDrive/R Functions/utilities.R"),
#' fs::path_home("OneDrive/R Functions/irt.R")
#' )}
#'
copy_files_over <- function(...) {
  oldpaths <- c(...)
  oldpaths <- unique(oldpaths)
  newpaths <- fs::path_file(oldpaths) %>%
    paste0("R/", .) %>%
    fs::path_wd(.)
  purrr::map2_chr(oldpaths, newpaths, ~fs::file_copy(.x, .y))
  #invisible(newpaths)
}
