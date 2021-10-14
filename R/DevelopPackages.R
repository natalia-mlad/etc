#' @title Create a package just how I like it <3
#'
#' @param package_name Package name to use. By default, uses `basename(path)`.
#' If `path == '.'` & `package_name` is not explicitly set,
#' then `basename(getwd())` will be used.
#'
#' @param rstudio Boolean. In RStudio right now?
#' @param open Boolean. Open the created project?
#' @param roxygen Boolean. Use roxygen?
#' @param check_name Should we check that the package name is correct according to CRAN requirements.
#'
#' @importFrom rstudioapi isAvailable openProject sendToConsole
#' @importFrom usethis create_package use_latest_dependencies ui_line ui_nope
#' @importFrom fs path path_abs path_package path_file path_expand dir_copy dir_create
#' @importFrom yaml write_yaml
#'
#' @export
#'
#' @return The path, invisibly.
#'
#' @note checke golem::create_golem for more inspiration;
#' magrittr pipe
#' example DevelopMyPackage("bunny")
#'
DevelopMyPackage <- function(package_name,
                             rstudio = rstudioapi::isAvailable(),
                             open = rlang::is_interactive(),
                             roxygen = TRUE, check_name = TRUE,
                             use_email = c("personal", "work", "city")) {
  stopifnot(is.character(package_name))
  require(usethis)
  #require(rstudioapi)
  #require(glue)

  ##~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 1. Check Name:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~
  ui_todo("Checking name availability & suitability")
  # TODO:
  #sendToConsole("available::available(name = package_name)")
  #available::available(name = package_name)
  ui_done("Valid package name")
  #ui_warn() / ui_stop()
  #{qualification}
  #qualification <- if (is_windows()) {
  #   glue("a special directory, i.e. some applications regard it as ")
  # } else ""
  ui_line("{package_name} is a available!")
  if (ui_nope(
    "Would you like to continue with the creation of this package?",
    yes = "Yes", no = "No", shuffle = F)) {
    ui_stop("OK. Use some of the other packages to come up with different name")
  }
  #if (check_name) check_package_name(package_name)
  #invisible()

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 2. Create the Package Directories:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  path <- paste0(path_expand("~/OneDrive/pkgs"), "/", package_name)
  ###
  #path <- paste0(fs::path_home("OneDrive/pkgs"), "/", package_name)
  #%>% fs::path_norm() #fs::path_tidy()
  ###
  if (path == '.' & package_name == path_file(path)) {
    package_name <- path_file(path_wd())
  }
  ui_todo("Creating dir")
  dir_create(path, recurse = TRUE)
  ui_done("Created package directory")
  # desired directories:
  #dir_names <- c("dev", "data", "code", "paper")
  dir_names <- c("dev", "inst/data", "inst/paper")
  # create the directories (silentrly ignoring existing ones)
  dir_create(dir_names, recurse = TRUE)
  #   if (dir_exists(path)){
  #     res <- yesno(
  #       paste("The path", path, "already exists, override?")
  #     )
  #     if (!res){
  #       return(invisible(NULL))
  #     }
  #   }
  #sendToConsole("usethis::create_package(path)")
  #create_package(path, rstudio = rstudio, open = open, roxygen = roxygen, check_name = check_name)
  usethis::use_build_ignore("/dev")

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 3. Copying the Package Skeleton:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ui_todo("Copying package skeleton")
  from <- fs::path_package("etc", "package_skeleton")
  to <- paste0(path, "/dev")
  # Copy over whole directory
  dir_copy(path = from, new_path = to, overwrite = TRUE)
  # Listing copied files ***from source directory***
  # copied_files <-
  #   list.files(
  #     path = from,
  #     full.names = FALSE,
  #     all.files = TRUE,
  #     recursive = TRUE
  #   )
  # Going through copied files to replace package name
  # for (f in copied_files) {
  #   copied_file <- file.path(path, f)
  #   if (grepl("^REMOVEME", f)) {
  #     file.rename(from = copied_file,
  #                 to = file.path(path, gsub("REMOVEME", "", f)))
  #     copied_file <- file.path(path, gsub("REMOVEME", "", f))
  #   }
  #   if (!grepl("ico$", copied_file)) {
  #     try({
  #       replace_word(file = copied_file,
  #                    pattern = "shinyexample",
  #                    replace = package_name)
  #     }, silent = TRUE)
  #   }
  # }
  ui_done("Copied package skeleton")

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 4. Setting the Default Config:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ui_todo("Setting the default config")
  #   yml_path <- path(path, "inst/golem-config.yml")
  #   conf <- yaml::read_yaml(yml_path, eval.expr = TRUE)
  #   yaml_golem_wd <- "here::here()"
  #   attr(yaml_golem_wd, "tag") <- "!expr"
  #   conf$dev$golem_wd <- yaml_golem_wd
  #   conf$default$golem_name <- package_name
  #   conf$default$golem_version <- "0.0.0.1"
  #   write_yaml(conf, yml_path)
  #ui_done("{ui_field('Configured app')}")
  ui_done("Configured the package")

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 5. Creating the Package:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #create_package <- function(path, fields = list(), rstudio = rstudioapi::isAvailable(), roxygen = TRUE, check_name = TRUE, open = rlang::is_interactive()) {
  #create_directory(path)
  # initialize prjct + R dir:
  local_project(path, force = TRUE)
  use_directory("R")
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
  use_description(desc, check_name = FALSE, roxygen = roxygen)
  use_proprietary_license("Natalia Mladentseva")
  use_namespace(roxygen = roxygen)

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ 6. Returning the Package:  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if (open & rstudioapi::isAvailable()) {
  #   rstudioapi::openProject(path = path)
  # }
  if (rstudio) use_rstudio()
  if (open) {
    ui_done("Done!")
    ui_line("A new package named {package_name} was created at {path}.\n") #,path_abs(path),
    ui_todo("Go to the `dev/01_start.R` file to continue working on your package.")
    if (proj_activate(proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
      #rstudioapi::navigateToFile("dev/01_start.R")
    }
  }
  invisible(proj_get())
  #return(invisible(path))
}


###
# create_golem <- function(
#   path,
#   check_name = TRUE,
#   open = TRUE,
#   package_name = basename(path),
#   without_comments = FALSE,
#   project_hook = golem::project_hook,
#   ...
# ) {
#   path <- path_expand(path)
#
#   if (check_name){
#     cat_rule("Checking package name")
#     getFromNamespace("check_package_name", "usethis")(package_name)
#     cat_green_tick("Valid package name")
#   }
#
#   if ( rstudioapi::isAvailable() ) {
#     cat_rule("Rstudio project initialisation")
#     rproj_path <- rstudioapi::initializeProject(path = path)
#
#     if (file.exists(rproj_path)){
#       enable_roxygenize(path = rproj_path)
#     }else{
#       stop("can't create .Rproj file ")
#     }
#   }
#
#   cat_rule("Running project hook function")
#   old <- setwd(path)
#   # TODO fix
#   # for some weird reason test() fails here when using golem::
#   # and I don't have time to search why rn
#   if (substitute(project_hook) == "golem::project_hook"){
#     project_hook <- getFromNamespace("project_hook", "golem")
#   }
#   project_hook(path = path, package_name = package_name, ...)
#   setwd(old)
#   cat_green_tick("All set")
#   if ( without_comments == TRUE ) {
#     files <- list.files(
#       path = c(
#         path(path, "dev"),
#         path(path, "R")
#       ),
#       full.names = TRUE
#     )
#     for ( file in files ) {
#       remove_comments(file)
#     }
#   }
#
#   old <- setwd(path)
#   use_latest_dependencies()
#
#   # No .Rprofile for now
#   # cat_rule("Appending .Rprofile")
#   # write("# Sourcing user .Rprofile if it exists ", ".Rprofile", append = TRUE)
#   # write("home_profile <- file.path(", ".Rprofile", append = TRUE)
#   # write("  Sys.getenv(\"HOME\"), ", ".Rprofile", append = TRUE)
#   # write("  \".Rprofile\"", ".Rprofile", append = TRUE)
#   # write(")", ".Rprofile", append = TRUE)
#   # write("if (file.exists(home_profile)){", ".Rprofile", append = TRUE)
#   # write("  source(home_profile)", ".Rprofile", append = TRUE)
#   # write("}", ".Rprofile", append = TRUE)
#   # write("rm(home_profile)", ".Rprofile", append = TRUE)
#   #
#   # write("# Setting shiny.autoload.r to FALSE ", ".Rprofile", append = TRUE)
#   # write("options(shiny.autoload.r = FALSE)", ".Rprofile", append = TRUE)
#   # cat_green_tick("Appended")
#
#   setwd(old)
# }
#

