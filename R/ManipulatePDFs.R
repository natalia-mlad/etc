# TODO: create a shiny app/addin??
# See: path_home("OneDrive/PhD Psychology/01 - R Project/manipulate pdfs.R")

#' rotate pdf
#'
#' @param dir e.g., fs::path_home_r("01 - Important Documents/NM/BRP")
#' @param file_name e.g., "NM.pdf"
#' @param rotation_degrees numeric, default 90
#'
#' @export
rotate_pdf <- function(file_name, dir, rotation_degrees = 90) {
  input_pdf <- paste0(dir, "/", file_name)
  stopifnot(file_exists(input_pdf))

  output_pdf <- file.path(dir, "output.pdf")
  stopifnot(!file_exists(output_pdf))

  staplr::rotate_pdf(
    page_rotation = rotation_degrees,
    input_pdf = input_pdf,
    output_pdf = output_pdf
  )
  file_show(output_pdf)
}


#' combine pdfs
#'
#' @param files files
#' @param output_name output.pdf by default; only specify name, not extension
#' @param dir NULL by default so will put in common path of the files
#'
#' @export
combine_pdfs <- function(files, output_name = "output", dir = NULL) {
  stopifnot(all(file_exists(files)))
  # path_tidy(files) == files %>% allTRUE()
  # dir <- files %>% path_dir() %>% unique()
  # input_pdfs <- paste0(dir, "/", files)
  if(is.null(dir)) dir <- path_common(files)

  output_pdf <- file.path(dir, paste0(output_name, ".pdf"))
  stopifnot(!file_exists(output_pdf))

  qpdf::pdf_combine(input = files, output = output_pdf)
  usethis::ui_done("See {dir} for output PDF.")
  file_show(output_pdf)
}


#' split pdf
#' create a new pdf with a subset of the input pages
#'
#' @param file file
#' @param dir dir
#' @param pages e.g., 1 or 1:3 or 3:5 etc
#' @export
split_pdf <- function(file_name, dir, pages) {
  input_pdf <- paste0(dir, "/", file_name)
  stopifnot(file_exists(input_pdf))
  # pdf_length(input_pdf)

  output_pdf <- file.path(dir, "output.pdf")
  stopifnot(!file_exists(output_pdf))

  qpdf::pdf_subset(
    input = input_pdf,
    output = output_pdf,
    pages = pages
  )
  # password = ""
  file_show(output_pdf)
}