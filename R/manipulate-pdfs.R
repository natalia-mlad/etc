# TODO: create a shiny app/addin??
# See: path_home("OneDrive/PhD Psychology/01 - R Project/manipulate pdfs.R")

#' rotate_pdf
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
#' @param dir dir
#'
#' @export
combine_pdfs <- function(files, dir) {
  input_pdfs <- paste0(dir, "/", files)
  stopifnot(file_exists(input_pdf))

  output_pdf <- file.path(dir, "output.pdf")
  stopifnot(!file_exists(output_pdf))

  qpdf::pdf_combine(input = input_pdfs, output = output_pdf)
  file_show(output_pdf)
}

# qpdf
# pdf_length(output_pdf)
# split a single pdf into separate files, one for each page:
# pdf_split(input, output = NULL, password = "")
# create a new pdf with a subset of the input pages:
# pdf_subset(input,
#            pages = 1,
#            output = NULL,
#            password = "")
