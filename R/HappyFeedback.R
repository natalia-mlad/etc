#' Lighten Natalia's mood
#'
#' @description
#' Primarily a wrapper around bhappyr to make the output colourful!
#' TODO: Build on the function more to introduce more sentence possibilities + bunnies
#'
#' @return a happy feedback
#'
#' @export
#'
#' @examples
#' library(etc)
#' HappyFeedback()
#'
HappyFeedback <- function() {
  sentence <- bhappyr::generate_sentence()
  cat(crayon::cyan(sentence))
}
