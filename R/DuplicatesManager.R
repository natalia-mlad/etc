# fs::file_show(
#   "C:/Users/Natalia/OneDrive/PhD Psychology/01 - R Project/duplicated-files.R"
#   )

# identify_duplicates -----------------------------------------------------
#'
#' Identify Duplicate Files
#'
#' DupeGuru-esque.
#' No output difference between the different algorithms:
#' algo = c("md5","crc32", "sha1", "sha256", "sha512",
#' "xxhash32", "xxhash64", "murmur32", "blake3")
#'
#' @param my_dir the directory to check
#' @param glob type of file to search for
#' @param algo hashing algorithm to use (see digest::digest)
#' @param excluded_dir directories to exclude
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' my_dir <- fs::path_home("OneDrive/PhD Psychology")
#'
#' xclude <- c("/00 - Admin/", "/01 - R Project/",
#' "/PDFs_BackUps/", "/62 Included Psych - Lit Review/")
#'
#' identify_duplicates(my_dir, glob = "*.pdf", excluded_dir = xclude) %>%
#' janitor::remove_constant()
#' }
#'
identify_duplicates <- function(my_dir, glob, algo = "xxhash64", excluded_dir = NULL) {
  # Set-Up:
   if(is.null(excluded_dir)){
     excluded <- paste0(paste(LETTERS, collapse = ""),
                        paste(1:10, collapse = ""))
  } else {
    stopifnot(all(stringr::str_detect(excluded_dir, "^/")))
    stopifnot(all(stringr::str_detect(excluded_dir, "/$")))
    excluded <- paste(excluded_dir, collapse = "|")
  }
  my_dir <- fs::path_abs(my_dir)

  # All files (path length limit):
  filelist <- my_dir %>%
    fs::dir_info(glob = glob, all = T, recurse = T, fail = F) %>%
    dplyr::filter(stringr::str_detect(path, excluded, negate = T)) %>%
    dplyr::filter(nchar(path) < 259) %>% #n < 260 < 259 < 250
    dplyr::filter(fs::file_exists(path)) %>%
    dplyr::filter(size > 0) %>%
    dplyr::select(-c(access_time, change_time))
  # dir_ls(my_dir, glob = glob, all = T, recurse = T, fail = F)
  # hash <- try({filelist$path %>% map_chr(~ digest(.x, file = T, algo = algo))})
  #,length = 5000

  duplicate_files <- filelist$path %>%
    purrr::map_chr(~ digest::digest(.x, file = T, algo = algo)) %>%
    split(filelist$path, .) %>%
    purrr::keep(~ length(.x) > 1) %>%
    dplyr::tibble(duplicate_files = .) %>%
    dplyr::mutate(id = dplyr::row_number(), .before = duplicate_files) %>%
    tidyr::unnest_longer(col = duplicate_files) %>% #unnest_auto
    dplyr::mutate(
      dir = fs::path_dir(duplicate_files),
      name = fs::path_file(duplicate_files)
      # head_path = fs::path_split(duplicate_files) %>% purrr::map_chr(pluck, 2)
    )

  x <- dplyr::filter(filelist, path %in% duplicate_files$duplicate_files)

  duplicate_files <- duplicate_files %>%
    dplyr::full_join(x, by = c("duplicate_files" = "path")) %>%
    dplyr::select(id:size, blocks, inode, modification_time, birth_time) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      n = nchar(name),
      max_n_group = max(n),
      min_n_group = min(n) #, my_dir = my_dir
    )

  mismatched_filesize <- duplicate_files %>%
    dplyr::summarise(nn = dplyr::n_distinct(size)) %>%
    dplyr::filter(nn > 1) %>%
    dplyr::pull(id)
  duplicate_files <- duplicate_files %>%
    dplyr::filter(!(id %in% mismatched_filesize))

  n <- summarise(duplicate_files, n = n() - 1) %>% pull(n) %>% sum()
  usethis::ui_done("{n} duplicate files found.")

  return(duplicate_files)
  # original <- duplicate_files %>% arrange(birth_time, n) %>% filter(row_number() == 1)
  # duplicate_files %>% mutate(id_original = if_else(duplicate_files %in% original$duplicate_files, 1, 0))
}

# remove_duplicates -------------------------------------------------------
#' remove_duplicates
#'
#' @param df output from identify_duplicates()
#' @param bad_dir directories that shouldn't contain the files
#' @param auto_remove T/F
#'
#' @return a list
#' @export
#'
remove_duplicates <- function(df, bad_dir, auto_remove = FALSE) {
  stopifnot(is.character(bad_dir))
  stopifnot(length(bad_dir) == 1)
  # all_ids <- unique(df$id)
  t1 <- df %>%
    dplyr::filter(any(duplicated(id) & duplicated(dir))) %>%
    #filter(n == min_n_group) %>%
    dplyr::arrange(n) %>% dplyr::filter(dplyr::row_number() != 1) %>%
    dplyr::pull(duplicate_files)
  t2 <- df %>%
    dplyr::filter(any(duplicated(id) & dir == bad_dir)) %>%
    # filter(row_number() != 1) %>%
    dplyr::filter(dir == bad_dir) %>%
    dplyr::pull(duplicate_files)
  bad_dir_ids <- dplyr::filter(df, duplicate_files %in% t2)$id
  # flatten_int(df2[which(df2$duplicate_files %in% t2), "id"])
  to_remove <- unique(c(t1, t2))

  if (length(to_remove) == 0) {
    cat(crayon::green("No duplicates found"))
    return(NULL)
  }

  df2 <- df %>%
    dplyr::filter(any(duplicate_files %in% to_remove)) %>%
    dplyr::select(id:head_path) %>%
    dplyr::mutate(remove = dplyr::if_else(duplicate_files %in% to_remove, 1, 0)) %>%
    ungroup %>% dplyr::arrange(id) %>% dplyr::mutate(id2 = dplyr::row_number()) %>%
    dplyr::group_by(id)

  if(dplyr::filter(df2, all(remove == 1)) %>% nrow() > 0){
    stop(cat(crayon::red("ERROR!!!")))
    #cli_abort(
  }

  if (length(to_remove) > 20) #cli_alert_warning("")
    cat(crayon::yellow("\nCaution: More than 20 files will be removed."))
  #alert <- combine_ansi_styles("bold", "red4"); cat(alert("Warning!"), "\n")
  # file_delete
  temp_trash <- fs::dir_create(fs::path_home("Desktop/temp_trash"))
  cat(
    crayon::green(
      paste0("\n"),
      crayon::bold(paste0("\n", "Files to be removed:", "\n")),
      paste0("\n") #, paste0("\t", to_remove, collapse = "\n")
    )
  )
  #cli::cli_bullets(to_remove)
  cli::cat_bullet(to_remove, col = "grey", bullet = "tick", bullet_col = "green")

  #-------------------------------------------------------------------------
  cli::cat_rule(col = "green") #col = NULL, background_col = NULL
  #cat(crayon::green(""))
  if(auto_remove) {
    unique(df$my_dir) %>% paste0(to_remove) %>% fs::file_move(temp_trash)
    x <- nrow(fs::dir_info(temp_trash, all = T, recurse = T, type = "file"))

    cli::cli_alert_success("{x} files removed.", wrap = T)
    cli::cli_alert_info("See {.path {temp_trash}} for the files.", wrap = T)
  } else{
    x <- length(to_remove)
    cli::cli_alert_info(
      "Please move the {x} files to {.path {temp_trash}}.", wrap = T
    )
  }
  # return:
  df <- df %>%
    dplyr::filter(!(duplicate_files %in% to_remove)) %>%
    dplyr::filter(any(duplicated(id)))
  out <- list(
    temp_trash = temp_trash,
    removed = list(
      filelist = to_remove,
      in_context = df2,
      bad_dir_ids = bad_dir_ids
    ),
    new_df = df
  )
  return(out)
}

##
# out %>%
#   #group_by(id) %>%
#   arrange(n) %>%
#   filter(row_number() == 1) %>%
#   # | row_number()==n()
#   #filter(id == 1)
#   arrange(id) %>%
#   pull(id) %>% unique() %>% length()
# out %>%
#   filter(any(duplicated(dir) & duplicated(id))) %>%
#   filter(n == n_group) %>%
#   filter(row_number() != 1)
# out %>%
#   filter(any(duplicate_files %in% to_remove)) %>%
#   select(id, duplicate_files) %>%
#   mutate(remove = if_else(duplicate_files %in% to_remove, 1, 0)) %>%
#   slice(3,6) %>%
#   filter(all(remove == 1))
##

# cli ---------------------------------------------------------------------
# > x <- cli_vec(names(mtcars), list(vec_trunc = 3))
# > cli_text("Column names: {x}.")
# months <- month.name[1:3]
# cli_text("{.val {months}}")
# ?cli_tick_reset()
# ?themes
# cli_h1("Header {.emph 1}")
# cli_h2("Header {.emph 2}")
# cli_h3("Header {.emph 3}")
# cli_bullets(c(
#   "noindent",
#   " " = "indent",
#   "*" = "bullet",
#   ">" = "arrow",
#   "v" = "success",
#   "x" = "danger",
#   "!" = "warning",
#   "i" = "info"
# ))
# debugonce(cli_bullets)
# cli_bullets(c("x" = to_remove))
#args$text$x1
# $str
# /03 - Data Science Sharepoint Copy/01 - Reports/TSB_Consumers_Matter_Loans (1).pdf
#
# $values
# <environment: 0x0000020e2c6088c0>
#
#args$text$x10
# $str
# /01 - Papers, Books, Theses/Literature on Forgiveness/Forgiveness_in_marriage_Current_status_a (2).pdf
#
# $values
# <environment: 0x0000020e2c50c2c8>
# #
# cli_bullets(c("x" = "danger"))
# cli_bullets("x" = "boop")
# cli_bullets(c("x" = "bunnies"))
# # args$text
# $x
# $str
# bunnies
#
# $values
# <environment: 0x0000020e2d4057c8>
#
# attr(,"class")
# [1] "cli_glue_delay"
#
#cli__message: function (type, args, .auto_close = TRUE, .envir = NULL, record = getOption("cli.record")) {
#   if ("id" %in% names(args) && is.null(args$id))
#     args$id <- new_uuid()
#   if (.auto_close && !is.null(.envir) && !identical(.envir, .GlobalEnv)) {
#     if (type == "status") {
#       defer(cli_status_clear(id = args$id, result = args$auto_result),
#             envir = .envir, priority = "first")
#     }
#     else {
#       defer(cli_end(id = args$id), envir = .envir, priority = "first")
#     }
#   }
#   cond <- cli__message_create(type, args)
#   if (is.null(record)) {
#     cli__message_emit(cond)
#     invisible(args$id)
#   }
#   else {
#     cli_recorded[[record]] <- c(cli_recorded[[record]],
#                                 list(cond))
#     invisible(cond)
#   }
# }
# args$text$x$str #danger

