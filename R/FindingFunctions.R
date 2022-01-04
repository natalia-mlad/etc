#' fetch_helpfiles
#' See C:/Users/Natalia/OneDrive/PhD Psychology/01 - R Project/finding-func.R
#' @param func_name function name
#' @param pkg package name e.g., "base"
#' @export
fetch_helpfiles <- function(func_name, pkg = "base") {
  # fetch_helpfiles <- function(func_name, pkg){
  # help(func_name, package = pkg) %>%
  # cat(crayon::red(paste0(func_name, "\n")))

  helpfile <- tryCatch({
    helpfile <- getHelpFile(help(func_name, package = pkg))
  }, error = function(err) {
    helpfile <- NA
  })

  if(is.na(helpfile)) return(NULL)
  # if(is_empty(helpfile)) return(NULL)
  # cat(crayon::green(paste0(func_name, "\n")))

  helpfile %>%
    tools::Rd2HTML() %>%
    capture.output() %>%
    paste(collapse = "") %>%
    read_html()
}

# utils:::
getHelpFile <- function(file){
  path <- dirname(file)
  dirpath <- dirname(path)
  if (!file.exists(dirpath))
    stop(gettextf("invalid %s argument", sQuote("file")),
         domain = NA)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  if(!file.exists(paste0(RdDB, ".rdx")))
    stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed",
                  sQuote(pkgname)), domain = NA)
  fetchRdDB(RdDB, basename(file))
}

# tools:::
fetchRdDB <- function (filebase, key = NULL){
  fun <- function(db) {
    vals <- db$vals
    vars <- db$vars
    datafile <- db$datafile
    compressed <- db$compressed
    envhook <- db$envhook
    fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]],
                                           datafile, compressed, envhook)
    if (length(key)) {
      if (key %notin% vars)
        stop(gettextf("No help on %s found in RdDB %s",
                      sQuote(key), sQuote(filebase)), domain = NA)
      fetch(key)
    }
    else {
      res <- lapply(vars, fetch)
      names(res) <- vars
      res
    }
  }

  res <- lazyLoadDBexec(filebase, fun)
  if(length(key)) res else invisible(res)
}


######
#' extract_from_helpfiles
#' See C:/Users/Natalia/OneDrive/PhD Psychology/01 - R Project/finding-func.R
#' @param helpfile helpfile
#' @param selector "p"
#' @export
extract_from_helpfiles <- function(helpfile, selector = "p"){
  if(is.na(helpfile)) return(NA)

  headings <- helpfile %>% html_elements("h3")
  # Get the text inside the headlines
  xpaths <- selector %>%
    paste0("//", ., "[count(preceding-sibling::h3)=%d]") %>%
    sprintf(seq_along(headings))
  xpaths %>%
    map( ~ html_elements(helpfile, xpath = .x)) %>%
    map( ~ html_text2(.x) %>% paste(collapse = "\n")) %>%
    set_names(html_text2(headings)) #%>% compact
}


#######
#' lsp
#' See C:/Users/Natalia/OneDrive/PhD Psychology/01 - R Project/finding-func.R
#' @param package pkg
#' @param what what
#' @param pattern pattern
#' @export
lsp <- function(package, what, pattern){
  if (!is.character(substitute(package))) package <- deparse(substitute(package))
  ns <- asNamespace(package)

  if (missing(pattern)) pattern <- '.*'

  ## base package does not have NAMESPACE
  if (isBaseNamespace(ns)) {
    res <- ls(.BaseNamespaceEnv, all.names = TRUE)
    return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
  } else {
    ## for non base packages
    if (exists('.__NAMESPACE__.', envir = ns, inherits = FALSE)) {
      wh <- get(
        '.__NAMESPACE__.',
        inherits = FALSE,
        envir = asNamespace(package, base.OK = FALSE)
      )

      what <- if(missing(what)) 'all'
      else if('?' %in% what) return(ls(wh))
      else ls(wh)[pmatch(what[1], ls(wh))]

      if(!is.null(what) && !any(what %in% c('all', ls(wh))))
        stop(
          '\'what\' should be one of ',
          paste0(shQuote(ls(wh)), collapse = ', '),
          ', or \'all\'',
          domain = NA
        )

      res <- sapply(ls(wh), function(x) getNamespaceInfo(ns, x))
      res <- rapply(res, ls, classes = 'environment', how = 'replace', all.names = T)

      if (is.null(what)) return(res[grep(pattern, res, perl = T, ignore.case = T)])

      if (what %in% 'all') {
        res <- ls(getNamespace(package), all.names = TRUE)
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      }

      if (any(what %in% ls(wh))) {
        res <- res[[what]]
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      }
    } else
      stop(sprintf('no NAMESPACE file found for package %s', package))
  }
}
