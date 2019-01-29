##----------------------------------------------------------------------------##
##                              OBJECT: wiki_data                             ##
##----------------------------------------------------------------------------##

#' wiki_data
#' 
#' @param x Either file name, a connection, or character vector of lines read
#'   into R environment.
#' @param ... Other args passed along to \link{\code{read_wiki}}.
#' @return A tibble data frame of wiki page view data.
wiki_data <- function(x, ...) UseMethod("wiki_data")

## if object is data frame
wiki_data.data.frame <- function(x, ...) {
  x <- tibble::as_tibble(x)
  class(x) <- c("wiki_data", "tbl_df", "data.frame")
  x
}

## if object is tbl_df
wiki_data.tbl_df <- function(x, ...) {
  class(x) <- c("wiki_data", "tbl_df", "data.frame")
  x
}

## if object is character
wiki_data.character <- function(x, ...) {
  x <- read_wiki(x, ...)
  wiki_data(x)
}

## if object is read_wiki
wiki_data.read_wiki <- function(x, ...) {
  x <- do.call("rbind", strsplit(x, " "))
  x <- tibble::data_frame(
    lang = x[, 1],
    topic = x[, 2], 
    pageviews = as.integer(x[, 3])
    #filter = as.integer(x[, 4])
  )
  x$medium <- NA_character_
  x$medium[grepl("\\.b$", x$lang)] <- "wikibooks"
  x$medium[grepl("\\.d$", x$lang)] <- "wikionary"
  x$medium[grepl("\\.m$", x$lang)] <- "wikimedia"
  x$medium[grepl("\\.mw$", x$lang)] <- "wikimedia mobile"
  x$medium[grepl("\\.n$", x$lang)] <- "wikinews"
  x$medium[grepl("\\.q$", x$lang)] <- "wikiquote"
  x$medium[grepl("\\.s$", x$lang)] <- "wikisource"
  x$medium[grepl("\\.v$", x$lang)] <- "wikiversity"
  x$medium[grepl("\\.w$", x$lang)] <- "mediawiki"
  x$lang <- gsub("\\..*", "", x$lang)
  x
}



##----------------------------------------------------------------------------##
##                               READ: read_wiki                              ##
##----------------------------------------------------------------------------##

#' read_wiki
#' 
#' @param x Name of file or connection containing wiki data.
#' @param skip Number of lines to skip before reading.
#' @param n_max Number of total lines to read.
#' @param na Value of missing data. For no missing, use \code{character()}.
#' @return Character object of class read_wiki
#' @export
read_wiki <- function(x, skip = 0, n_max = 10000, na = "") {
  UseMethod("read_wiki")
}

## if read_wiki is given character string
read_wiki.character <- function(x, skip = 0, n_max = 10000, na = "") {
  stopifnot(file.exists(x))
  x <- readr::read_lines(
    x, skip = skip, n_max = n_max, na = na
  )
  class(x) <- c("read_wiki", "character")
  x
}

## if read_wiki is given a file connection
read_wiki.file <- function(x, skip = 0, n_max = 10000, na = "") {
  x <- readr::read_lines(
    x, skip = skip, n_max = n_max, na = na
  )
  class(x) <- c("read_wiki", "character")
  x
}

## if read_wiki is given a url connection
read_wiki.url <- function(x, skip = 0, n_max = 10000, na = "") {
  x <- readr::read_lines(
    x, skip = skip, n_max = n_max, na = na
  )
  class(x) <- c("read_wiki", "character")
  x
}