#' @title get_chapter - Download a single chapter from ctext.org
#' @description Unvectorized function. Used as a building block for get_chapters and get_books.
#' @param book name of book
#' @param chapter name or number of chapter in book
#' @export TRUE
#' @return Returns a dataframe with 4 columns: text, book, chapter name in English, & chapter name in Chinese.
#'   In the case for the dao-de-jing, chapter param not needed.
#'   If you reach the API call limit or the connection has an error, function will retry 3 times.
#'    If still unsuccessful, an NA value will be returned in the "text" column.
#' @examples
#' get_chapter("analects", "xue-er")
#' get_chapter("analects", 1)
#' get_chapter("dao-de-jing")

get_chapter <- function(book, chapter = NA) {
  stopifnot(is.character(book) | book %in% ctextclassics::book_list$book)

  counter <- 0
  ok <- FALSE

  if (is.numeric(chapter)) {
    indexed_book <-
      book_list[ctextclassics::book_list$book == book, c("book", "chapter")]
    chapter <- indexed_book[chapter, 2]
  }
  if (book == "dao-de-jing") {
    path <- paste("/gettext?urn=", "ctp:", book, sep = "")
  } else {
    path <-
      paste("/gettext?urn=", "ctp:", book, "/", chapter[[1]], sep = "")
  }

  while(ok == FALSE & counter < 3) {
    raw_data <- ctextclassics::ctext_api(path)
    counter <- counter + 1

    if(grepl("requires|request", raw_data[[1]][1], ignore.case = TRUE)) {
      cat(" API error, trying", 3-counter, "more times\n", sep = " ")
      raw_data <- list(text = NA, chapter = chapter)
      Sys.sleep(2)
    } else {
      ok <- TRUE
    }
  }
  raw_data <- do.call(rbind, raw_data)

  setNames(
    data.frame(
      book,
      chapter,
      matrix(unlist(raw_data), ncol = 2,
             byrow = TRUE),
      stringsAsFactors = FALSE
    ),
    c("book", "chapter", "word", "chapter_cn")
  )
}

#' @title get_chapters - download multiple chapters from ctext.org.
#' @description Vectorized form of get_chapter. Download multiple chapters from a single book or multple books of your
#' specification. Can partially download books, and can mix and match books.
#' @param book name of book or vector of book names
#' @param chapter name or number of chapter, or vector of chapters
#' @export TRUE
#' @return Returns a dataframe with 4 columns: text, book, chapter name in English, & chapter name in Chinese.
#'   In the case for the dao-de-jing, only book arg is needed. If you reach the API call limit or
#'   the connection has an error, function will retry 3 times. If still unsuccessful, an NA value will be returned in the "text" column.
#' @examples
#' most basic use is to call a book and a chapter
#' get_chapters("analects", "xue-er")
#' get_chapters("analects", 1)
#' Can accept multiple chapters in a book, e.g.
#' get_chapters("analects", c("xue-er", "wei-zheng"))
#' get_chapters("analects", 1:2)
#' can specify multiple books and chapters
#' get_chapters(c("analects", "mengzi"), c("xue-er", "liang-hui-wang-i"))
#' get_chapters(c("analects", "mengzi", 1:2))

get_chapters <- function(book, chapter) {
  if (length(book) < length(chapter)) {
    book <- rep(book, length(chapter))
  }
  if (length(book) > length(chapter)) {
    chapter <- rep(chapter, length(book))
  }
  do.call(rbind, purrr::map2(book, chapter, ~ get_chapter(..1, ..2)))
}

#' @title get_books - Get an entire book or several books in book_list
#' @description A wrapper around get_text that downloads an entire book or books.
#' Different from get_chapters in that it directly downloads all chapters of specified
#' books.
#' @param book name or vector of book names
#' @export TRUE
#' @return Returns a dataframe with 4 columns: text, book, chapter in English, & chapter in Chinese.
#' In the case that the connection experiences a problem or you meet your daily API imit,
#' an NA value will be returned in the "text" column.
#' @examples
#' get_books("analects")
#' get_books("dao-de-jing")
#' On average you can download around 3 books before hitting the API limit.
#' my_booklist <- c("analects", "mengzi", "zhuangzi")
#' my_books <- get_books(my_booklist)

get_books <- function(...) {
  books <- c(...)
  if(!all(books %in% ctextclassics::book_list$book)|length(books) == 0) {
    missing_book <- setdiff(books, ctextclassics::book_list$book)
    print(missing_book)
    stop("Books unable to be indexed. Print 'book_list$book' for details\n")
  }
  selected_books <- book_list[book_list$book %in% books, ]
  my_books <- purrr::map2(selected_books$book, selected_books$chapter, ~get_chapters(..1, ..2))
  data.frame(do.call(rbind, my_books), stringsAsFactors = FALSE)
}



