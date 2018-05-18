#' @title download a chapter of a Chinese book on ctext.org
#' @description Download a chapter's worth of text from a Chinese book available in book_list.
#' @param book name of book
#' @param chapter name or number of chapter in book
#' @export TRUE
#' @return Returns a dataframe with 4 columns: text, book, chapter name in English, & chapter name in Chinese.
#'   In the case for the dao-de-jing, only book arg is needed. In the case you reach the API call limit or
#'   the connection has an error, an NA value will be returned in the "text" column.
#' @examples
#' get_text("analects", "xue-er")
#' get_text("analects", 1)
#' get_text("dao-de-jing")

get_text <- function(book, chapter = NA) {
  counter <- 0
  ok <- FALSE

  if(!is.character(book)) stop("Book must be in lower case, hyphenated text, ie 'dao-de-jing'")
  if(is.numeric(chapter)) {
    chapter <- book_list[c("book", "chapter")][book_list$book == book, ][chapter, 2]
  }

  if(book == "dao-de-jing") {
    path <- paste("/gettext?urn=", "ctp:", book, sep = "")
  } else {
    path <-  paste("/gettext?urn=", "ctp:", book, "/", chapter[[1]], sep = "")
  }

  while(ok == FALSE & counter < 3) {
    raw_data <- call.and.count(path)
    counter <- counter + 1

    if(grepl("requires|request", raw_data[[1]][1], ignore.case = TRUE)) {
      cat(" API error, trying", 3-counter, "more times\n", sep = " ")
      raw_data <- list(text = NA, chapter = chapter)
      Sys.sleep(2)
    } else {
      ok <- TRUE
    }
  }
  setNames(data.frame(matrix(do.call(rbind, raw_data), ncol = 2,
                             byrow = TRUE), book, chapter, stringsAsFactors = FALSE), c("text", "chapter_cn", "book", "chapter_en"))
}

#' @title Get an entire book that is in book_list
#' @description A wrapper around get_text() that downloads an entire book.
#' @param a vector detailing a book or multiple books.
#' @export
#' @return Returns a dataframe with 4 columns: text, book, chapter in English, & chapter in Chinese.
#' In the case that the connection experiences a problem or you meet your daily API imit,
#' an NA value will be returned in the "text" column.
#' @examples
#' get_books("analects")
#' get_books("dao-de-jing")
#' #Be careful not to download too many books at once - you may hit the API limit.
#' my_booklist <- c("analects", "mengzi", "zhuangzi")
#' my_books <- get_books(my_booklist)
#'
#' # For "Mozi" you will only be allowed to download Chapter 1 and none of the other books,
#' #unless you register.
#'
get_books <- function(...) {
  books <- c(...)
  if(!all(books %in% book_list$book)|length(books) == 0) {
    missing_book <- setdiff(books, book_list$book)
    print(missing_book)
    stop("Books unable to be indexed. Print 'book_list$book' for details\n")
  }
  selected_books <- book_list[book_list$book %in% books, ]
  my_books <- purrr::map2(selected_books$book, selected_books$chapter, ~get_text(..1, ..2))
  data.frame(do.call(rbind, my_books), stringsAsFactors = FALSE)
}
#' @title Get total API call count
#' @description Get the amount of times you have called the ctext.org API.
#' @export
#' @return an integer
#' @examples get_calls()
#'
get_calls <- function() {
  if(!exists("counter.env")) {
    counter.env <<- new.env()
    counter.env$i <- 0 }
  return(counter.env$i)
}
