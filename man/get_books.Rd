% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_text.R
\name{get_books}
\alias{get_books}
\title{get_books - Get an entire book or several books in book_list}
\usage{
get_books(...)
}
\arguments{
\item{book}{name or vector of book names}
}
\value{
Returns a dataframe with 4 columns: text, book, chapter in English, & chapter in Chinese.
In the case that the connection experiences a problem or you meet your daily API imit,
an NA value will be returned in the "text" column.
}
\description{
A wrapper around get_text that downloads an entire book or books.
Different from get_chapters in that it directly downloads all chapters of specified
books.
}
\examples{
get_books("analects")
get_books("dao-de-jing")
On average you can download around 3 books before hitting the API limit.
my_booklist <- c("analects", "mengzi", "zhuangzi")
my_books <- get_books(my_booklist)
}
