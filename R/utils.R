#' Negation of the \code{in} operator
#'
#' Used for easier readability of code.
#'
#' \code{'Pikachu' \%!in\% c ('Squirtle','Bulbasaur','Charmander')}
#'
#' is equivalent to
#'
#' \code{!('Pikachu' \%in\% c('Squirtle','Bulbasaur','Charmander'))}
#'
#' @export
#' @name not_in
`%!in%` = Negate(`%in%`)

#' Utility function to print time before console messages
#'
#' usage is similar to \code{cat} from base \code{R}, but automatically prints the time in the console before output the messaging.
#' @export
#'
#' @param ... Arguments to pass to \code{cat}
#'
#' @examples
#' catTime('Hello World!')
catTime <- function(...){
  #adds a time stamp before catting
  cat(paste0('[',format(Sys.time(),'%H:%M:%S'),']'), ...,'\n')
}


#' Convenient wrapper to read a text file
#'
#' Function used to read a text file (mostly used for SQL queries). If \code{list.param} is not \code{NULL}, then it attemps to replace the string in the text files by the provided ones, otherwise returns as is.
#' @export
#'
#' @param f Valid file path of a text file, can be .sql, .txt, etc...
#' @param list.param \strong{Optional}. List containing the strings to replace in the text file. Defaults to \code{NULL}, i.e. no replacements.
#'
#' @examples
#' \dontrun{
#' read('query.sql',list.param = list(start.date = '2018-01-05')) #Replaces any 'start.date' substring in the query by '2018-01-05'}
read <- function(f,list.param = NULL){
  if(is.null(list.param)){
    return(readChar(f,nchars = file.size(f)))
  } else {
    query.txt <- readChar(f,nchars = file.size(f))
    for(param in names(list.param)){
      query.txt <- gsub(param,list.param[[param]],query.txt,fixed=T)
    }
    return(query.txt)
  }
}

#' Get a query from a SQL file if valid, pass as is if not.
#'
#' @param q SQL query or valid file path containing one
#' @param list.param list of param sent to the underlying \code{\link{read}} function.
#'
#' @return a string containing a SQL query.
#'
GetQueryFromInput <- function(q,list.param){
  q_low <- tolower(substr(q,nchar(q)-3,nchar(q)))
  if(file.exists(q)) {
    return(read(q,list.param))
  } else if(q_low == '.sql' | q_low == '.txt') {
    stop("It seems you intended to pass a .sql query as argument, but the file couldn't be found. Please make sure the path is correct and try again.")
  } else {
    return(q)
  }
}
