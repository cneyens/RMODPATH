#' Read comments
#' Internal function used in the read_* functions to read comments
#' @details removes empty comments and prevents copying of RMODPATH header comment
#' @keywords internal
rmpi_parse_comments <- function(remaining_lines) {
  v <- paste("RMODPATH, version",  packageDescription("RMODPATH")$Version)
  comments <- NULL
  comment_tag <- substr(remaining_lines, 1, 1)
  comment_id <- which(comment_tag %in% c("#", '!', '//'))

  if(length(comment_id) > 0) {
    comments <- gsub('#|!|//', '', remaining_lines[comment_id])

    # remove empty comments
    empty <- which(nchar(trimws(comments)) == 0)
    if(length(empty) > 0) comments <- comments[-empty]

    # remove RMODPATH header
    header <- grep(v, comments)
    if(length(header) > 0) comments <- comments[-header]

    remaining_lines <- remaining_lines[-comment_id]
  }

  return(list(comments = comments, remaining_lines = remaining_lines))
}

#' Read MODPATH variables
#' If all are numbers, returns numeric, otherwise returns character vector
#' @param format character, either \code{'free'} (default) or \code{'fixed'}. When 'fixed', reads 10-character fields and converts to numeric. Empty fields are set to zero.
#' @param ... additional arguments passed to \link[RMODFLOW]{rmfi_parse_variables}
#' @keywords internal
rmpi_parse_variables <- function(remaining_lines, format = 'free', ...) {
  RMODFLOW:::rmfi_parse_variables(remaining_lines, format = format, ...)
}

#' Get an array specified by a control record from the text lines analyzed in an \code{RMODPATH} \code{rmp_read.*} function
#' @param remaining_lines lines to read the array from
#' @param nrow number of rows in the array
#' @param ncol number of columns in the array
#' @param nlay number of layers in the array that should be read
#' @param ndim dimensions of the array to read; either 1, 2 or 3. Denotes the if the returned array should be 1D, 2D or 3D.
#' @param ... additional arguments passed to \link[RMODFLOW]{rmfi_parse_array}
#' @return A list containing the array and the remaining text of the MODPATH input file
#' @keywords internal
rmpi_parse_array <- function(remaining_lines,nrow,ncol,nlay, ndim, ...) {
  RMODFLOW:::rmfi_parse_array(remaining_lines, nrow = nrow, ncol = ncol, nlay = nlay, ndim = ndim, ...)
}

#' Write MODPATH variables
#' Internal function used in the rmp_write_* functions for writing variables
#' @keywords internal
rmpi_write_variables <- function(..., file, append=TRUE, width = 10, format = 'free', integer = FALSE, iprn = -1) {
  RMODFLOW:::rmfi_write_variables(..., file = file, append = append, width = width, format = format, integer = integer, iprn = iprn)
}

#' Write MODPATH array
#' Internal function used in the rmp_write_* functions for writing array datasets
#' @param ... additional arguments passed to \link[RMODFLOW]{rmfi_write_array}
#' @details if the array should be written as integers, an integer array should be provided
#' @keywords internal
rmpi_write_array <- function(array, file, ...) {
  RMODFLOW:::rmfi_write_array(array = array, file = file, ...)
}
