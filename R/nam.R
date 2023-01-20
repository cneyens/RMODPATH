
#' Read a MODPATH Name File
#'
#' \code{rmp_read_nam} reads a MODPATH Name File and returns at as a \code{mpnam} object
#'
#' @param file path to the Name File
#' @param ... ignored
#'
#' @return a \code{RMODPATH} mpnam object
#' @export
#' @seealso \code{\link{rmp_write_nam}}, \code{\link{rmp_create_nam}}
#'
#' @examples
rmp_read_nam <- function(file, ...) {

  nam <- list()
  lines <- readr::read_lines(file, lazy = FALSE)

  # top comments
  data_set_0 <- rmpi_parse_comments(lines)
  comments <- data_set_0$comments
  lines <- data_set_0$remaining_lines
  rm(data_set_0)

  indices <- rep(TRUE,length(lines))
  for(i in 1:length(lines)) {
    if(length(RMODFLOW:::rmfi_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])) == 0 || strsplit(RMODFLOW:::rmfi_remove_empty_strings(strsplit(lines[i],' |\t')[[1]])[1], "")[[1]][1] == "#") {
      comments <-  c(comments, gsub('#', '', lines[i]))
      indices[i] <- FALSE
    } else {
      lines[i] <- RMODFLOW:::rmfi_remove_comments_end_of_line(lines[i])
    }
  }

  nam_lines <- lines[indices]
  nam_lines <- lapply(strsplit(nam_lines, ' |\t'), RMODFLOW:::rmfi_remove_empty_strings)
  nam_lines <- lapply(nam_lines, function(i) RMODFLOW:::rmfi_ifelse0(length(unlist(i)) < 3, c(unlist(i)[1],NA, unlist(i)[2]), unlist(i)))

  nam <- data.frame(do.call(rbind, nam_lines), stringsAsFactors = FALSE)
  colnames(nam) <- c('ftype','nunit','fname')
  nam$nunit<- as.numeric(nam$nunit)
  nam$fname <- gsub('\"|\'', '', nam$fname)
  nam$ftype <- toupper(nam$ftype)
  if(all(is.na(nam$nunit))) nam <- nam[,-which(colnames(nam) == 'nunit')] # TODO maybe always keep or always drop nunit?

  comment(nam) <- comments
  class(nam) <- c('mpnam', 'rmp_package', 'data.frame')
  return(nam)

}

#' Write a MODPATH Name File
#'
#' \code{rmp_write_nam} writes a MODPATH Name File based on an \code{RMODPATH} mpnam object.
#'
#' @param nam a \code{RMODPATH} mpnam object
#' @param file filename to write to; typically '*.mpnam'
#' @param ... ignored
#'
#' @return \code{NULL}
#'
#' @export
#' @seealso \code{\link{rmp_read_nam}}, \code{\link{rmp_create_nam}}
#'
#' @examples
rmp_write_nam <- function(nam, file, ...) {

  if(!is.null(nam$nunit)) {
    if(length(unique(nam$nunit)) < nrow(nam)) stop('Please make sure every file has a unique nunit specified', call. = FALSE)
    nam$nunit <- as.integer(nam$nunit)
  }

  # check for spaces in fname
  # if(any(grepl(' |\t', nam$fname))) stop('Whitespaces are not allowed in fname', call. = FALSE)

  # data set 0
  v <- packageDescription("RMODPATH")$Version
  cat(paste('# MODPATH Name File created by RMODPATH, version',v,'\n'), file=file)
  cat(paste('#', comment(nam)), sep='\n', file=file, append=TRUE)

  # data set 1
  # write.table(nam, file = file, row.names = FALSE, col.names = FALSE, quote = FALSE, na='', append=TRUE)
  # readr::write_tsv(nam, file = file, append = TRUE, col_names = FALSE, quote = 'none', escape = 'none', na = '')
  readr::write_delim(nam, file = file, append = TRUE, col_names = FALSE, quote = 'none', escape = 'none', na = '', delim = ' ')
}

#' Create a RMODPATH mpnam object
#'
#' \code{rmp_create_nam} creates a \code{RMODPATH} mpnam object.
#'
#' @param dis_file path to the dis file to be used in the MODPATH simulation. Only a MODFLOW-2005 or NWT DIS file is currently supported.
#' @param mpbas_file path to the mpbas file to be used in the MODPATH simulation
#' @param head_file path to the MODFLOW head output file to be used in the MODPATH simulation
#' @param budget_file path to the MODFLOW cell-by-cell budget file to be used in the MODPATH simulation
#' @param unit_numbers optional; integer vector of length 4 specifying the unit numbers. Only required for backward compatability with older MODPATH versions.
#'
#' @return Object of class mpnam
#' @export
#' @seealso \code{\link{rmp_read_nam}}, \code{\link{rmp_write_nam}}
#'
#' @examples
rmp_create_nam <- function(dis_file,
                           mpbas_file,
                           head_file,
                           budget_file,
                           unit_numbers = NULL) {

  nam <- data.frame(ftype = c('DIS', 'MPBAS', 'HEAD', 'BUDGET'),
                    filename = c(dis_file, mpbas_file, head_file, budget_file),
                    stringsAsFactors = FALSE)
  if(!is.null(unit_numbers)) {
    nam <- data.frame(ftype = c('DIS', 'MPBAS', 'HEAD', 'BUDGET'),
                      nunit = unit_numbers,
                      filename = c(dis_file, mpbas_file, head_file, budget_file),
                      stringsAsFactors = FALSE)
  }
  class(nam) <- c('mpnam', 'rmp_package', 'data.frame')
  return(nam)
}
