
#' Read starting locations of a particle group
#'
#' \code{rmp_read_starting_locations} reads the starting locations of a particle group and returns it as a \code{sloc} object
#'
#' @param file path to the starting locations file. Only necessary when locations are specified in an EXTERNAL file
#' @param remaining_lines character vector with remaining lines of the Simulation File. Only necessary when locations are specified using the INTERNAL keyword.
#' @details Either \code{file} or \code{remaining_lines} should be specified, but not both.
#' @return If read from \code{file}, a \code{sloc} object containing the starting locations of the particle group. If read from \code{remaining_lines}, a list with the \code{sloc} object and the \code{remaining_lines} of the input vector.
#' @export
#'
#' @examples
rmp_read_starting_locations <- function(file = NULL, remaining_lines = NULL) {

  if((is.null(file) + is.null(remaining_lines)) %in% c(0,2)) stop('Either file or remaining_lines should be specified', call. = FALSE)
  rtn_lines <- FALSE

  if(!is.null(file)) {
    sloc_lines <- readr::read_lines(file)

  } else if(!is.null(remaining_lines)) {
    sloc_lines <- remaining_lines
    rtn_lines <- TRUE
  }

  sloc <- list()

  # data set 0
  data_set_0 <- rmpi_parse_comments(sloc_lines)
  comment(sloc) <- data_set_0$comments
  sloc_lines <- data_set_0$remaining_lines
  rm(data_set_0)

  # data set 1
  data_set_1 <- rmpi_parse_variables(sloc_lines)
  sloc$inputstyle <- as.numeric(data_set_1$variables[1])
  sloc_lines <- data_set_1$remaining_lines
  rm(data_set_1)

  if(sloc$inputstyle == 1) {

    # data set 2
    data_set_2 <- rmpi_parse_variables(sloc_lines)
    sloc$locationstyle <- as.numeric(data_set_2$variables[1])
    sloc_lines <- data_set_2$remaining_lines
    rm(data_set_2)

    # data set 3
    data_set_3 <- rmpi_parse_variables(sloc_lines)
    sloc$particlecount <- as.numeric(data_set_3$variables[1])
    sloc$particleidoption <- as.numeric(data_set_3$variables[2])
    sloc_lines <- data_set_3$remaining_lines
    rm(data_set_3)

    # data set 4 - 5
    if(sloc$locationstyle == 1) {

      if(sloc$particleidoption == 1) {
        nvar <- 9
        df_names <- c('id', 'layer', 'row', 'column', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      } else if(sloc$particleidoption == 0){
        nvar <- 8
        df_names <- c('layer', 'row', 'column', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      }

    } else if(sloc$locationstyle == 2) {

      if(sloc$particleidoption == 1) {
        nvar <- 7
        df_names <- c('id', 'cellnumber', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      } else if(sloc$particleidoption == 0){
        nvar <- 6
        df_names <- c('cellnumber', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      }
    }

    df <- readr::read_table(sloc_lines, col_names = FALSE, col_types = readr::cols(.default = 'd'), n_max = sloc$particlecount)
    df <- df[,1:nvar]
    names(df) <- df_names
    sloc_lines <- sloc_lines[-c(1:sloc$particlecount)]
    sloc$data <- df

    # TODO additional input styles

  } else if(sloc$inputstyle == 2) {
    stop('Starting locations input style 2 not yet supported', call. = FALSE)
  } else if(sloc$inputstyle == 3) {
    stop('Starting locations input style 3 not yet supported', call. = FALSE)
  } else if(sloc$inputstyle == 4) {
    stop('Starting locations input style 4 not yet supported', call. = FALSE)
  }




  class(sloc) <- c('sloc', 'modpath_object')

  if(rtn_lines) {
    return(list(starting_locations = sloc, remaining_lines = sloc_lines))
  } else {
    return(sloc)
  }

}
