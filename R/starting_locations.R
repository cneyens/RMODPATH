
#' Read starting locations of a particle group
#'
#' \code{rmp_read_starting_locations} reads the starting locations of a particle group and returns it as a \code{sloc} object
#'
#' @param file path to the starting locations file. Only necessary when locations are specified in an EXTERNAL file
#' @param remaining_lines character vector with remaining lines of the Simulation File. Only necessary when locations are specified using the INTERNAL keyword.
#' @param pathname when reading from file, the path specified in the Simulation File, e.g. the basename. Defaults to using \code{file}
#' @param ... ignored
#' @details Either \code{file} or \code{remaining_lines} should be specified, but not both.
#' @return If read from \code{file}, a \code{sloc} object containing the starting locations of the particle group. If read from \code{remaining_lines}, a list with the \code{sloc} object and the \code{remaining_lines} of the input vector.
#' @export
#' @seealso \code{\link{rmp_create_sloc}}, \code{\link{rmp_write_sloc}}
#'
#' @examples
rmp_read_sloc <- function(file = NULL, remaining_lines = NULL, pathname = file, ...) {

  if((is.null(file) + is.null(remaining_lines)) %in% c(0,2)) stop('Either file or remaining_lines should be specified', call. = FALSE)
  rtn_lines <- FALSE

  if(!is.null(file)) {
    sloc_lines <- readr::read_lines(file, lazy = FALSE)

  } else if(!is.null(remaining_lines)) {
    sloc_lines <- remaining_lines
    rtn_lines <- TRUE
  }

  # data set 0
  data_set_0 <- rmpi_parse_comments(sloc_lines)
  comments <- data_set_0$comments
  sloc_lines <- data_set_0$remaining_lines
  rm(data_set_0)

  # set INTERNAl or EXTERNAL
  if(!is.null(file)) {
    ext_file <- pathname
  } else if(!is.null(remaining_lines)) {
    ext_file <- NULL
  }

  # data set 1
  data_set_1 <- rmpi_parse_variables(sloc_lines)
  inputstyle <- as.numeric(data_set_1$variables[1])
  sloc_lines <- data_set_1$remaining_lines
  rm(data_set_1)

  if(inputstyle == 1) {

    # data set 2
    data_set_2 <- rmpi_parse_variables(sloc_lines)
    locationstyle <- as.numeric(data_set_2$variables[1])
    sloc_lines <- data_set_2$remaining_lines
    rm(data_set_2)

    # data set 3
    data_set_3 <- rmpi_parse_variables(sloc_lines)
    particlecount <- as.numeric(data_set_3$variables[1])
    particleidoption <- as.numeric(data_set_3$variables[2])
    sloc_lines <- data_set_3$remaining_lines
    rm(data_set_3)

    # data set 4 - 5
    if(locationstyle == 1) {

      if(particleidoption == 1) {
        nvar <- 9
        df_names <- c('id', 'layer', 'row', 'column', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      } else if(particleidoption == 0){
        nvar <- 8
        df_names <- c('layer', 'row', 'column', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      }

    } else if(locationstyle == 2) {

      if(particleidoption == 1) {
        nvar <- 7
        df_names <- c('id', 'cellnumber', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      } else if(particleidoption == 0){
        nvar <- 6
        df_names <- c('cellnumber', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      }
    }

    df <- readr::read_table(I(sloc_lines), col_names = FALSE, col_types = readr::cols(.default = 'd'), n_max = particlecount)
    df <- df[,1:nvar]
    names(df) <- df_names
    sloc_lines <- sloc_lines[-c(1:particlecount)]

    sloc <- rmp_create_sloc(df, inputstyle = inputstyle, ext_file = ext_file)

    # TODO additional input styles
  } else if(inputstyle == 2) {
    stop('Starting locations input style 2 not yet supported', call. = FALSE)
  } else if(inputstyle == 3) {
    stop('Starting locations input style 3 not yet supported', call. = FALSE)
  } else if(inputstyle == 4) {
    stop('Starting locations input style 4 not yet supported', call. = FALSE)
  }

  comment(sloc) <- comments

  if(rtn_lines) {
    return(list(starting_locations = sloc, remaining_lines = sloc_lines))
  } else {
    return(sloc)
  }

}

#' Write starting locations of a particle group
#'
#' \code{rmp_write_starting_locations} writes the starting locations of a particle group based on an \code{RMODPATH} sloc object to either a Simulation File or an external ASCII file.
#'
#' @param sloc a \code{RMODPATH} sloc object
#' @param file path to write to
#' @param append logical, should the file be appended? Defaults to TRUE
#' @param ... ignored
#'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmp_create_sloc}}, \code{\link{rmp_read_sloc}}
#'
#' @examples
rmp_write_sloc <- function(sloc, file, append = sloc$startinglocationsfileoption == 'INTERNAL', ...) {

  if(!append) { # EXTERNAL
    # data set 0
    v <- packageDescription("RMODPATH")$Version
    cat(paste('# MODPATH Starting Locations Data File created by RMODPATH, version',v,'\n'), file=file)
    cat(paste('#', comment(sloc)), sep='\n', file=file, append=TRUE)
  }

  # data set 1
  rmpi_write_variables(sloc$inputstyle, file=file, integer = TRUE)

  if(sloc$inputstyle == 1) {
    # data set 2
    rmpi_write_variables(sloc$locationstyle, file=file, integer = TRUE)

    # data set 3
    rmpi_write_variables(sloc$particlecount, sloc$particleidoption, file=file, integer = TRUE)

    # data set 5
    if(sloc$locationstyle == 1) {
      cls <- c('layer', 'row', 'column', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      if(sloc$particleidoption == 1) {
        cls <- c('id', cls)
      } else if(sloc$particleidoption != 0) {
        stop('ParticleIdOption should be 0 or 1', call. = FALSE)
      }


    } else if(sloc$locationstyle == 2) {
      cls <- c('cellnumber', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
      if(sloc$particleidoption == 1) {
        cls <- c('id', cls)
      } else if(sloc$particleidoption != 0) {
        stop('ParticleIdOption should be 0 or 1', call. = FALSE)
      }
    } else {
      stop('LocationStyle should be 1 or 2', call. = FALSE)
    }

    if(!all(cls %in% colnames(sloc$data))) stop(paste0('Starting locations should have following columns: ', cls), call. = FALSE)
    readr::write_delim(sloc$data[1:sloc$particlecount,cls], file = file, delim = ' ', col_names = FALSE, append = TRUE)

  } else if(sloc$inputstyle == 2) {
    stop('Starting locations input style 2 not yet supported', call. = FALSE)
  } else if(sloc$inputstyle == 3) {
    stop('Starting locations input style 3 not yet supported', call. = FALSE)
  } else if(sloc$inputstyle == 4) {
    stop('Starting locations input style 4 not yet supported', call. = FALSE)
  }

}

#' Create a RMODPATH starting locations object
#'
#' \code{rmp_create_sloc} creates a \code{RMODPATH} sloc object.
#'
#' @param df data.frame with columns specifying starting location information. See details.
#' @param inputstyle integer, type of starting locations input style. Currently, only inputstyle = 1 is supported.
#' @param ext_file if \code{NULL} (default), when writing starting locations, the sloc object is written inside the Simulation File. If a character, specifies the path to the external file to which the sloc object will be written.
#' @param ... ignored
#'
#' @details For inputstyle = 1, \code{df} should have following columns: (layer, row, column) OR (cellnumber), localx, localy, localz, timeoffset, drape. An optional id column may also be specified.
#'          \code{\link{rmp_get_locations}} may be used to obtain this data from given x, y and z coordinates.
#'
#' @return Object of class sloc
#' @export
#' @seealso \code{\link{rmp_write_sloc}}, \code{\link{rmp_read_sloc}} and \code{\link{rmp_get_locations}}
#'
#' @examples
rmp_create_sloc <- function(df,
                            inputstyle = 1,
                            ext_file = NULL,
                            ...) {

  if(inputstyle != 1) stop('Starting locations input style other than 1 not yet supported', call. = FALSE)

  sloc <- list()
  sloc$startinglocationsfileoption <- ifelse(is.null(ext_file), 'INTERNAL', 'EXTERNAL')
  sloc$startinglocationsfilename <- file.path(ext_file)

  sloc$inputstyle <- 1 # TODO support other input styles
  sloc$locationstyle <- ifelse('cellnumber' %in% colnames(df), 2, 1)
  sloc$particlecount <- nrow(df)
  sloc$particleidoption <- ifelse('id' %in% colnames(df), 1, 0)
  if(sloc$locationstyle == 1) {
    cls <- c('layer', 'row', 'column', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
  } else {
    cls <- c('cellnumer', 'localx', 'localy', 'localz', 'timeoffset', 'drape')
  }
  if(sloc$particleidoption == 1) {
    cls <- c('id', cls)
  }
  if(!all(cls %in% colnames(df))) stop(paste('sloc should have following columns:', paste(cls, collapse = ' ')), call. = FALSE)
  sloc$data <- df[, cls]

  class(sloc) <- c('sloc', 'rmp_object')
  return(sloc)
}

#' Create a RMODPATH particle group object
#'
#' \code{rmp_create_particlegroup} creates a \code{RMODPATH} particle_group object.
#'
#' @param sloc \code{RMODPATH} sloc object specyfing the starting locations
#' @param name character, particle group name
#' @param releaseoption integer specifying the release option
#' @param releasetimes single numeric value when releaseoption = 1 or numeric vector when releaseoption = 3; specifies the releasetimes.
#' @param timecount numeric; number of multiple releasetimes when releaseoption = 2. When releaseoption = 3, this is obtained from the length of releasetimes.
#' @param initialreleasetime numeric; value of tracking time at which first particles are released when releaseoption = 2.
#' @param interval numeric; uniform time interval between particle releases when releaseoption = 2.
#'
#' @return an object of class particle_group
#' @export
#'
#' @examples
rmp_create_particlegroup <- function(sloc,
                                     name,
                                     releaseoption,
                                     releasetimes,
                                     timecount,
                                     initialreleasetime,
                                     interval) {

  grp <- list()
  grp$groupname <- as.character(name)
  grp$releaseoption <- releaseoption
  if(grp$releaseoption == 1) {
    if(length(releasetimes) > 1) stop('When releaseoption = 1, releasetimes should have length 1', call. = FALSE)
    grp$releasetime <- releasetimes
  } else if(grp$releaseoption == 2) {
    grp$releasetimecount <- timecount
    grp$initialreleasetime <- initialreleasetime
    grp$releaseinterval <- interval
  } else if(grp$releaseoption == 3) {
    grp$releasetimecount <- length(releasetimes)
    grp$releasetimes <- releasetimes
  } else {
    stop('releaseoption should be 1, 2 or 3', call. = FALSE)
  }

  if(!inherits(sloc, 'sloc')) stop('sloc should be a RMODPATH sloc object', call. = FALSE)
  grp$startinglocations <- sloc

  class(grp) <- c('particle_group', 'rmp_object')
  return(grp)
}

#' Obtain particle starting locations from x, y and z coordinates
#'
#' \code{rmp_get_locations} obtains the starting locations of particles from an object with x, y and z coordinates
#'
#' @param obj data.frame with x, y and z columns or an {sf}, {sfc} or {sfg} XY or XYZ POINT or MULTIPOINT object. See details.
#' @param dis \code{RMODFLOW} dis object
#' @param prj optional \code{RMODFLOW} prj object; required when obj contains real-world coordinates instead of MODFLOW grid coordinates
#' @param locationstyle integer specifying the locationstyle to be used. The default (1) uses layer, row and column indices; the other (2) a cellnumber index.
#' @param timeoffset optional numeric column specifying the timeoffset. This is required by \code{\link{rmp_create_sloc}} when using the outputted tibble as the \code{sloc} argument.
#' @param drape optional integer column specifying the drape option. This is required by \code{\link{rmp_create_sloc}} when using the outputted tibble as the \code{sloc} argument.
#' @param id optional integer column specifying the particle id. This is an optional argument by \code{\link{rmp_create_sloc}} when using the outputted tibble as the \code{sloc} argument
#' @param ... ignored
#'
#' @details When obj is an \code{sf}, \code{sfc} or \code{sfg} object with dimensions XY, an additional z column should be specified. The \code{sf} package should be installed when obj is of class \code{sf}, \code{sfc} or \code{sfg}.
#'
#' @return a tibble with layer, row, column, localx, localy and localz columns when inputstyle = 1 or cellnumber, localx, localy and localz columns when inputstyle = 2,
#'         corresponding to the given coordinates in obj. When timeoffset and drape (and optionally, id) columns are added, this can be used as the \code{sloc} argument in \code{\link{rmp_create_sloc}} to create a starting locations object.
#' @export
#' @seealso \code{\link{rmp_create_sloc}}
#'
#' @examples
rmp_get_locations <- function(obj,
                              dis,
                              prj = RMODFLOW::rmf_get_prj(dis),
                              locationstyle = 1,
                              timeoffset = NULL,
                              drape = NULL,
                              id = NULL,
                              ...) {

  if(inherits(obj, 'sf') | inherits(obj, 'sfc') | inherits(obj, 'sfc')) {
    if(!requireNamespace('sf', quietly = TRUE)) stop('Please install the {sf} package when obj is an sf or sfc object', call. = FALSE)
    if(class(sf::st_geometry(obj)[[1]])[1] == "XYZ") {
      coords <- sf::st_coordinates(obj)
      x <- coords[,1]
      y <- coords[,2]
      z <- coords[,3]
    } else if(class(sf::st_geometry(obj)[[1]])[1] == "XY") {
      if(is.null(obj$z)) stop('If POINT geometries do not have dimension XYZ, a z column should be specified', call. = FALSE)
      coords <- sf::st_coordinates(obj)
      x <- coords[,1]
      y <- coords[,2]
      z <- obj$z
    } else {
      stop('Only sf POINT and MULTIPOINT geometries of dimensions XY or XYZ are supported.', call. = FALSE)
    }
  } else {
    if(is.null(obj$x) | is.null(obj$y) | is.null(obj$z)) stop('x, y, and z columns should be specified in obj', call. = FALSE)
    x <- obj$x
    y <- obj$y
    z <- obj$z
  }

  lcs <- RMODFLOW::rmf_convert_xyz_to_grid(dis = dis, prj = prj, x = x, y = y, z = z, output = c('ijk', 'off'))
  # off is between -0.5 & 0.5 but localx, y & z should be between 0 and 1
  lcs$roff <- -lcs$roff + 0.5 # = localy, in positive y direction
  lcs$coff <- lcs$coff + 0.5 # = localx, in positive x direction
  lcs$loff <- -lcs$loff + 0.5 # = localz, in positive z direction

  if(locationstyle == 1) {
    lcs <- lcs[,c('k', 'i', 'j', 'coff', 'roff', 'loff')]
    lcs <- setNames(lcs, c('layer', 'row', 'column', 'localx', 'localy', 'localz'))
  } else if(locationstyle == 2) {
    lcs$cellnumber <- RMODFLOW::rmf_convert_ijk_to_id(i = lcs$i, j = lcs$j, k = lcs$k, dis = dis, type = 'modflow')
    lcs <- lcs[,c('cellnumber', 'coff', 'roff', 'loff')]
    lcs <- setNames(lcs, c('cellnumber', 'localx', 'localy', 'localz'))
  } else {
    stop('locationstyle should be 1 or 2', call. = FALSE)
  }

  if(!is.null(timeoffset)) lcs$timeoffset <- timeoffset
  if(!is.null(drape)) lcs$drape <- drape
  if(!is.null(id)) {
    cls <- colnames(lcs)
    lcs$id <- id
    lcs <- lcs[,c('id', cls)]
  }
  return(tibble::as_tibble(lcs))
}


