
#' Read a MODPATH Basic Data File
#'
#' \code{rmp_read_bas} reads a MODPATH Basic Data file and returns at as a \code{mpbas} object
#'
#' @param file path to the Basic Data file
#' @param dis \code{RMODFLOW} dis object
#' @param style either '2005', 'usg' or '6' denoting the type of basic file to be read. Currently, only '2005' is supported.
#' @param ... ignored
#'
#' @return a \code{RMODPATH} mpbas object
#' @export
#' @seealso \code{\link{rmp_write_bas}}, \code{\link{rmp_create_bas}}
#'
#' @examples
rmp_read_bas <- function(file, dis, style = '2005', ...) {

  # TODO better error check for MODFLOW-USG or MODFLOW-6 style input
  if(as.character(style) != '2005') stop('Reading of MODPATH Basic Data Files is currently only supported for simulations based on MODFLOW-2005',
                                         call. = FALSE)

  bas_lines <- readr::read_lines(file, lazy = FALSE)
  bas <- list()

  # data set 0
  data_set_0 <- rmpi_parse_comments(bas_lines)
  comment(bas) <- data_set_0$comments
  bas_lines <- data_set_0$remaining_lines
  rm(data_set_0)

  # data set 1
  data_set_1 <- rmpi_parse_variables(bas_lines)
  bas$hnoflo <- as.numeric(data_set_1$variables[1])
  bas$hdry <- as.numeric(data_set_1$variables[2])
  bas_lines <- data_set_1$remaining_lines
  rm(data_set_1)

  # data set 2
  data_set_2 <- rmpi_parse_variables(bas_lines)
  bas$defaultifacecount <- as.numeric(data_set_2$variables[1])
  bas_lines <- data_set_2$remaining_lines
  rm(data_set_2)

  if(bas$defaultifacecount > 0) {
    bas$packagelabel <- vector(mode = 'character', length = bas$defaultifacecount)
    bas$defaultifacevalue <- vector(mode = 'integer', length = bas$defaultifacecount)

    for(i in 1:bas$defaultifacecount) {
      # data set 3
      data_set_3 <- rmpi_parse_variables(bas_lines, character = TRUE)
      bas$packagelabel[i] <- as.character(data_set_3$variables[1])
      bas_lines <- data_set_3$remaining_lines
      rm(data_set_3)

      # data set 4
      data_set_4 <- rmpi_parse_variables(bas_lines)
      bas$defaultifacevalue[i] <- as.numeric(data_set_4$variables[1])
      bas_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
  }

  # data set 5
  data_set_5 <- rmpi_parse_variables(bas_lines, nlay = dis$nlay)
  bas$laytyp <- as.numeric(data_set_5$variables[1:dis$nlay])
  bas_lines <- data_set_5$remaining_lines
  rm(data_set_5)

  # data set 6
  data_set_6 <- rmpi_parse_array(bas_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = dis$nlay, ndim = 3, file = file, integer = TRUE)
  bas$ibound <- RMODFLOW::rmf_create_array(apply(data_set_6$array, MARGIN = 1:length(dim(data_set_6$array)), function(i) as.integer(i)),
                                           dim =  c(dis$nrow, dis$ncol, dis$nlay),
                                           dimlabels = c("i", "j", "k"))
  bas_lines <- data_set_6$remaining_lines
  rm(data_set_6)

  # data set 7
  data_set_7 <- rmpi_parse_array(bas_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = dis$nlay, ndim = 3, file = file)
  bas$porosity <- data_set_7$array
  bas_lines <- data_set_7$remaining_lines
  rm(data_set_7)

  class(bas) <- c('mpbas', 'rmp_package')
  return(bas)
}

#' Write a MODPATH Basic Data File
#'
#' \code{rmp_write_bas} writes a MODPATH Basic Data File based on an \code{RMODPATH} mpbas object.
#'
#' @param bas a \code{RMODPATH} mpbas object
#' @param file filename to write to; typically '*.mpbas'
#' @param ... additional arguments passed to \code{rmpi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmp_read_bas}}, \code{\link{rmp_create_bas}}
#'
#' @examples
rmp_write_bas <- function(bas, file, ...) {

  # data set 0
  v <- packageDescription("RMODPATH")$Version
  cat(paste('# MODPATH Basic Data File created by RMODPATH, version',v,'\n'), file=file)
  cat(paste('#', comment(bas)), sep='\n', file=file, append=TRUE)

  # data set 1
  rmpi_write_variables(bas$hnoflo, bas$hdry, file=file)

  # data set 2
  rmpi_write_variables(bas$defaultifacecount, file = file)

  # data set 3 & 4
  if(bas$defaultifacecount > 0) {
    for(i in 1:bas$defaultifacecount) {
      rmpi_write_variables(as.character(bas$packagelabel[i]), file = file)
      rmpi_write_variables(as.integer(bas$defaultifacevalue[i]), file = file, integer = TRUE)
    }
  }

  # data set 5
  rmpi_write_variables(bas$laytyp, file = file, integer = TRUE)

  # data set 6
  rmpi_write_array(bas$ibound, file = file, ...)

  # data set
  rmpi_write_array(bas$porosity, file = file, ...)
}

#' Create a RMODPATH mpbas object
#'
#' \code{rmp_create_bas} creates a \code{RMODPATH} mpbas object.
#'
#' @param dis \code{RMODFLOW} dis object, required to obtain dimensions of ibound and porosity arrays.
#' @param hnoflo numeric; value of head used in the MODFLOW simulation to represent inactive cells
#' @param hdry numeric; values of thead used in the MODFLOW simulation to represent dry cells
#' @param packagelabel optional character vector; text string used by MODFLOW in the budget output file to label flow rates for a stress package. Should have the same length as defaultifacevalue.
#' @param defaultifacevalue optional integer vector; with the IFACE value of the corresponding packagelabel stress package. Value must be in range of 0 to 6. Should have the same length as packagelabel.
#' @param laytyp numeric vector with length equal to the number of MODFLOW layers; specifies the layertype for each MODFLOW layer. 0 = confined, 1 = convertible. See details.
#' @param ibound 3D integer array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} corresponding to the MODFLOW grid; specifies the active and inactive cells in the MODFLOW simulation. See details.
#' @param porosity numeric array of dimensions \code{dis$nrow x dis$ncol x dis$nlay} specifying the effective porosity for the MODPATH simulation
#' @param bas optional \code{RMODFLOW} bas object. See details.
#' @param flow optional \code{RMODFLOW} flow object of class \code{lpf}, \code{upw}, \code{bcf} or \code{huf}. See details.
#' @param modflow optional \code{RMODFLOW} modflow object. See details.
#'
#' @details When a \code{RMODFLOW} bas object is supplied, hdry and ibound are obtained from that object.
#'  When a \code{RMODFLOW} flow package object (lpf, upw, bcf or huf) is supplied, laytyp and hnoflo are obtained from that object.
#'  When a \code{RMODFLOW} modflow object is supplied, the \code{RMODFLOW} bas and flow objects are obtained from that object.
#'
#' @return Object of class mpbas
#' @export
#' @seealso \code{\link{rmp_read_bas}}, \code{\link{rmp_write_bas}}
#'
#' @examples
rmp_create_bas <- function(dis,
                           hnoflo,
                           hdry,
                           packagelabel = NULL,
                           defaultifacevalue = NULL,
                           laytyp,
                           ibound,
                           porosity,
                           bas = NULL,
                           flow = NULL,
                           modflow = NULL) {

  # set arguments from supplied RMODFLOW packages

  if(!is.null(modflow)) {
    if(!inherits(modflow, c('modflow'))) stop('Supplied modflow object should be a RMODFLOW modflow object')
    hnoflo <- modflow$bas$hnoflo
    ibound <- modflow$bas$ibound
  } else if(!is.null(bas)) {
    if(!inherits(bas, 'rmf_package') | !inherits(bas, 'bas')) stop('Supplied bas object should be a RMODFLOW bas package object')
    hnoflo <- bas$hnoflo
    ibound <- bas$ibound
  }

  if(!is.null(modflow)) {
    ftype <- vapply(modflow, function(i) class(i)[which(class(i) == 'rmf_package') - 1], 'text')
    flow_pckg <- which(ftype %in% c('lpf', 'upw', 'bcf', 'huf'))
    if(length(flow_pckg) != 1) stop('Supplied modflow object should have 1 and only 1 flow package', call. = FALSE)
    flow <- modflow[[flow_pckg]]
  }
  if(!is.null(flow)) {
    if(!(inherits(flow, 'rmf_package')) | !any(c('lpf', 'upw', 'huf', 'bcf') %in% class(flow))) {
      stop('Supplied flow object should be a RMODFLOW lpf, upw, huf or bcf package', call. = FALSE)
    }
    hdry <- flow$hdry
    if('bcf' %in% class(flow)) {
      laytyp <- ifelse(flow$laycon > 0, 1, 0)
    } else if('huf' %in% class(flow)) {
      laytyp <- ifelse(flow$lthuf == 0, 0, 1)
    } else {
      laytyp <- ifelse(flow$laytyp == 0, 0, 1)
    }
  }

  mpbas <- list()

  # data set 1
  mpbas$hnoflo <- hnoflo
  mpbas$hdry <- hdry

  # data set 2 - 4
  chck <- is.null(packagelabel) + is.null(defaultifacevalue)
  if(chck == 1) stop('If packagelabel or defaultifacevalue is supplied, both must be supplied', call. = FALSE)
  if(chck == 2) {
    if(length(packagelabel) != length(defaultifacevalue)) stop('packagelabel and defaultifacevalue should have the same length', call. = FALSE)
    if(any(!(defaultifacevalue %in% c(0:6)))) stop('defaultifacevalues should be in range of 0 to 6', call. = FALSE)
    mpbas$defaultifacecount <- length(packagelabel)
    mpbas$packagelabel <- packagelabel # TODO check for valid value
    mpbas$defaultifacevalue <- defaultifacevalue

  } else {
    mpbas$defaultifacecount <- 0
  }

  # data set 5
  mpbas$laytyp <- laytyp

  # data set 6
  mpbas$ibound <- RMODFLOW::rmf_create_array(as.integer(ibound), dim = c(dis$nrow, dis$ncol, dis$nlay))

  # data set 7
  mpbas$porosity <- RMODFLOW::rmf_create_array(porosity, dim = c(dis$nrow, dis$ncol, dis$nlay))

  class(mpbas) <- c('mpbas', 'rmp_package')
  return(mpbas)
}
