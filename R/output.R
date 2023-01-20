
#' Reads an endpoint output file
#'
#' \code{rmp_read_endpoint} reads an endpoint output file and returns it as a \code{RMODPATH} endpoint object
#'
#' @param file path tot the endpoint output file
#' @param ... ignored
#'
#' @return object of class endpoint
#' @export
#' @seealso \code{\link{rmp_read_pathline}}, \code{\link{rmp_read_timeseries}}
#' @examples
rmp_read_endpoint <- function(file, ...) {

  endp_lines <- readr::read_lines(file, lazy = FALSE)
  endp <- list()

  # data set 1
  data_set_1 <- rmpi_parse_variables(endp_lines, character = TRUE)
  label <- as.character(data_set_1$variables[1])
  version <- as.numeric(data_set_1$variables[2])
  revision <- as.numeric(data_set_1$variables[3])
  endp$header <- paste(label, 'Version', paste(version, revision, sep = '.'))
  endp_lines <- data_set_1$remaining_lines
  rm(data_set_1)

  # data set 2
  data_set_2 <- rmpi_parse_variables(endp_lines)
  endp$TrackingDirection <- as.numeric(data_set_2$variables[1])
  endp$TotalCount <- as.numeric(data_set_2$variables[2])
  endp$ReleaseCount <- as.numeric(data_set_2$variables[3])
  endp$MaximumID <- as.numeric(data_set_2$variables[4])
  endp$ReferenceTime <- as.numeric(data_set_2$variables[5])
  endp$XOrigin <- as.numeric(data_set_2$variables[6])
  endp$YOrigin <- as.numeric(data_set_2$variables[7])
  endp$AngRot <- as.numeric(data_set_2$variables[8])
  endp_lines <- data_set_2$remaining_lines
  rm(data_set_2)

  # data set 3
  data_set_3 <- rmpi_parse_variables(endp_lines)
  endp$StatusCount <- tibble::tibble(NumberOfParticles = as.numeric(data_set_3$variables[1:10]),
                                     StatusID = seq(0, 9),
                                     Status = c('Pending release', 'Active', 'Terminated at boundary face',
                                                'Terminated in weak sink cell', 'Terminated in weak source cell',
                                                'Terminated in cell with no exit face',
                                                'Terminated in cell with specified zone number',
                                                'Terminated in inactive cell', 'Permanently unreleased',
                                                'Terminated for unknown reasons'))
  endp_lines <- data_set_3$remaining_lines
  rm(data_set_3)

  # data set 4
  data_set_4 <- rmpi_parse_variables(endp_lines)
  endp$ParticleGroupCount <- as.numeric(data_set_4$variables[1])
  endp_lines <- data_set_4$remaining_lines
  rm(data_set_4)

  # data set 5
  endp$ParticleGroupName <- vector(mode = 'character', length = endp$ParticleGroupCount)
  for(i in 1:endp$ParticleGroupCount) {
    data_set_5 <- rmpi_parse_variables(endp_lines, character = TRUE)
    endp$ParticleGroupName[1] <- as.character(data_set_5$variables[1])
    endp_lines <- data_set_5$remaining_lines
    rm(data_set_5)
  }

  # data set 6
  # END HEADER
  data_set_6 <- rmpi_parse_variables(endp_lines, character = TRUE)
  endp_lines <- data_set_6$remaining_lines
  rm(data_set_6)

  # endpoint record
  npart <- sum(endp$StatusCount$NumberOfParticles[-c(1,10)])
  colnames <- c('SequenceNumber', 'ParticleGroup', 'ParticleID', 'Status', 'InitialTrackingTime', 'FinalTrackingTime',
                'InitialCellNumber', 'InitialLayer', 'InitialLocalX', 'InitialLocalY', 'InitialLocalZ',
                'InitialGlobalX', 'InitialGlobalY', 'InitialGlobalZ', 'InitialZone', 'InitialFace',
                'FinalCellNumber', 'FinalLayer', 'FinalLocalX', 'FinalLocalY', 'FinalLocalZ',
                'FinalGlobalX', 'FinalGlobalY', 'FinalGlobalZ', 'FinalZone', 'FinalFace')
  df <- readr::read_table(I(endp_lines), col_names = colnames, col_types = readr::cols(.default = 'd'), n_max = npart) # no 'lazy' argument
  endp$EndpointRecord <- tibble::as_tibble(df)

  class(endp) <- c('endpoint')
  return(endp)
}

#' Reads a pathline output file
#'
#' \code{rmp_read_pathline} reads a pathline output file and returns it as a \code{RMODPATH} pathline object
#'
#' @param file path tot the pathline output file
#' @param ... ignored
#'
#' @return object of class pathline
#' @export
#' @seealso \code{\link{rmp_read_timeseries}}, \code{\link{rmp_read_endpoint}}
#' @examples
rmp_read_pathline <- function(file, ...) {

  pth_lines <- readr::read_lines(file, lazy = FALSE)
  pth <- list()

  # data set 1
  data_set_1 <- rmpi_parse_variables(pth_lines, character = TRUE)
  label <- as.character(data_set_1$variables[1])
  version <- as.numeric(data_set_1$variables[2])
  revision <- as.numeric(data_set_1$variables[3])
  pth$header <- paste(label, 'Version', paste(version, revision, sep = '.'))
  pth_lines <- data_set_1$remaining_lines
  rm(data_set_1)

  # data set 2
  data_set_2 <- rmpi_parse_variables(pth_lines)
  pth$TrackingDirection <- as.numeric(data_set_2$variables[1])
  pth$ReferenceTime <- as.numeric(data_set_2$variables[2])
  pth$XOrigin <- as.numeric(data_set_2$variables[3])
  pth$YOrigin <- as.numeric(data_set_2$variables[4])
  pth$AngRot <- as.numeric(data_set_2$variables[5])
  pth_lines <- data_set_2$remaining_lines
  rm(data_set_2)

  # data set 3
  # END HEADER
  data_set_3 <- rmpi_parse_variables(pth_lines, character = TRUE)
  pth_lines <- data_set_3$remaining_lines
  rm(data_set_3)

  # Pathline record
  colnames_pathline <- c('CellNumber', 'GlobalX', 'GlobalY', 'GlobalZ', 'TrackingTime', 'LocalX', 'LocalY', 'LocalZ',
                         'Layer', 'StressPeriod', 'Timestep')
  colnames_header <- c('SequenceNumber', 'Group', 'ParticleID', 'PathlinePointCount')
  df <- list()

  while(length(pth_lines) > 0 && nchar(pth_lines[1]) > 0) {

    # data set 4
    data_set_4 <- rmpi_parse_variables(pth_lines)
    SequenceNumber <- as.numeric(data_set_4$variables[1])
    Group <- as.numeric(data_set_4$variables[2])
    ParticleID <- as.numeric(data_set_4$variables[3])
    PathlinePointCount <- as.numeric(data_set_4$variables[4])
    ds4 <- tibble::as_tibble(t(setNames(c(SequenceNumber, Group, ParticleID, PathlinePointCount), colnames_header)))
    pth_lines <- data_set_4$remaining_lines
    rm(data_set_4)

    # TODO check if this always works because it does not use rmpi_parse_variables
    # data set 5
    ds5 <- readr::read_table(I(pth_lines), col_names = colnames_pathline,
                             col_types = readr::cols(.default = 'd'), n_max = PathlinePointCount) # no 'lazy' argument
    pth_lines <- pth_lines[-c(1:PathlinePointCount)]
    df[[length(df)+1]] <- tibble::as_tibble(cbind(ds4, ds5))
  }

  pth$PathlineRecord <- do.call(rbind, df)

  class(pth) <- c('pathline')
  return(pth)
}



#' Reads a timeseries output file
#'
#' \code{rmp_read_timeseries} reads a timeseries output file and returns it as a \code{RMODPATH} timeseries object
#'
#' @param file path tot the timeseries output file
#' @param ... ignored
#'
#' @return object of class timeseries
#' @export
#' @seealso \code{\link{rmp_read_pathline}}, \code{\link{rmp_read_endpoint}}
#' @examples
rmp_read_timeseries <- function(file, ...) {

  ts_lines <- readr::read_lines(file, lazy = FALSE)
  ts <- list()

  # data set 1
  data_set_1 <- rmpi_parse_variables(ts_lines, character = TRUE)
  label <- as.character(data_set_1$variables[1])
  version <- as.numeric(data_set_1$variables[2])
  revision <- as.numeric(data_set_1$variables[3])
  ts$header <- paste(label, 'Version', paste(version, revision, sep = '.'))
  ts_lines <- data_set_1$remaining_lines
  rm(data_set_1)

  # data set 2
  data_set_2 <- rmpi_parse_variables(ts_lines)
  ts$TrackingDirection <- as.numeric(data_set_2$variables[1])
  ts$ReferenceTime <- as.numeric(data_set_2$variables[2])
  ts$XOrigin <- as.numeric(data_set_2$variables[3])
  ts$YOrigin <- as.numeric(data_set_2$variables[4])
  ts$AngRot <- as.numeric(data_set_2$variables[5])
  ts_lines <- data_set_2$remaining_lines
  rm(data_set_2)

  # data set 3
  # END HEADER
  data_set_3 <- rmpi_parse_variables(ts_lines, character = TRUE)
  ts_lines <- data_set_3$remaining_lines
  rm(data_set_3)

  # Timeseries record
  colnames_ts <- c('TimePointIndex', 'CumulativeTimeStep', 'TrackingTime', 'SequenceNumber',
                   'ParticleGroup', 'ParticleID', 'CellNumber', 'LocalX', 'LocalY', 'LocalZ',
                   'GlobalX', 'GlobalY', 'GlobalZ', 'Layer')
  ts$TimeseriesRecord <-  readr::read_table(I(ts_lines), col_names = colnames_ts,
                                            col_types = readr::cols(.default = 'd')) # no 'lazy' argument

  class(ts) <- c('timeseries')
  return(ts)
}


