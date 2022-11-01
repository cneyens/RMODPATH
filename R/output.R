
#' Reads an endpoint output file
#'
#' \code{rmp_read_endpoint} reads an endpoint file and returns it as a \code{\link{RMODPATH}} endpoint object
#'
#' @param file path tot the endpoint output file
#'
#' @return object of class endpoint
#' @export
#'
#' @examples
rmp_read_endpoint <- function(file) {

  endp_lines <- readr::read_lines(file)
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
  df <- readr::read_table(I(endp_lines), col_names = colnames, col_types = paste0(rep('d', length(colnames)), collapse = ''), n_max = npart) # no 'lazy' argument
  endp$EndpointRecord <- tibble::as_tibble(df)

  class(endp) <- c('endpoint')
  return(endp)
}

# rmp_read_pathline <- function() {
#
# }
#
# rmp_read_timeseries <- function() {
#
# }


