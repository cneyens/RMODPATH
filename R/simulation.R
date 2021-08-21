
#' Read a MODPATH Simulation File
#'
#'  \code{rmp_read_sim} reads a MODPATH simulation file and returns at as a \code{sim} object
#'
#' @param file path to the simulation file
#' @param dis \code{RMODFLOW} dis object; only needed when reading zone arrays and/or retardation arrays
#'
#' @return a \code{RMODPATH} sim object
#' @export
#'
#' @examples
rmp_read_sim <- function(file, dis) {

  sim_lines <- readr::read_lines(file)
  sim <- list()

  # data set 0
  data_set_0 <- rmpi_parse_comments(sim_lines)
  comment(sim) <- data_set_0$comments
  sim_lines <- data_set_0$remaining_lines
  rm(data_set_0)

  # data set 1
  data_set_1 <- rmpi_parse_variables(sim_lines, character = TRUE)
  sim$namefilename <- as.character(data_set_1$variables[1])
  sim_lines <- data_set_1$remaining_lines
  rm(data_set_1)

  # data set 2
  data_set_2 <- rmpi_parse_variables(sim_lines, character = TRUE)
  sim$listingfilename <- as.character(data_set_2$variables[1])
  sim_lines <- data_set_2$remaining_lines
  rm(data_set_2)

  # data set 3
  data_set_3 <- rmpi_parse_variables(sim_lines)
  sim$simulationtype <- as.numeric(data_set_3$variables[1])
  sim$trackingdirection <- as.numeric(data_set_3$variables[2])
  sim$weaksinkoption <- as.numeric(data_set_3$variables[3])
  sim$weaksourceoption <- as.numeric(data_set_3$variables[4])
  sim$budgetoutputoption <- as.numeric(data_set_3$variables[5])
  sim$tracemode <- as.numeric(data_set_3$variables[6])
  sim_lines <- data_set_3$remaining_lines
  rm(data_set_3)

  # data set 4
  data_set_4 <- rmpi_parse_variables(sim_lines, character = TRUE)
  sim$endpointfilename <- as.character(data_set_4$variables[1])
  sim_lines <- data_set_4$remaining_lines
  rm(data_set_4)

  # data set 5
  if(sim$simulationtype %in% c(2, 4)) {
    data_set_5 <- rmpi_parse_variables(sim_lines, character = TRUE)
    sim$pathlinefilename <- as.character(data_set_5$variables[1])
    sim_lines <- data_set_5$remaining_lines
    rm(data_set_5)
  }

  # data set 6
  if(sim$simulationtype %in% c(3, 4)) {
    data_set_6 <- rmpi_parse_variables(sim_lines, character = TRUE)
    sim$timeseriesfilename <- as.character(data_set_6$variables[1])
    sim_lines <- data_set_6$remaining_lines
    rm(data_set_6)
  }

  # data set 7 & 8
  if(sim$tracemode == 1) {
    data_set_7 <- rmpi_parse_variables(sim_lines, character = TRUE)
    sim$tracefilename <- as.character(data_set_7$variables[1])
    sim_lines <- data_set_7$remaining_lines
    rm(data_set_7)

    data_set_8 <- rmpi_parse_variables(sim_lines)
    sim$traceparticlegroup <- as.numeric(data_set_8$variables[1])
    sim$traceparticleid <- as.numeric(data_set_8$variables[2])
    sim_lines <- data_set_8$remaining_lines
    rm(data_set_8)
  }

  # data set 9
  data_set_9 <- rmpi_parse_variables(sim_lines)
  sim$budgetcellcount <- as.numeric(data_set_9$variables[1])
  sim_lines <- data_set_9$remaining_lines
  rm(data_set_9)

  # data set 10
  if(sim$budgetcellcount > 0) {
    data_set_10 <- rmpi_parse_array(sim_lines, nrow = 1, ncol = sim$budgetcellcount, nlay = 1, ndim = 1, file = file)
    sim$budgetcellnumbers <- as.numeric(data_set_10$array)
    sim_lines <- data_set_10$remaining_lines
    rm(data_set_10)
  }

  # data set 11
  data_set_11 <- rmpi_parse_variables(sim_lines)
  sim$referencetimeoption <- as.numeric(data_set_11$variables[1])
  sim_lines <- data_set_11$remaining_lines
  rm(data_set_11)

  # data set 12 & 13
  if(sim$referencetimeoption == 1) {
    data_set_12 <- rmpi_parse_variables(sim_lines)
    sim$referencetime <- as.numeric(data_set_12$variables[1])
    sim_lines <- data_set_12$remaining_lines
    rm(data_set_12)

  } else if(sim$referencetimeoption == 2) {
    data_set_13 <- rmpi_parse_variables(sim_lines)
    sim$stressperiod <- as.numeric(data_set_13$variables[1])
    sim$timestep <- as.numeric(data_set_13$variables[2])
    sim$timestepfraction <- as.numeric(data_set_13$variables[3])
    sim_lines <- data_set_13$remaining_lines
    rm(data_set_13)
  }

  # data set 14
  data_set_14 <- rmpi_parse_variables(sim_lines)
  sim$stoptimeoption <- as.numeric(data_set_14$variables[1])
  sim_lines <- data_set_14$remaining_lines
  rm(data_set_14)

  # data set 15
  if(sim$stoptimeoption == 3) {
    data_set_15 <- rmpi_parse_variables(sim_lines)
    sim$stoptime <- as.numeric(data_set_15$variables[1])
    sim_lines <- data_set_15$remaining_lines
    rm(data_set_15)
  }

  # data set 16 - 19
  if(sim$simulationtype %in% c(3, 4)) {
    data_set_16 <- rmpi_parse_variables(sim_lines)
    sim$timepointoption <- as.numeric(data_set_16$variables[1])
    sim_lines <- data_set_16$remaining_lines
    rm(data_set_16)

    if(sim$timepointoption == 1) {
      data_set_17 <- rmpi_parse_variables(sim_lines)
      sim$timepointcount <- as.numeric(data_set_17$variables[1])
      sim$timepointinterval <- as.numeric(data_set_17$variables[2])
      sim_lines <- data_set_17$remaining_lines
      rm(data_set_17)

    } else if(sim$timepointoption == 2) {
      data_set_18 <- rmpi_parse_variables(sim_lines)
      sim$timepointcount <- as.numeric(data_set_18$variables[1])
      sim_lines <- data_set_18$remaining_lines
      rm(data_set_18)

      data_set_19 <- rmpi_parse_array(remaining_lines, nrow = 1, ncol = sim$timepointcount, nlay = 1, ndim = 1, file = file)
      sim$timepoints <- as.numeric(data_set_19$array)
      sim_lines <- data_set_19$remaining_lines
      rm(data_set_19)
    }
  }

  # data set 20
  data_set_20 <- rmpi_parse_variables(sim_lines)
  sim$zonedataoption <- as.numeric(data_set_20$variables[1])
  sim_lines <- data_set_20$remaining_lines
  rm(data_set_20)

  # data set 21 & 22
  if(sim$zonedataoption == 2) {
    data_set_21 <- rmpi_parse_variables(sim_lines)
    sim$stopzone <- as.integer(data_set_21$variables[1])
    sim_lines <- data_set_21$remaining_lines
    rm(data_set_21)

    data_set_22 <- rmpi_parse_array(sim_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = dis$nlay, ndim = 3, file = file, integer = TRUE)
    sim$zones <- apply(data_set_22$array, MARGIN = 1:length(dim(data_set_22$array)), function(x) as.integer(x))
    attributes(sim$zones) <- attributes(data_set_22$array)
    sim_lines <- data_set_22$remaining_lines
    rm(data_set_22)
  }

  # data set 23
  data_set_23 <- rmpi_parse_variables(sim_lines)
  sim$retardationfactoroption <- as.numeric(data_set_23$variables[1])
  sim_lines <- data_set_23$remaining_lines
  rm(data_set_23)

  # data set 24
  if(sim$retardationfactoroption == 2) {
    data_set_24 <- rmpi_parse_array(sim_lines, nrow = dis$nrow, ncol = dis$ncol, nlay = dis$nlay, ndim = 3, file = file)
    sim$retardation <- data_set_24$array
    sim_lines <- data_set_24$remaining_lines
    rm(data_set_24)
  }

  # data set 25
  data_set_25 <- rmpi_parse_variables(sim_lines)
  sim$particlegroupcount <- as.numeric(data_set_25$variables[1])
  sim_lines <- data_set_25$remaining_lines
  rm(data_set_25)

  # data set 26 - 32
  if(sim$particlegroupcount > 0) {
    sim$particlegroups <- list()

    for(i in 1:sim$particlegroupcount) {
      data_set_26 <- rmpi_parse_variables(sim_lines, character = TRUE)
      groupname <- as.character(data_set_26$variables[1])
      sim_lines <- data_set_26$remaining_lines
      rm(data_set_26)

      data_set_27 <- rmpi_parse_variables(sim_lines)
      releaseoption <- as.numeric(data_set_27$variables[1])
      sim_lines <- data_set_27$remaining_lines
      rm(data_set_27)

      if(releaseoption == 1) {
        data_set_28 <- rmpi_parse_variables(sim_lines)
        releasetime <- as.numeric(data_set_28$variables[1])
        sim_lines <- data_set_28$remaining_lines
        rm(data_set_28)

        sim$particlegroups[[i]] <- list(groupname = groupname, releaseoption = releaseoption, releasetime = releasetime)

      } else if(releaseoption == 2) {
        data_set_29 <- rmpi_parse_variables(sim_lines)
        releasetimecount <- as.numeric(data_set_29$variables[1])
        initialreleasetime <- as.numeric(data_set_29$variables[2])
        releaseinterval <- as.numeric(data_set_29$variables[3])
        sim_lines <- data_set_29$remaining_lines
        rm(data_set_29)

        sim$particlegroups[[i]] <- list(groupname = groupname, releaseoption = releaseoption,
                                                releasetimecount = releasetimecount, initialreleasetime = initialreleasetime,
                                                releaseinterval = releaseinterval)

      } else if(releaseoption == 3) {
        data_set_30 <- rmpi_parse_variables(sim_lines)
        releasetimecount <- as.numeric(data_set_30$variables[1])
        sim_lines <- data_set_30$remaining_lines
        rm(data_set_30)

        data_set_31 <- rmpi_parse_array(sim_lines, nrow = 1, ncol = releasetimecount, nlay = 1, ndim = 1, file = file)
        releasetimes <- data_set_31$array
        sim_lines <- data_set_31$remaining_lines
        rm(data_set_31)

        sim$particlegroups[[i]] <- list(groupname = groupname, releaseoption = releaseoption,
                                                releasetimecount = releasetimecount, releasetimes = releasetimes)

      }

      # data set 32
      data_set_32 <- rmpi_parse_variables(sim_lines, character = TRUE)
      startinglocationsfileoption <- toupper(as.character(data_set_32$variables[1]))

      if(startinglocationsfileoption == 'EXTERNAL') {
        startinglocationsfile <- as.character(data_set_32$variables[2])
        sloc_file <- file.path(dirname(file), startinglocationsfile)
        startinglocations <- rmp_read_starting_locations(file = sloc_file)

        sim_lines <- data_set_32$remaining_lines
        rm(data_set_32)

      } else if(startinglocationsfileoption == 'INTERNAL') {
        sim_lines <- data_set_32$remaining_lines
        rm(data_set_32)

        startinglocations_ds <- rmp_read_starting_locations(remaining_lines = sim_lines)
        startinglocations <- startinglocations_ds$starting_locations
        sim_lines <- startinglocations_ds$remaining_lines
      }

      sim$particlegroups[[i]]$startinglocations <- startinglocations
      names(sim$particlegroups)[i] <- groupname
    }
  }

  class(sim) <- c('sim', 'modpath_object')
  return(sim)
}
