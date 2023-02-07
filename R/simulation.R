
#' Read a MODPATH Simulation File
#'
#' \code{rmp_read_sim} reads a MODPATH Simulation File and returns at as a \code{sim} object
#'
#' @param file path to the Simulation File
#' @param dis \code{RMODFLOW} dis object; only needed when reading zone arrays and/or retardation arrays
#' @param ... ignored
#'
#' @return a \code{RMODPATH} sim object
#' @export
#' @seealso \code{\link{rmp_write_sim}}, \code{\link{rmp_create_sim}}
#'
#' @examples
rmp_read_sim <- function(file, dis, ...) {

  sim_lines <- readr::read_lines(file, lazy = FALSE)
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

    data_set_8 <- rmpi_parse_variables(sim_lines, character = TRUE)
    sim$traceparticlegroup <- as.character(data_set_8$variables[1])
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
  # TODO check if budgetcellnumers are read through an array reader or not
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

    if(missing(dis)) stop('Please supply dis object when reading zone arrays', call. = FALSE)
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
    if(missing(dis)) stop('Please supply dis object when reading retardation arrays', call. = FALSE)
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
        releasetimes <- as.numeric(data_set_28$variables[1])
        sim_lines <- data_set_28$remaining_lines
        rm(data_set_28)

      } else if(releaseoption == 2) {
        data_set_29 <- rmpi_parse_variables(sim_lines)
        releasetimecount <- as.numeric(data_set_29$variables[1])
        initialreleasetime <- as.numeric(data_set_29$variables[2])
        releaseinterval <- as.numeric(data_set_29$variables[3])
        sim_lines <- data_set_29$remaining_lines
        rm(data_set_29)

      } else if(releaseoption == 3) {
        data_set_30 <- rmpi_parse_variables(sim_lines)
        releasetimecount <- as.numeric(data_set_30$variables[1])
        sim_lines <- data_set_30$remaining_lines
        rm(data_set_30)

        data_set_31 <- rmpi_parse_array(sim_lines, nrow = 1, ncol = releasetimecount, nlay = 1, ndim = 1, file = file)
        releasetimes <- data_set_31$array
        sim_lines <- data_set_31$remaining_lines
        rm(data_set_31)

      }

      # data set 32
      data_set_32 <- rmpi_parse_variables(sim_lines, character = TRUE)
      startinglocationsfileoption <- toupper(as.character(data_set_32$variables[1]))

      if(startinglocationsfileoption == 'EXTERNAL') {
        startinglocationsfile <- as.character(data_set_32$variables[2])
        sloc_file <- file.path(dirname(file), startinglocationsfile)
        startinglocations <- rmp_read_sloc(file = sloc_file, pathname = startinglocationsfile)

        sim_lines <- data_set_32$remaining_lines
        rm(data_set_32)

      } else if(startinglocationsfileoption == 'INTERNAL') {
        sim_lines <- data_set_32$remaining_lines
        rm(data_set_32)

        startinglocations_ds <- rmp_read_sloc(remaining_lines = sim_lines)
        startinglocations <- startinglocations_ds$starting_locations
        sim_lines <- startinglocations_ds$remaining_lines
      }

      # create particle group
      if(releaseoption == 1) {
        sim$particlegroups[[i]] <- rmp_create_particlegroup(startinglocations, name = groupname,
                                                            releaseoption = releaseoption, releasetimes = releasetimes)
      } else if(releaseoption == 2) {
        sim$particlegroups[[i]] <- rmp_create_particlegroup(startinglocations, name = groupname,
                                                            releaseoption = releaseoption, timecount = releasetimecount,
                                                            initialreleasetime = initialreleasetime, interval = releaseinterval)
      } else {
        sim$particlegroups[[i]] <- rmp_create_particlegroup(startinglocations, name = groupname,
                                                            releaseoption = releaseoption, timecount = releasetimecount,
                                                            releasetimes = releasetimes)
      }

      names(sim$particlegroups)[i] <- groupname
    }
  }

  class(sim) <- c('sim', 'rmp_package')
  return(sim)
}


#' Write a MODPATH Simulation File
#'
#' \code{rmp_write_sim} writes a MODPATH Simulation File based on an \code{RMODPATH} sim object.
#'
#' @param sim a \code{RMODPATH} sim object
#' @param file filename to write to; typically '*.mpsim'
#' @param ... additional arguments passed to \code{rmpi_write_array}. Can be ignored when arrays are INTERNAL or CONSTANT.
#'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmp_read_sim}}, \code{\link{rmp_create_sim}}
#'
#' @examples
rmp_write_sim <- function(sim, file, ...) {

  # data set 0
  v <- packageDescription("RMODPATH")$Version
  cat(paste('# MODPATH Simulation File created by RMODPATH, version',v,'\n'), file=file)
  cat(paste('#', comment(sim)), sep='\n', file=file, append=TRUE)

  # data set 1
  rmpi_write_variables(sim$namefilename, file=file)

  # data set 2
  rmpi_write_variables(sim$listingfilename, file=file)

  # data set 3
  rmpi_write_variables(sim$simulationtype, sim$trackingdirection, sim$weaksinkoption, sim$weaksourceoption,
                       sim$budgetoutputoption, sim$tracemode, file=file, integer = TRUE)

  # data set 4
  rmpi_write_variables(sim$endpointfilename, file=file)

  # data set 5
  if(sim$simulationtype %in% c(2, 4)) rmpi_write_variables(sim$pathlinefilename, file=file)

  # data set 6
  if(sim$simulationtype %in% c(3, 4)) rmpi_write_variables(sim$timeseriesfilename, file=file)

  # data set 7 & 8
  # TODO check if traceparticlegroup is integer or character name
  if(sim$tracemode == 1) {
    rmpi_write_variables(sim$tracefilename, file=file)
    rmpi_write_variables(as.character(sim$traceparticlegroup), as.integer(sim$traceparticleid), file=file)
  }

  # data set 9
  rmpi_write_variables(sim$budgetcellcount, file=file, integer = TRUE)

  # data set 10
  # TODO check if budgetcellnumers are read through an array reader or not
  if(sim$budgetcellcount > 0) rmpi_write_variables(sim$budgetcellnumbers, file=file, integer = TRUE)

  # data set 11
  rmpi_write_variables(sim$referencetimeoption, file=file, integer = TRUE)

  # data set 12 & 13
  if(sim$referencetimeoption == 1) {
    rmpi_write_variables(sim$referencetime, file=file)
  } else if(sim$referencetimeoption == 2) {
    rmpi_write_variables(as.integer(sim$stressperiod), as.integer(sim$timestep), sim$timestepfraction, file=file)
  } else {
    stop('ReferenceTimeOption should be 1 or 2', call. = FALSE)
  }

  # data set 14
  rmpi_write_variables(sim$stoptimeoption, file=file, integer = TRUE)

  # data set 15
  if(sim$stoptimeoption == 3) rmpi_write_variables(sim$stoptime, file=file)

  # data set 16-19
  if(sim$simulationtype %in% c(3, 4)) {
    rmpi_write_variables(sim$timepointoption, file=file, integer = TRUE)

    if(sim$timepointoption == 1) {
      rmpi_write_variables(as.integer(sim$timepointcount), sim$timepointinterval, file=file)

    } else if(sim$timepointoption == 2) {
      rmpi_write_variables(sim$timepointcount, file=file, integer = TRUE)
      # TODO check if timepoints are read through an array reader or not
      rmpi_write_variables(sim$timepoints, file=file)

    } else {
      stop('TimePointOption should be 1 or 2', call. = FALSE)
    }
  }

  # data set 20
  rmpi_write_variables(sim$zonedataoption, file=file, integer = TRUE)

  # data set 21 & 22
  if(sim$zonedataoption == 2) {
    if(sim$stopzone < 0) stop('Negative StopZone value not allowed', call. = FALSE)
    rmpi_write_variables(sim$stopzone, file=file, integer = TRUE)
    rmpi_write_array(sim$zones, file = file, ...)
  } else if(sim$zonedataoption != 1) {
    stop('ZoneDataOption should be 1 or 2', call. = FALSE)
  }

  # data set 23
  rmpi_write_variables(sim$retardationfactoroption, file=file, integer = TRUE)

  # data set 24
  if(sim$retardationfactoroption == 2) {
    rmpi_write_array(sim$retardation, file = file, ...)
  } else if(sim$retardationfactoroption != 1) {
    stop('RetardationFactorOption should be 1 or 2', call. = FALSE)
  }

  # data set 25
  rmpi_write_variables(sim$particlegroupcount, file=file, integer = TRUE)

  # data set 26 - 31
  if(sim$particlegroupcount > 0) {
    for(i in 1:sim$particlegroupcount) {
      grp <- sim$particlegroups[[i]]
      rmpi_write_variables(as.character(grp$groupname), file=file)
      rmpi_write_variables(grp$releaseoption, file=file, integer = TRUE)

      if(grp$releaseoption == 1) {
        rmpi_write_variables(grp$releasetime, file=file)
      } else if(grp$releaseoption == 2) {
        rmpi_write_variables(as.integer(grp$releasetimecount), grp$initialreleasetime, grp$releaseinterval, file=file)
      } else if(grp$releaseoption == 3) {
        rmpi_write_variables(grp$releasetimecount, file=file, integer = TRUE)
        # TODO check if releasetimes are read through an array reader or not
        rmpi_write_variables(grp$releasetimes, file=file)
      } else {
        stop('ReleaseOption should be 1, 2 or 3', call. = FALSE)
      }

      # data set 32
      if(toupper(grp$startinglocations$startinglocationsfileoption) == "EXTERNAL") {
        rmpi_write_variables(toupper(grp$startinglocations$startinglocationsfileoption),
                             grp$startinglocations$startinglocationsfilename, file=file)

        # TODO check if this works when startinglocationsfilename is an absolute path
        f <- file.path(dirname(file), grp$startinglocations$startinglocationsfilename)
        rmp_write_sloc(grp$startinglocations, file=f, append = FALSE)

      } else if(toupper(grp$startinglocations$startinglocationsfileoption) == "INTERNAL"){
        rmpi_write_variables(toupper(grp$startinglocations$startinglocationsfileoption), file=file)
        rmp_write_sloc(grp$startinglocations, file=file, append = TRUE)

      } else {
        stop('StartingLocationsFileOption should be INTERNAL or EXTERNAL', call. = FALSE)
      }
      rm(grp)
    }
  }

}


#' Create a RMODPATH sim object
#'
#' \code{rmp_create_sim} creates a \code{RMODPATH} sim object.
#'
#' @param namefilename character; filename of the MODPATH Name File
#' @param dis \code{RMODFLOW} dis object; only required when specifying zones or retardation.
#' @param listingfilename character; filename of the MODPATH listing file. Defaults to 'output.mplist'.
#' @param simulationtype integer specifying the type of MODPATH simulation. Defaults to 4.
#' @param trackingdirection integer indicating whether the forward (1; default) or backward (2) particle tracking is performed.
#' @param weaksinkoption integer indicating whether particles pass through (1; default) or stop at (2) weak sink cells.
#' @param weaksourceoption integer indicating whether particles pass through (1; default) or stop at (2) weak source cells.
#' @param budgetoutputoption integer controlling the type of information on individual cell water balance errors to be computed and printed. Defaults to 1 (summary is printed; record headers are not).
#' @param tracemode integer specifying whether trace mode is on (1) or off (0; default).
#' @param endpointfilename character; name of the MODPATH endpoint output file. Defaults to 'output.endp'.
#' @param pathlinefilename character; name of the MODPATH pathline output file. Defaults to 'output.path'.
#' @param timeseriesfilename character; name of the MODPATH timeseries output file. Defaults to 'output.tms'.
#' @param tracefilename character; name of the MODPATH trace output file. Defaults to 'output.trace'.
#' @param traceparticlegroup character; particle group name of the specified particle to be traced when tracemode = 1.
#' @param traceparticelid integer; particle ID of the specified particle to be traced when tracemode = 1.
#' @param budgetcellnumbers optional integer vector; MODFLOW grid cell numbers for which detailed water budgets are computed.
#' @param referencetimeoption integer specifying how the reference time is defined. Default to 1.
#' @param referencetime numeric specifying the reference time when referencetimeoption = 1. Defaults to 0.0.
#' @param stressperiod integer specifying the  stress period from which the reference time is obtained when referencetimeoption = 2.
#' @param timestep integer specifying the time step within stressperiod from which the reference time is obtained when referencetimeoption = 2.
#' @param timestepfraction numeric specifying the fraction between 0 and 1 of timestep from which the reference time is obtained when referencetimeoption = 2.
#' @param stoptimeoption integer; determines how the simulation is terminated based on time. Defaults to extended the final time step until all particles are terminated (2).
#' @param stoptime numeric; tracking time value at which to stop the simulation when stoptimeoption = 3.
#' @param timepointoption integer; controls how time point data should be specified when timeseries output is written (simulationtype = 3 or 4).
#' @param timepointcount integer; is the number of time points for which time point data should be specified at intervals timepointinterval when timepointoption = 1. When timepointoption = 2, this is determined from the length of timepoints.
#' @param timepointinterval numeric; uniform time interval at which time point data is written when timepointoption = 1.
#' @param timepoints numeric vector specifying the time points at which to write time point data when timepointoption = 2.
#' @param stopzone integer; specifies the zone value in zones where particles are terminated. Only used when zones is also specified. Only positive values are allowed.
#' @param zones optional 3d positive integer array of dimensions {dis$nrow x dis$ncol x dis$nlay} specifying the zone numbers for each grid.
#' @param retardation optional 3d numeric array of dimensions {dis$nrow x dis$ncol x dis$nlay} specifying the retardation factor for each grid cell.
#' @param particlegroups a single \code{RMODPATH} particle_group object or a list of \code{RMODPATH} particle_group objects specifying the particle groups and corresponding starting locations. See also \code{\link{rmp_create_particlegroup}}.
#'
#' @return Object of class sim
#' @export
#' @seealso \code{\link{rmp_write_sim}}, \code{\link{rmp_read_sim}}, \code{\link{rmp_create_particlegroup}}, \code{\link{rmp_create_sloc}}, \code{\link{rmp_get_locations}}
#'
#' @examples
rmp_create_sim <- function(namefilename,
                           dis,
                           listingfilename = 'output.mplist',
                           simulationtype = 4,
                           trackingdirection = 1,
                           weaksinkoption = 1,
                           weaksourceoption = 1,
                           budgetoutputoption = 1,
                           tracemode = 0,
                           endpointfilename = 'output.endp',
                           pathlinefilename = 'output.path',
                           timeseriesfilename = 'output.tms',
                           tracefilename = 'output.trace',
                           traceparticlegroup,
                           traceparticelid,
                           budgetcellnumbers = NULL,
                           referencetimeoption = 1,
                           referencetime = 0.0,
                           stressperiod,
                           timestep,
                           timestepfraction,
                           stoptimeoption = 2,
                           stoptime,
                           timepointoption,
                           timepointcount,
                           timepointinterval,
                           timepoints,
                           stopzone,
                           zones = NULL,
                           retardation = NULL,
                           particlegroups) {

  sim <- list()

  # data set 1-2
  sim$namefilename <- as.character(namefilename)
  sim$listingfilename <- as.character(listingfilename)

  # data set 3
  sim$simulationtype <- simulationtype
  sim$trackingdirection <- trackingdirection
  sim$weaksinkoption <- weaksinkoption
  sim$weaksourceoption <- weaksourceoption
  sim$budgetoutputoption <- budgetoutputoption
  sim$tracemode <- tracemode

  # data set 4
  sim$endpointfilename <- as.character(endpointfilename)

  # data set 5
  if(sim$simulationtype %in% c(2, 4)) sim$pathlinefilename <- as.character(pathlinefilename)

  # data set 6
  if(sim$simulationtype %in% c(3, 4)) sim$timeseriesfilename <- as.character(timeseriesfilename)

  # data set 7-8
  if(sim$tracemode == 1) {
    sim$tracefilename <- as.character(tracefilename)
    sim$traceparticlegroup <- as.character(traceparticlegroup)
    sim$traceparticelid <- traceparticelid
  }

  # data set 9
  if(is.null(budgetcellnumbers)) {
    sim$budgetcellcount <- 0
  } else {
    sim$budgetcellcount <- length(budgetcellnumbers)
  }

  # data set 10
  if(sim$budgetcellcount > 0) sim$budgetcellnumbers <- budgetcellnumbers

  # data set 11
  sim$referencetimeoption <- referencetimeoption

  # data set 12
  if(sim$referencetimeoption == 1) sim$referencetime <- referencetime

  # data set 13
  if(sim$referencetimeoption == 2) {
    sim$stressperiod <- stressperiod
    sim$timestep <- timestep
    sim$timestepfraction <- timestepfraction
  }

  # data set 14
  sim$stoptimeoption <- stoptimeoption

  # data set 15
  if(sim$stoptimeoption == 3) sim$stoptime <- stoptime

  # data set 16-19
  if(sim$simulationtype %in% c(3, 4)) {
    sim$timepointoption <- timepointoption

    if(sim$timepointoption == 1) {
      sim$timepointcount <- timepointcount
      sim$timepointinterval <- timepointinterval
    }
    if(sim$timepointoption == 2) {
      sim$timepointcount <- length(timepoints)
      sim$timepoints <- timepoints
    }
  }

  # data set 20-22
  if(is.null(zones)) {
    sim$zonedataoption <- 1
  } else {
    sim$zonedataoption <- 2
    if(length(stopzone) > 1 | stopzone < 0) stop('stopzone should be a single positive integer', call. = FALSE)
    sim$stopzone <- stopzone
    sim$zones <- RMODFLOW::rmf_create_array(as.integer(zones), dim = c(dis$nrow, dis$ncol, dis$nlay))
  }

  # data set 23-24
  if(is.null(retardation)) {
    sim$retardationfactoroption <- 1
  } else {
    sim$retardationfactoroption <- 2
    sim$retardation <- RMODFLOW::rmf_create_array(retardation, dim = c(dis$nrow, dis$ncol, dis$nlay))
  }

  # data set 25

  # data set 26-32
  if(inherits(particlegroups, 'particle_group')) particlegroups <- list(particlegroups)
  all_pgr <- all(vapply(particlegroups, function(i) inherits(i, "particle_group"), TRUE))
  if(!all_pgr) stop('particlegroups should be a list consisting of RMODPATH particle_group objects', call. = FALSE)
  grp_names <- vapply(particlegroups, '[[', 'groupname', FUN.VALUE = 'text')
  if(any(duplicated(grp_names))) stop('particlegroups should have unique group names', call. = FALSE)

  sim$particlegroupcount <- length(particlegroups)
  sim$particlegroups <- particlegroups
  names(sim$particlegroups) <- grp_names

  class(sim) <- c('sim', 'rmp_package')
  return(sim)
}
