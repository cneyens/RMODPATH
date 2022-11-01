.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.RMODPATH <- list(
    RMODPATH.path = file.path(system.file(package = "RMODPATH"), "code"),
    RMODPATH.ui = ifelse(is.null(opts$RMODFLOW.ui), "verbose", opts$RMODFLOW.ui),
    RMODPATH.theme = ifelse(is.null(opts$RMODFLOW.theme), "RMODFLOW", opts$RMODFLOW.theme)
  )
  toset <- !(names(opts.RMODPATH) %in% names(opts))
  if(any(toset)) options(opts.RMODPATH[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  rui::alert("{{RMODPATH}} is still in its experimental lifecycle stage.")
  rui::alert("Use at your own risk, and submit issues here:")
  rui::alert("{.url https://github.com/cneyens/RMODPATH/issues}")
  invisible()
}
