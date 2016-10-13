# Functions that support running problemsets in RStudio

info = function(info.name, am = get.am()) {
  restore.point("info")
  if (is.null(am)) {
    display("Please check your problem set once with Ctrl-Alt-R. Then you can see infos.")
    return()
  }
  infos = am$am$infos
  if (! info.name %in% names(infos)) {
    display("You problem set has not the info '", info.name, "'. Only the following infos are stored: ", paste0("'", names(infos),"'", collapse=", "))
    return()
  }
  htmlFile <- tempfile(fileext=".html")
  writeLines(infos[[info.name]]$html,htmlFile )
  if (require(rstudioapi)) {
    rstudioapi::viewer(htmlFile)
  } else {
    cat("Info boxes can only be shown from RStudio. Please install the package rstudioapi.")
  }

}

