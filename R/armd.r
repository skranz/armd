#library(devtools); install_github(repo="armd", studname="skranz")
.onLoad = function(...) {
  addArmdRessourcePath()
}

reset.am = function(am=get.am()) {
  init.am(am$name,am$user.name, am$stud.path,am$stud.short.file)
}

#' Get the current problem set
#'
#' Either a globally stored problem set or
#' if armd runs as a web-app the associated problem set
#' with the current shiny session
#'
#' @export
get.am = function(force.global = FALSE) {
  if (force.global) {
    gps = get(".__armd_ps",.GlobalEnv)
    return(gps)

  }

  app = getApp()
  if (!is.null(app[["am"]]))
    return(app[["am"]])

  if (!exists(".__armd_ps",.GlobalEnv))
    return(NULL)
  gps = get(".__armd_ps",.GlobalEnv)
  return(gps)
}

#' @export
set.am = function(am, app=getApp()) {
  assign(".__armd_ps", am, .GlobalEnv)
  if (!is.null(app))
    app$am = am
}



fetch.am = function(am = NULL, am.file=NULL, rmd.file=NULL, txt=NULL, name=NULL) {
  restore.point("fetch.am")

  if (!is.null(rmd.file)) {
    txt = readLines(rmd.file)
    if (is.null(name)) {
      name = tools::file_path_sans_ext(basename(rmd.file))
    }
  }
  if (!is.null(txt)) {
    if (is.null(name)) name = "armd"
    am = parse.armd(txt=txt, name=name)
  } else if (!is.null(am.file)) {
    am = read.am(am.file)
  }
  am
}


examples = function() {

  library(restorepoint)
  setwd("C:/libraries/armd")
  source("armd.r")

  am = load.problem.set("First Steps")
  create.stud.am(am)

  check.exercise("1a")

}
