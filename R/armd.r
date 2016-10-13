#library(devtools); install_github(repo="armd", studname="skranz")


.onLoad = function(...)  {
  # If loaded as library overwrite restore.point to an empty function
  #assign("restore.point", function(...){}, envir=parent.env(environment()))
}


#' Add a hint to an exercise.
#'
#' @param hint.name A name for the hint
#' @param code R code inside {} that will be evaluated in sol.env when the hint is shown. A hint could show messages via cat, but it could also show a plot.
add.hint = function(hint.name, code,cond=NULL, ex=get.ex()) {
  code = substitute(code)
  restore.point("add.hint")
  hint = list(name=hint.name,cond=cond,code=code,ex=ex)
  ex$hints[[hint.name]]=hint
}

print.Problemset = function(am) {
  omit = c("output","shiny.dt","cdt","tdt", "view.ui.li")
  omit = c("output")
  fields = setdiff(ls(am),omit)
  print(str(as.list(am)[fields], max.level=1, give.attr=FALSE))


  cat("\n Ommited: ", paste0(omit, collapse="\n"))
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

#' @export
get.ex = function() {
  if (!exists(".__armd_ex",.GlobalEnv))
    return(NULL)
  get(".__armd_ex",.GlobalEnv)
}

#' @export
set.ex = function(ex) {
  assign(".__armd_ex", ex, .GlobalEnv)
}


examples = function() {

  library(restorepoint)
  setwd("C:/libraries/armd")
  source("armd.r")

  am = load.problem.set("First Steps")
  create.stud.am(am)

  check.exercise("1a")

}

#' Create a zip file of your solution that can be submitted
#'
#' Only works after you have once checked your problem set!
#' @export
zip.solution = function(am = get.am(), user.name=get.user.name(), dir = am$stud.path, ask=TRUE) {
  restore.point("zip.solution")

  if (is.null(am)) {
    display("You must check your problem set before you can zip the solution")
    return(invisible(NULL))
  }


  if (ask) {
    stats()
    cat("\nDo you want to zip your solution with the stats above?\n(If the stats seem wrong, cancel and check your problem set again.)")
    res = readline(prompt="Type y and Enter to continue: ")
    if (!tolower(res)=="y") {
      cat("\nCancelled zipping of solution.")
      return(invisible(NULL))
    }
  }

  old.dir = getwd()
  setwd(dir)
  #files = paste0(am.name,c(".log",".r",".rmd",".html"))
  #files = c(am$stud.file, am$log.file, paste0(am$stud.path,"/",user.name,"_",am$name,".ups"))

  files = c(am$stud.short.file, paste0(am$name,".log"), paste0(user.name,"_",am$name,".ups"))

  zip.file = paste0(dir,"/solution_", gsub(" ","_",am$name,fixed=TRUE), "_by_", user.name, ".zip")

  zip(zip.file, files)
  display(paste0("Created zipped solution ", zip.file))
  setwd(old.dir)
  return(invisible(zip.file))
}

unzip.solutions = function(dir = getwd(), dest.dir = dir) {
  restore.point("unzip.solutions")
  files = c(list.files(path=dir,pattern=glob2rx("*.zip"), recursive=check.sub.dir, full.names=TRUE),
            list.files(path=dir,pattern=glob2rx("*.ZIP"), recursive=check.sub.dir, full.names=TRUE))

  lapply(files, function(file) {
    restore.point("zwsdhd")
    subdir =  paste0(dest.dir,"/solutions/",str.left.of(basename(file),"."))
    dir.create(subdir)
    unzip(file, exdir=subdir, overwrite=TRUE)
    message(paste0("Unzipped ", file))
  })

}

