replace.mathjax.dollars = function(...) {
  txt = readClipboard()
  txt = armd:::mathjax.dollars.to.brackets(txt)
  txt = writeClipboard(txt)
}

guess.armd.file = function(txt = readLines(file)) {
  restore.point("guess.armd.file")

  num = sum(str.starts.with(txt,"#. section") | str.starts.with(txt, "#. frame"))
  if (num > 0) return("armd")
  num = sum(str.starts.with(txt,"#. rmd") | str.starts.with(txt, "#. readonly"))
  if (num > 0) return("rmdform")

  return("armd")
}

showArmdPartAddin = function(...) {
  preview.armd.part.addin(single.part=TRUE)
}

showArmdOfflineSlides = function(...) {
  compileArmdDocument(..., offline=TRUE)
}


showArmdPrint = function(...) {
  library(armd)
  doc = rstudioapi::getActiveDocumentContext()
  file = basename(doc$path)
  if (nchar(file)==0) {
    cat("\nRStudio has not detected your .rmd tab. Please try again!")
    return()
  }
  dir = dirname(doc$path)
  setwd(dir)

  app = armdPrintApp(rmd.file = file)
  viewApp(app, launch.browser = TRUE)

}


showArmdAddin = function(...) {
  preview.armd.part.addin(single.part=FALSE)
}

compileArmdDocument = function(..., offline=FALSE) {
  library(armd)
  restore.point("compileArmdDocument")

  doc = rstudioapi::getActiveDocumentContext()

  file = basename(doc$path)

  if (nchar(file)==0) {
    cat("\nRStudio has not detected your .rmd tab. Please try again!")
    return()
  }
  dir = dirname(doc$path)
  setwd(dir)

  am = parse.armd(file=file, dir=dir, source.file = file, offline=offline)


  if (!offline) {
    preview.armd.part.addin(am=am,...)
  } else {
    createArmdOfflineSlides(am, doc)
  }
}

createArmdOfflineSlides = function(am,doc) {
  restore.point("createArmdOfflineSlides")

  armd.offline.html(am)
}

preview.armd.part.addin = function(single.part=TRUE,...,am=NULL, greyout=FALSE) {

  library(rstudioapi)
  library(armd)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("preview.armd.part.addin")
  #cat("\nView frame")

  file = basename(doc$path)
  dir = dirname(doc$path)

  if (nchar(file)==0) {
    cat("\nRStudio has not detected your academic rmarkdown .rmd tab. Please try again!")
    return(invisible())
  }

  setwd(dir)

  txt = doc$contents

  guess = guess.armd.file(txt=txt)
  if (guess == "rmdform") {
    show.rmdform(txt=txt)
    return()
  }

  range = doc$selection[[1]]$range
  line = range$start[1]


  show.line = filter.line = NULL
  if (single.part) {
    filter.line = line
  } else {
    show.line = line
  }

  if (is.null(am)) {
    cat("\nparse",file)
    am = parse.armd(txt=txt, dir=dir, source.file = file, show.line=show.line, filter.line = filter.line)

  }

  preview = am$opts$preview
  if (identical(preview,"rstudio")) {
    browser = rstudioapi::viewer
  } else if (identical(preview,"browser")) {
    browser = TRUE
  } else {
    browser = rstudioapi::viewer
  }

  if (isTRUE(am$rtutor)) {
    library(RTutor2)
    ps = armd.to.ps(am)
    if (isTRUE(am$opts$just.rmd)) {
      cat("\nRTutor rmd files have been created.")
      return(invisible())
    }

    app = rtutorApp(ps)
    viewApp(app,launch.browser = browser)
    return()
  }


  ui = armd.ui(am = am)

  # if (!greyout) {
  #   ui = tagList(
  #     tags$head(tags$style(type="text/css",
  #       "body.disconnected {
  #         background-color: inherit;
  #         opacity: 1;
  #       }"
  #     )),
  #     ui
  #   )
  # }

  view.html(ui=ui, browser=browser)
}

external.preview.armd.part.addin = function(single.part=TRUE,..., am=NULL) {
  library(rstudioapi)
  library(armd)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("external.preview.armd.part.addin")
  #cat("\nView frame")

  file = basename(doc$path)
  dir = dirname(doc$path)

  if (nchar(file)==0) {
    cat("\nRStudio has not detected your academic rmarkdown .rmd tab. Please try again!")
    return(invisible())
  }

  setwd(dir)

  txt = doc$contents

  # ignore guess
  #guess = guess.armd.file(txt=txt)
  #if (guess == "rmdform") {
  #  show.rmdform(txt=txt)
  #  return()
  #}

  range = doc$selection[[1]]$range
  line = range$start[1]

  #commandline.preview.armd(path=doc$path, line=line, single.part=single.part)
  #return()

  code = paste0("armd::commandline.preview.armd(path='",doc$path,"', line=",line,', single.part=',single.part,")")

  script.bin = file.path(R.home("bin"), "R")
  com = paste0(script.bin,' -e "',code,'"')
  cat(com)
  system(com,wait = FALSE)



}



# A function that can be called from the command line
# to open an shiny app that that previews a script
commandline.preview.armd = function(path, line, single.part=FALSE, auto.updates=TRUE) {
  library(rstudioapi)
  library(armd)
  restore.point("commandline.preview.armd")
  #cat("\nView frame")

  refresherArmdApp(path,show.line=line)
  return()

  # must load file
  txt = readLines(path)

  show.line = filter.line = NULL
  if (single.part) {
    filter.line = line
  } else {
    show.line = line
  }

  file = basename(path)
  dir = dirname(path)

  am = parse.armd(txt=txt, dir=dir, source.file = file, show.line=show.line, filter.line = filter.line)

  browser = TRUE

  if (isTRUE(am$rtutor)) {
    library(RTutor2)
    ps = armd.to.ps(am)
    if (isTRUE(am$opts$just.rmd)) {
      cat("\nRTutor rmd files have been created.")
      return(invisible())
    }
  } else {
    ps = NULL
  }

  ui = armd.ui(am = am)
  if (auto.updates) {
    start.autoupdate.armd.app(rmd.file=path, ui=ui, am=am, ps=ps)
  } else {
    if (isTRUE(am$rtutor)) {
      app = rtutorApp(ps)
      viewApp(app,launch.browser = browser)
    } else {
      view.html(ui=ui, browser=browser)
    }
  }
}
