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

showArmdAddin = function(...) {
  preview.armd.part.addin(single.part=FALSE)
}

compileArmdDocument = function(...) {
  restore.point("compileArmdDocument")

  library(armd)
  doc = rstudioapi::getActiveDocumentContext()

  file = basename(doc$path)

  if (nchar(file)==0) {
    cat("\nRStudio has not detected your .rmd tab. Please try again!")
    return()
  }
  dir = dirname(doc$path)
  setwd(dir)
  am = parse.armd(file=file, dir=dir, source.file = file)

  if (isTRUE(am$rtutor)) {
    preview.armd.part.addin(am=am,...)
  } else {
    createArmdOfflineSlides(am, doc)
  }
}

createArmdOfflineSlides = function(am,doc) {
  restore.point("createArmdOfflineSlides")

  armd.offline.html(am)
}

preview.armd.part.addin = function(single.part=TRUE,...,am=NULL) {
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
    browser = rstudio::viewer
  } else if (identical(preview,"browser")) {
    browser = TRUE
  } else {
    browser = rstudio::viewer
  }

  if (isTRUE(am$rtutor)) {
    library(RTutor3)
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
  view.html(ui=ui, browser=browser)
}
