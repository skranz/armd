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


createArmdOfflineSlides = function(...) {
  library(armd)
  doc = rstudioapi::getActiveDocumentContext()

  file = basename(doc$path)

  if (nchar(file)==0) {
    cat("\nRStudio has not detected your .rmd tab. Please try again!")
    return()
  }
  dir = dirname(doc$path)
  setwd(dir)
  create.offline.slides(file)
}

preview.armd.part.addin = function(single.part=TRUE,...) {
  library(rstudioapi)
  library(armd)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("preview.armd.part.addin")
  cat("\nView frame")

  file = basename(doc$path)
  dir = dirname(doc$path)

  if (nchar(file)==0) {
    cat("\nRStudio has not detected your academic rmarkdown .rmd tab. Please try again!")
    return()
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

  am = parse.armd(txt=txt, dir=dir, source.file = file, show.line=show.line, filter.line = filter.line)

  ui = armd.slide.ui(am)

  html = shiny.ui.to.html.document(ui)

  file = tempfile(fileext = ".html")
  writeLines(html, file)
  rstudio::viewer(url=file)


  #view.html(ui=ui)
}
