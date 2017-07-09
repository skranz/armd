examples.offline.html = function() {
  setwd("D:/libraries/armd/examples")
  outdir = paste0(getwd(),"/figures")

  name = "offline_test"
  rmd.file = paste0(name,".rmd")

  #str(am$header.tags[[1]])
  app = armdPrintApp(rmd.file=rmd.file)
  viewApp(app, launch.browser=TRUE)
}



armdPrintApp = function(am=NULL, rmd.file=NULL, dir = getwd(),...) {
  restore.point("armdPrintApp")
  if (is.null(am)) {
    am = parse.armd(file = rmd.file)
  }

  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)

  app = eventsApp()
  ui = armd.ui(am,add.page = TRUE)
  js = '
  $(".armd-static-container-div").css({"display": "block", "visibility": "visible"});
  $(".collapse").css({"display": "block"});
  $(".remove_offline_print").css({"display": "none"});
  $(".container-block-title").css({"display": "block"});
  $(".container-inline-title").css({"display": "inline"});
  $(".btn").css({"display": "none"});
  '
  ui = tagList(
    tags$style(HTML(slides.print.screen.css())),
    ui,
    bottomScript(HTML(js))
  )
  app$ui = ui
  app
}

