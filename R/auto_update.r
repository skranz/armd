recompile.autoupdate.am = function(rmd.file, old.am=get.am(), app=getApp()) {
  restore.point("recompile.autoupdate.am")

  file = basename(rmd.file)
  dir = dirname(rmd.file)

  #app$am = NULL
  am = try(parse.armd(dir=dir, file=file , show.line=NULL))

  # could not compile without error
  if (is(am,"try-error")) {
    cat("\nAuto update failed due to error:\n", as.character(am))
    return()
  }

  app$am = am
  # auto update for RTutor not yet implemented
  if (isTRUE(am$rtutor)) {
    library(RTutor3)
    ps = armd.to.ps(am)
    #app = rtutorApp(ps)
    #viewApp(app,launch.browser = browser)
    return()
  }

  ui = armd.ui(am = am)
  setUI("MainAppContainer",ui)
  dsetUI("MainAppContainer",ui)

}

start.autoupdate.armd.app = function(rmd.file,ui, am, browser=TRUE) {
  restore.point("start.autoupdate.app")

  app = eventsApp()
  file = basename(rmd.file)
  dir = dirname(rmd.file)

  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)
  app$ui = fluidPage(uiOutput("MainAppContainer"))
  app$rmd.file.info = file.info(rmd.file)
  init.handler = function(...) {
    fileChecker = observe({
      restore.point("fileChecker")
      #cat("\n check file change...")
      rmd.file.info = file.info(rmd.file)
      # rmd file has changed
      if (!identical(rmd.file.info,app$rmd.file.info)) {
        cat("\nRmd file has been changed...")
        recompile.autoupdate.am(rmd.file)
        app$rmd.file.info = rmd.file.info
      }
      # check every second
      invalidateLater(1000)
    })
  }

  appInitHandler(init.handler)

  setUI("MainAppContainer",ui)
  #dsetUI("MainAppContainer",ui)
  runEventsApp(app, launch.browser=browser)
}

