refresherArmdApp = function(rmd.file,show.line=NULL, browser=rstudio::viewer, update.am.millis = 5000) {
  restore.point("refresherArmdApp")

  # TO DO: Deal with RTutorApps
  file = basename(rmd.file)
  dir = dirname(rmd.file)

  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)

  txt = readLines(rmd.file)
  am = try(parse.armd(file = rmd.file, dir=dir, source.file = file, show.line=show.line, refreshable.content.ui=TRUE))

  if (is(am, "try-error")) {
    msg = paste0("\n",format(Sys.time),", compilation failed due to error:\n", as.character(am))
    app = eventsApp()
    app$ui = fluidPage(HTML(colored.html(msg, color="red")))
    viewApp(app,launch.browser=TRUE)
    quit()
  }

  browser = TRUE

  if (isTRUE(am$rtutor)) {
    library(RTutor3)
    ps = armd.to.ps(am)
    if (isTRUE(am$opts$just.rmd)) {
      cat("\nRTutor rmd files have been created.")
      return(invisible())
    }
    app = rtutorApp(ps)
  } else {
    ps = NULL
    app = eventsApp()
    app$ui = armd.ui(am=am)
  }


  app$glob$old.am = app$glob$new.am = am
  app$glob$start.slide = NULL
  app$glob$first.time = TRUE
  # will be called when app is refreshed
  init.handler = function(session,...) {
    restore.point("resfreshArmdAppInit")

    app=getApp()
    timedMessage("armdMsgUI",html= colored.html("Recompile..."), millis=Inf)

    txt = readLines(rmd.file)
    #if (!isTRUE(app$glob$first.time)) {
      am = try(parse.armd(txt=txt, dir=dir, source.file = file, show.line=show.line, start.slide = app$glob$start.slide))
      # could not compile without error
      if (is(am,"try-error")) {
        msg = paste0("\n",format(Sys.time),", compilation failed due to error:\n", as.character(am))
        timedMessage("armdMsgUI",html= colored.html(msg, color="#cc0000"), millis=Inf)
        cat(msg)
        return()
      }
    app$glob$new.am = am
    timedMessage("armdMsgUI",html= colored.html(""), millis=Inf)

    if (!isTRUE(app$glob$first.time))
      timedMessage("armdMsgUI",html= colored.html("New session started..."), millis=3000)

    am = app$glob$new.am
    app$glob$first.time = FALSE
    app$was.changed = first.non.null(app$was.changed, 0)

    start.slide = first.non.null(app$glob$start.slide, am$start.slide,1)
    cat("\nStart with slide ", start.slide)

    slide.ids = am$bdf$div.id[am$slide.bis]
    start.js = paste0('
      rtNumSlides = ', am$num.slides,';
      rtSlideIds = [', paste0('"',slide.ids,'"', collapse=","),'];
      rtShowSlide(',start.slide,',true);
      '
    )

    content.ui = tagList(
      with.mathjax(am$bdf$ui[[1]]),
      tags$script(HTML(start.js))
    )
    setUI("maindiv",
      content.ui
    )
    dsetUI("maindiv",
      content.ui
    )
    evalJS("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")
    mathjax.updater = observe({
      restore.point("mathjax.updater")
      app=getApp()
      app$was.changed = first.non.null(app$was.changed, 0)+1
      if (app$was.changed<3) {
        evalJS("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")
        invalidateLater(1000)
      }
    })
  }
  appInitHandler(init.handler)

  eventHandler(eventId = "armdShowSlide", id="armdShowSlide",fun = function(slideNum=1, ..., app=getApp()) {
    restore.point("slideChangedEvent")
    app$glob$start.slide = slideNum
    cat("\nSlide ", app$glob$start.slide, " shown.")
  })

  runEventsApp(app, launch.browser=browser)
  quit()
}




recompile.autoupdate.am = function(rmd.file, start.slide=1, old.am=get.am(), app=getApp()) {
  restore.point("recompile.autoupdate.am")

  file = basename(rmd.file)
  dir = dirname(rmd.file)

  #app$am = NULL
  am = try(parse.armd(dir=dir, file=file , start.slide=start.slide))

  # could not compile without error
  if (is(am,"try-error")) {
    msg = paste0("\n",format(Sys.time),", auto update failed due to error:\n", as.character(am))
    timedMessage("armdMsgUI",html= colored.html(msg, color="#cc0000"), millis=50000)
    cat(msg)
    return()
  }

  app$am = am
  # auto update for RTutor not yet implemented
  if (isTRUE(am$rtutor)) {
    library(RTutor3)
    ps = armd.to.ps(am)
    app$ps = ps
    #RTutor3::get.ps
  }
  content.ui = am$bdf$ui[[1]]


  setUI("maindiv",tagList(
    content.ui
  ))

  dsetUI("maindiv",tagList(
    content.ui
  ))
  evalJS(paste0("rtNumSlides = ",am$num.slides,"; rtShowSlide(",start.slide,",true);"))
  evalJS("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")
  timedMessage("armdMsgUI",html= colored.html(paste0("<hr>Slides updated: ", format(Sys.time()))),millis = 5000)
}

start.autoupdate.armd.app = function(rmd.file,ui, am,ps=am, browser=TRUE) {
  restore.point("start.autoupdate.app")

  # TO DO: Deal with RTutorApps
  app = eventsApp()
  app$am = am
  app$ps = ps
  file = basename(rmd.file)
  dir = dirname(rmd.file)

  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)
  app$ui = ui
  app$rmd.file.info = file.info(rmd.file)
  app$glob$slideNum = am$start.slide
  init.handler = function(...) {
    fileChecker = observe({
      restore.point("fileChecker")
      app=getApp()
      #cat("\n check file change...")
      rmd.file.info = file.info(rmd.file)
      # rmd file has changed
      if (!identical(rmd.file.info,app$rmd.file.info)) {
        app$rmd.file.info = rmd.file.info
        start.slide = app$glob$slideNum
        cat("\nRmd file has been changed... Current slide is ", start.slide)
        restore.point("fileCheckerchange")
        app$was.changed = 1
        recompile.autoupdate.am(rmd.file, start.slide = start.slide)
        invalidateLater(1000)
        return()
      } else if (isTRUE(app$was.changed<2)) {
        if (app$was.changed==1)
          evalJS(paste0("rtShowSlide(",app$glob$slideNum,",true);"))
        app$was.changed = app$was.changed +1
        evalJS("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")

      }
      # check every second
      invalidateLater(1000)
    })
  }

  appInitHandler(init.handler)

  eventHandler(eventId = "armdShowSlide", id="armdShowSlide",fun = function(slideNum=1, ..., app=getApp()) {
    restore.point("slideChangedEvent")
    app$glob$slideNum = slideNum
    cat("\nSlide ", app$glob$slideNum, " shown.")
  })

  #setUI("MainAppContainer",ui)
  #dsetUI("MainAppContainer",ui)
  runEventsApp(app, launch.browser=browser)
}

