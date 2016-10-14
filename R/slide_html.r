examples.make.am.html = function() {
  setwd("D:/libraries/armd/examples")
  outdir = paste0(getwd(),"/figures")

  am.name = "test"

  rmd.file = paste0(am.name,".rmd")
  am.file = paste0(am.name,".am")
  html.file = paste0(am.name,".html")


  am = fetch.am(rmd.file = rmd.file)
  ui = armd.slide.ui(am = am)
  view.html(ui=ui)

  html = shiny.ui.to.html.document(ui)
  writeLines(html, html.file)

  file = tempfile(fileext = ".html")
  writeLines(html, file)
  rstudio::viewer(url=file)
  browseURL()

  view.html(html=html)

  app = eventsApp()

  tl = offline.slide.am.ui(am=am)
  create.offline.html(tl, outfile=paste0(am.name,"_offline.html"),use.button = FALSE)

  tl = offline.print.slide.am.ui(am=am)
  create.offline.html(tl, outfile=paste0(am.name,"_offline_print.html"),use.button = FALSE)
}

armd.ui = function(am = NULL, am.file=NULL, rmd.file=NULL) {
  restore.point("armd.ui")

  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)
  if (isTRUE(am$slides)) {
    ui = armd.slide.ui(am=am)
  } else {
    ui = armd.page.ui(am=am)
  }

}

armd.slide.ui = function(am = NULL, am.file=NULL, rmd.file=NULL) {
  restore.point("armd.slide.ui")

  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)

  css = if (!is.null(am$css)) tags$head(tags$style(merge.lines(am$css))) else NULL
  head = if (!is.null(am$head)) tags$head(HTML(am$head)) else NULL
  content.ui = am$bdf$ui[[1]]
  mcss = tags$head(tags$style(math.css()))


  slide.ids = am$bdf$div.id[am$slide.bis]
  start.js = paste0('
    rtNumSlides = ', am$num.slides,';
    rtSlideIds = [', paste0('"',slide.ids,'"', collapse=","),'];
    rtShowSlide(1);
    '
  )

  ui = bootstrapPage(
    tags$head(HTML("\n<!-- MyHeadStart -->\n")),

    singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css',href = 'highlightjs/styles/mycode.css'))),
    singleton(tags$head(tags$script(src = 'highlightjs/highlight.min.js',type = 'text/javascript'))),
    singleton(tags$head(tags$script(src = 'highlightjs/languages/r.min.js',type = 'text/javascript'))),
    head,
    css,
    mcss,
    #tags$style("table { max-width: 100%;}"),
    div(id="maindiv",
      content.ui
    ),
    includeScript(paste0(path.package("armd"),"/www/offline_slides.js")),
    tags$script(
      HTML(start.js)
    )
  )
  withMathJax(ui)
}

armd.page.ui = function(am = NULL, am.file=NULL, rmd.file=NULL) {
  restore.point("armd.slide.ui")
  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)

  css = if (!is.null(am$css)) tags$head(tags$style(merge.lines(am$css))) else NULL
  head = if (!is.null(am$head)) tags$head(HTML(am$head)) else NULL
  content.ui = am$bdf$ui[[1]]
  mcss = tags$head(tags$style(math.css()))

  ui = bootstrapPage(
    tags$head(HTML("\n<!-- MyHeadStart -->\n")),

    singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css',href = 'highlightjs/styles/mycode.css'))),
    singleton(tags$head(tags$script(src = 'highlightjs/highlight.min.js',type = 'text/javascript'))),
    singleton(tags$head(tags$script(src = 'highlightjs/languages/r.min.js',type = 'text/javascript'))),
    head,
    css,
    mcss,
    div(id="maindiv",
      content.ui
    )
  )
  withMathJax(ui)
}

