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

armd.ui = function(am = NULL, am.file=NULL, rmd.file=NULL, add.page=TRUE) {
  restore.point("armd.ui")

  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)
  if (isTRUE(am$slides)) {
    ui = armd.slide.ui(am=am, add.page=add.page)
  } else {
    ui = armd.page.ui(am=am, add.page=add.page)
  }

}

armd.slide.ui = function(am = NULL, am.file=NULL, rmd.file=NULL, add.page=TRUE) {
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

  ui = tagList(
    tags$head(HTML("\n<!-- MyHeadStart -->\n")),

    singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css',href = 'highlightjs/styles/mycode.css'))),
    singleton(tags$head(tags$script(src = 'highlightjs/highlight.min.js',class="remove_offline", type = 'text/javascript'))),
    singleton(tags$head(tags$script(src = 'highlightjs/languages/r.min.js', class="remove_offline",type = 'text/javascript'))),
    head,
    css,
    mcss,
    #tags$style("table { max-width: 100%;}"),
    div(id="maindiv",
      content.ui
    ),
    tags$script(class="remove_offline_print",
      src="armd/armd_slides.js"
    ),
    tags$script(
      class="remove_offline_print",
      HTML(start.js)
    )
  )
  if (add.page) ui = bootstrapPage(ui)
  with.mathjax(ui)
}

armd.page.ui = function(am = NULL, am.file=NULL, rmd.file=NULL, add.page=TRUE) {
  restore.point("armd.slide.ui")
  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)

  css = if (!is.null(am$css)) tags$head(tags$style(merge.lines(am$css))) else NULL
  head = if (!is.null(am$head)) tags$head(HTML(am$head)) else NULL
  content.ui = am$bdf$ui[[1]]
  mcss = tags$head(tags$style(math.css()))

  ui = tagList(
    singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css',href = 'highlightjs/styles/mycode.css'))),
    singleton(tags$head(tags$script(src = 'highlightjs/highlight.min.js',class="remove_offline", type = 'text/javascript'))),
    singleton(tags$head(tags$script(src = 'highlightjs/languages/r.min.js',class="remove_offline",type = 'text/javascript'))),
    head,
    css,
    mcss,
    div(id="maindiv",
      content.ui
    )
  )
  if (add.page) ui = bootstrapPage(ui)
  with.mathjax(ui)
}
