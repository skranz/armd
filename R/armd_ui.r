examples.make.am.html = function() {
  setwd("D:/libraries/armd/examples")
  outdir = paste0(getwd(),"/figures")

  name = "test"

  rmd.file = paste0(name,".rmd")
  am.file = paste0(name,".am")
  html.file = paste0(name,".html")


  am = fetch.am(rmd.file = rmd.file)
  ui = armd.slide.ui(am = am)
  view.html(ui=ui)

  html = shiny.ui.to.html.document(ui)
  writeLines(html, html.file)

  file = tempfile(fileext = ".html")
  writeLines(html, file)
  rstudio::viewer(url=file)
  browseURL(url=file)

  view.html(html=html)

  app = eventsApp()

  tl = offline.slide.am.ui(am=am)
  create.offline.html(tl, outfile=paste0(name,"_offline.html"),use.button = FALSE)

  tl = offline.print.slide.am.ui(am=am)
  create.offline.html(tl, outfile=paste0(name,"_offline_print.html"),use.button = FALSE)
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

# Create block type specific html heads
# E.g. some block types require some javascript or css
armd.block.html.head = function(am) {
  restore.point("armd.block.html.head")
  types = unique(am$bdf$type)
  header.funs = setdiff(unique(am$bt.df$header.fun[am$bt.df$type %in% types]),"")

  li = lapply(header.funs, function(fun) {
    do.call(fun, list(am=am))
  })
  tagList(li)
}

armd.slide.ui = function(am = NULL, am.file=NULL, rmd.file=NULL, add.page=TRUE, refreshable.content.ui = isTRUE(am$refreshable.content.ui)) {
  restore.point("armd.slide.ui")

  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)

  css = if (!is.null(am$css)) tags$head(tags$style(merge.lines(am$css))) else NULL
  head = if (!is.null(am$head)) tags$head(HTML(am$head)) else NULL
  content.ui = am$bdf$ui[[1]]
  mcss = tags$head(tags$style(math.css()))


  start.slide = first.non.null(am$start.slide,1)
  slide.ids = am$bdf$div.id[am$slide.bis]

  start.js = paste0('
    rtNumSlides = ', am$num.slides,';
    rtSlideIds = [', paste0('"',slide.ids,'"', collapse=","),'];
    rtShowSlide(',start.slide,');
    '
  )

  ui = tagList(
    tags$head(HTML("\n<!-- MyHeadStart -->\n")),

    singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css',href = 'highlightjs/styles/mycode.css'))),
    singleton(tags$head(tags$script(src = 'highlightjs/highlight.min.js',class="remove_offline", type = 'text/javascript'))),
    singleton(tags$head(tags$script(src = 'highlightjs/languages/r.min.js', class="remove_offline",type = 'text/javascript'))),
    head,
    am$header.tags,
    css,
    mcss,
    armd.block.html.head(am),

    #tags$style("table { max-width: 100%;}"),
    if (refreshable.content.ui) {
      uiOutput("maindiv",inline=FALSE, container=tags$div, content.ui)
    } else {
      div(id="maindiv",
        content.ui
      )
    },
    uiOutput("armdMsgUI"),
    singleton(tags$script(class="remove_offline_print",
      src="armd/armd_slides.js"
    )),

    tags$script(
      class="remove_offline_print",
      if (!refreshable.content.ui) HTML(start.js)
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
    am$header.tags,
    css,
    mcss,
    armd.block.html.head(am),
    div(id="maindiv",
      content.ui
    )
  )
  if (add.page) ui = bootstrapPage(ui)
  with.mathjax(ui)
}

