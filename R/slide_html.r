


examples.make.am.html = function() {
  setwd("D:/libraries/armd/examples")
  outdir = paste0(getwd(),"/figures")

  am.name = "test"

  rmd.file = paste0(am.name,".rmd")
  am.file = paste0(am.name,".am")
  html.file = paste0(am.name,".html")


  am = fetch.am(rmd.file = rmd.file)
  ui = armd.slide.ui(am = am)
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

armd.slide.ui = function(am = NULL, am.file=NULL, rmd.file=NULL) {
  restore.point("offline.slide.am.ui")

  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)

  css = if (!is.null(am$css)) tags$head(tags$style(am$css)) else NULL
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
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/jquery.min.js"))),
    tags$head(includeCSS(paste0(path.package("shiny"),"/www/shared/bootstrap/css/bootstrap.css"))),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/bootstrap/js/bootstrap.min.js"))),
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
    ),
    tags$script(class="remove_me",
      '
      ')
  )
  ui
}


create.offline.slides = function(rmd.file, am.name = tools::file_path_sans_ext(basename(rmd.file))) {
  am = fetch.am(rmd.file = rmd.file)

  tl = offline.slide.am.ui(am=am)
  create.offline.html(tl, outfile=paste0(am.name,"_offline.html"),use.button = FALSE)

  tl = offline.print.slide.am.ui(am=am)
  create.offline.html(tl, outfile=paste0(am.name,"_offline_print.html"),use.button = FALSE)

}


create.offline.html = function(tagList=NULL, outfile, head=NULL, body=NULL, use.button = FALSE, launch.browser=TRUE, browse=TRUE) {
  restore.point("save.as.offline.html")

  if (is.null(head) & is.null(body)) {
    rendered = htmltools::renderTags(tagList)
    head = rendered$head
    body = rendered$html
  }

  app = eventsApp(adapt.ui = FALSE)
  app$ui = tagList(
    tags$head(head),
    mathjax.to.offline(container.id = NULL, use.button=use.button),
    with.mathjax(body)
  )
  eventHandler(eventId="downloadHtmlPage", id=NULL, fun=function(...) {
    args = list(...)
    restore.point("downloadHtmlPage")
    html = args$html
    Encoding(html) = "UTF-8"
    html = sep.lines(html)
    start.line = which(html == "<!-- MyHeadStart -->")[1]
    html = html[start.line:length(html)]
    html = c(
'<!DOCTYPE html>

<head>

<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
',
      html)
    getApp()$session$sendCustomMessage(type = "closeWindow", message = "message")
    stopApp(invisible(html))

  })
  html = viewApp(app,launch.browser = launch.browser)
  if (!is.null(outfile)) {
    writeUtf8(merge.lines(html), outfile)
    if (browse)
      browseURL(outfile)

  }

  restore.point("save.as.offline.html.res")

  invisible(html)

}


offline.slide.am.ui = function(am = NULL, am.file=NULL, rmd.file=NULL) {
  restore.point("offline.slide.am.ui")

  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)

  css = if (!is.null(am$css)) tags$head(tags$style(am$css)) else NULL
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
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/jquery.min.js"))),
    tags$head(includeCSS(paste0(path.package("shiny"),"/www/shared/bootstrap/css/bootstrap.css"))),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/bootstrap/js/bootstrap.min.js"))),
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
    ),
    tags$script(class="remove_me",
      '
      ')
  )
  ui
}


offline.print.slide.am.ui = function(am = NULL, am.file=NULL, rmd.file=NULL) {
  restore.point("offline.print.slide.am.ui")

  am = fetch.am(am=am, am.file=am.file, rmd.file = rmd.file)

  css = if (!is.null(am$css)) tags$head(tags$style(am$css)) else NULL
  head = if (!is.null(am$head)) tags$head(HTML(am$head)) else NULL
  content.ui = am$bdf$ui[[1]]

  content = htmltools::renderTags(content.ui)$html

  #content = unicode.html.math(content)

  mcss = tags$head(tags$style(math.css()))

  ui = bootstrapPage(

    tags$head(HTML("\n<!-- MyHeadStart -->\n")),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/jquery.min.js"))),
    tags$head(includeCSS(paste0(path.package("shiny"),"/www/shared/bootstrap/css/bootstrap.css"))),
    tags$style(HTML(slides.print.screen.css())),

    head,
    css,
    mcss,

    HTML(content),
    tags$script(class="remove_me",
      '
$("div").css("display","block");
$(".nav_buttons_div").remove();
      ')
  )
  ui
}

shiny.ui.to.html.document = function(ui) {
  restore.point("shiny.ui.to.html")

  rendered = htmltools::renderTags(ui)
  html = simple.html.page(head=rendered$head, body = rendered$html)
  html
}

offline.mathjax = function() {

}


slides.print.screen.css = function() {
  '
  body {
    margin-left: 10%;
    margin-right: 10%;
  }
  '

}


fetch.am = function(am = NULL, am.file=NULL, rmd.file=NULL, txt=NULL, am.name=NULL) {

  restore.point("fetch.am")

  if (!is.null(rmd.file)) {
    txt = readLines(rmd.file)
    if (is.null(am.name)) {
      am.name = tools::file_path_sans_ext(basename(rmd.file))
    }
  }
  if (!is.null(txt)) {
    if (is.null(am.name)) am.name = "armd"
    am = parse.armd(txt=txt, am.name=am.name)
  } else if (!is.null(am.file)) {
    am = read.am(am.file)
  }
  am
}
