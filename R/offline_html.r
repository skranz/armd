examples.offline.html = function() {
  setwd("D:/libraries/armd/examples")
  outdir = paste0(getwd(),"/figures")

  am.name = "test"

  rmd.file = paste0(am.name,".rmd")
  am.file = paste0(am.name,".am")
  html.file = paste0(am.name,".html")


  am = fetch.am(rmd.file = rmd.file)

  html = armd.offline.html(am=am)

  ui = armd.slide.ui(am = am)

  rendered = htmltools::renderTags(ui)
  head = rendered$head
  body = rendered$html

  head = inline.external.html(head)
  body = inline.external.html(body)

  cat(head)
  cat(body)
}

armd.offline.html = function(am, outfile=paste0(am$am.name,"_offline.html"),print.outfile=paste0(am$am.name,"_offline_print.html"), browse=TRUE) {
  restore.point("create.offline.am")

  ui = armd.ui(am,add.page = FALSE)
  ui = tagList(
    tags$style(class="just_offline_print",HTML(slides.print.screen.css())),
    ui
  )
  html = as.inlined.mathjax.html(ui,use.button = FALSE)
  restore.point("create.offline.am.inner")

  org.html = html
  library(XML)
  library(selectr)
  h = htmlParse(html)

  nodes = querySelectorAll(h,".remove_offline, #MathJax_Message")
  for(node in nodes) XML::removeNodes(node)

  del.files=c("MathJax.js","highlight.min.js","r.min.js","json2-min.js","shiny.css","shiny.min.js","shinyBS.css","shinyBS.js","babel-polyfill.min.js","font-awesome.min.css")
  del.path = NULL
  html = inline.external.html(h=h,del.path=del.path,del.files=del.files)

  work.html = xml2html(h)

  nodes = querySelectorAll(h,".just_offline_print")
  for(node in nodes) XML::removeNodes(node)

  html = xml2html(h)

  if (!is.null(outfile)) {
    writeUtf8(merge.lines(html), outfile)
    if (browse)
      browseURL(outfile)
  }

  # make offline print html
  h = htmlParse(work.html)

  nodes = querySelectorAll(h,".inlined_script_bootstrap_min_js")
  for(node in nodes) XML::removeNodes(node)

  html = inline.external.html(h=h,del.files=del.files, just.delete = TRUE)

  nodes = querySelectorAll(h,".remove_offline_print")
  for(node in nodes) XML::removeNodes(node)

  nodes = querySelectorAll(h,"div")
  for(node in nodes) set.node.css(node,display="block")
  nodes = querySelectorAll(h,".slide-container-div")
  for(node in nodes) set.node.css(node, "padding-left"=NULL, "padding-right"=NULL)

  print.html = xml2html(h)
  if (!is.null(print.outfile)) {
    writeUtf8(merge.lines(print.html), print.outfile)
    if (browse)
      browseURL(print.outfile)
  }

  invisible(html)
}


inline.external.html = function(html, h=NULL, inline.script=TRUE, inline.css=TRUE, del.files=NULL, ignore.files=NULL, del.path=NULL, just.delete=FALSE) {
  restore.point("inline.external.html")

  library(XML)
  library(selectr)
  if (is.null(h))
    h = XML::htmlParse(html, asText=TRUE)

  ##################################################
  # inline external css
  ##################################################
  nodes = querySelectorAll(h, "link")
  src = unlist(lapply(nodes, function(node) {
    res = xmlAttrs(node)["href"]
    if (is.null(res)) return(NA)
    res
  }))
  rows = !is.na(src)
  nodes = nodes[rows]
  src = src[rows]

  res = findShinyRessourceDir(src)

  rows = !(res$path %in% del.path)
  for (node in nodes[!rows]) removeNodes(node)

  restore.point("dhfgdhfgzuegfzu")
  nodes = nodes[rows]
  dirs = res$dir[rows]
  paths = res$path[rows]



  for (i in seq_along(nodes)) {
    node = nodes[[i]]
    dir  = dirs[[i]]
    path = paths[[i]]
    file = basename(dir)

    if (any(str.start.equal(file, del.files))) {
      removeNodes(node)
      next
    }
    if (any(str.start.equal(file, ignore.files))) {
      next
    }
    if (just.delete) next

    cat("\ninline style ", file)
    code = merge.lines(c(
      paste0("\n /* INLINED ",file,"*/ \n"),
      readLines(dir, warn = FALSE),
      paste0("\n /* END INLINED ",file,"*/ \n")
    ))
    code = mark_utf8(code)
    file_class = gsub(".","_",file,fixed=TRUE)
    replace.link.with.style.node(node,code, attr=c(class=paste0("inlined_style inlined_style_", file_class)))
  }

  ##################################################
  # inline external scripts
  ##################################################
  nodes = querySelectorAll(h, "script")
  src = unlist(lapply(nodes, function(node) {
    res = xmlAttrs(node)["src"]
    if (is.null(res)) return(NA)
    res
  }))
  rows = !is.na(src)
  nodes = nodes[rows]
  src = src[rows]

  res = findShinyRessourceDir(src)

  rows = !(res$path %in% del.path)
  # remove external scripts
  for (node in nodes[!rows]) removeNodes(node)

  nodes = nodes[rows]
  dirs = res$dir[rows]
  paths = res$path[rows]

  for (i in seq_along(nodes)) {
    node = nodes[[i]]
    dir  = dirs[[i]]
    path = paths[[i]]
    file = basename(dir)

    if (any(str.start.equal(file, del.files))) {
      removeNodes(node)
      next
    }
    if (any(str.start.equal(file, ignore.files))) {
      next
    }
    if (just.delete) next
    #restore.point("nhfbrbfrbf")

    code = merge.lines(c(
      paste0("\n /* INLINED ",file,"*/ \n"),
      readLines(dir, warn = FALSE,encoding = "UTF-8"),
      paste0("\n /* END INLINED ",file,"*/ \n")
    ))
    code = mark_utf8(code)
    cat("\ninline script ", file)
    file_class = gsub(".","_",file,fixed=TRUE)

    replace.script.node(node,code, attr=c(class=paste0("inlined_script inlined_script_", file_class)))
  }

  xml2html(h)
}

str.start.equal = function(str, vec) {
  if (length(vec)==0) return(NULL)
  short = substring(str,1,nchar(vec))
  short == vec
}

findShinyRessourceDir = function(src) {
  restore.point("findShinyRessourceDir")


  # get path name
  names = str.left.of(src,"/")

  # distinguish local and web ressources
  is.web = str.starts.with(names,"http") & has.substr(names,":")

  loc.src = src[!is.web]
  names = names[!is.web]
  right = str.right.of(loc.src,"/")

  sr = shiny:::.globals$resources

  dirs = sapply(sr[names], function(sr) sr$directoryPath)
  dirs[names=="shared"] = paste0(path.package("shiny"),"/www/shared")
  dirs = file.path(dirs,right)
  src[!is.web] = dirs
  path = src
  path[!is.web] = names

  list(dir=src, is.web =is.web, path=path)
}



#' Takes a shiny tagList and returns an html txt
#' in which mathjax is inlined
as.inlined.mathjax.html = function(x, page.fun = bootstrapPage, launch.browser=TRUE, use.button=FALSE) {
  restore.point("ui.to.inlined.mathjax.html")

  if (is.character(x)) x = HTML(x)
  ui = page.fun(
    mathjax.to.offline.code(container.id = NULL, use.button=use.button),
    with.mathjax(x)
  )
  app = eventsApp(adapt.ui = FALSE)
  app$ui = ui

  eventHandler(eventId="downloadHtmlPage", id=NULL, fun=function(...) {
    args = list(...)
    restore.point("downloadHtmlPage")
    html = args$html
    Encoding(html) = "UTF-8"
    getApp()$session$sendCustomMessage(type = "closeWindow", message = "message")
    stopApp(invisible(html))
  })
  html = viewApp(app,launch.browser = launch.browser)
}

offline.shiny.headers = function() {
  tagList(
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/jquery.min.js"))),
    tags$head(includeCSS(paste0(path.package("shiny"),"/www/shared/bootstrap/css/bootstrap.css"))),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/bootstrap/js/bootstrap.min.js")))
  )
}

shiny.ui.to.html.document = function(ui) {
  restore.point("shiny.ui.to.html")

  rendered = htmltools::renderTags(ui)
  html = simple.html.page(head=rendered$head, body = rendered$html)
  html
}


slides.print.screen.css = function() {
  '
  body {
    margin-left: 10%;
    margin-right: 10%;
  }
  '

}


mathjax.to.offline.code = function(container.id=NULL, use.button=TRUE) {

  inner.target = if (is.null(container.id)) {
    'document.documentElement'
  } else {
    paste0('document.getElementById("',container.id,'")')
  }

  js = paste0('
  Shiny.addCustomMessageHandler("closeWindow", function(m) {window.close();});

  function utf8_to_b64( str ) {
    return window.btoa(unescape(encodeURIComponent( str )));
  }
  function remove_mathjax_and_save() {
    $(".mathjax_load").remove();
    $(".mathjax_typeset").remove();
    $(".MJX_Assistive_MathML").remove();
    $(".remove_me").remove();
    $("#saveOfflineDiv").remove();
    Shiny.onInputChange("downloadHtmlPage", {id: "downloadHtmlPage", html: ', inner.target,'.innerHTML});
  }
  ')

  if (use.button) {
    js = paste0(js,'
$("#saveOfflineBtn").click(function(e) {
  remove_mathjax_and_save();
});'
    )
    btn = actionButton("saveOfflineBtn", "Save as offline html")
  } else {
    js = paste0(js,'
MathJax.Hub.Queue(function () {
  remove_mathjax_and_save();

});'
    )
    btn = NULL
  }

  tagList(
    div(id="saveOfflineDiv", class="remove_me",
      btn,
      tags$script(js)
    )

  )

}