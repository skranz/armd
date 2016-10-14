
highlight.code.script = function() {
  tags$script("$('pre code.r').each(function(i, e) {hljs.highlightBlock(e)});")
}


addArmdRessourcePath = function() {
  dir = paste0(system.file('www', package='armd'),"/highlightjs")
  shiny::addResourcePath('highlightjs', paste0(system.file('www', package='armd'),"/highlightjs"))
  shiny::addResourcePath('highlightjs', paste0(system.file('www', package='armd'),"/highlightjs"))
}
