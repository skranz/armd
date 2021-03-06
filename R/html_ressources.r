
highlight.code.script = function() {
  tags$script(class="remove_offline", "$('pre code.r, pre code.language-r').each(function(i, e) {hljs.highlightBlock(e)});")
}


addArmdRessourcePath = function() {
  dir = paste0(system.file('www', package='armd'),"/highlightjs")
  shiny::addResourcePath('highlightjs', paste0(system.file('www', package='armd'),"/highlightjs"))
  shiny::addResourcePath('armd', paste0(system.file('www', package='armd')))
}
