parse.as.math.block = function(bi, am, use.count = TRUE, add.end="",...) {
  restore.point("armd.parse.as.math.block")
  bdf = am$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str =br$arg.str, allow.unquoted.title = TRUE)
  type = br$type

  title = args$title
  if (is.null(title)) {
    if (use.count) {
      count = sum(bdf$type[seq.int(bi)] == br$type)
      title = paste0(capitalize(br$type)," ", count,". ",args$name)
    } else {
      title = paste0(capitalize(br$type),". ",args$name)
    }
  }
  add.br = ifelse(nchar(args$name)>0,"\n","")
  if (type == "proof") {
    class = type
  } else {
    class = "mathblock"
  }
  txt = merge.lines(get.bi.inner.txt(bi=bi,am=am))
  inner.md = paste0('<span class="',class,'-title">',title,'</span>\n',add.br,txt, add.end)
  inner.html = fragment.to.html(sep.lines(inner.md),bi=bi,am=am)
  html = paste0('<div class="',class,'">', inner.html,"</div>")

  am$bdf$ui[[bi]] = am$bdf$inner.ui = HTML(html)
}


armd.parse.proposition = function(...) parse.as.math.block(...)

armd.parse.theorem = function(...) parse.as.math.block(...)

armd.parse.lemma = function(...) parse.as.math.block(...)

armd.parse.proof = function(...)
  parse.as.math.block(..., use.count=FALSE, add.end = "&#9633;")

armd.parse.conjecture = function(...) parse.as.math.block(...)

armd.parse.assumption = function(...) parse.as.math.block(...)

armd.parse.fact = function(...) parse.as.math.block(...)

armd.parse.claim = function(...) parse.as.math.block(...)

armd.parse.remark = function(...) parse.as.math.block(...)

armd.parse.definition = function(...) parse.as.math.block(...)

armd.parse.condition = function(...) parse.as.math.block(...)


capitalize = function(str) {
  paste0(toupper(substring(str,1,1)),substring(str,2))

}