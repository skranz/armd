examples.html.modify = function() {
html = '<script> x = 1 < 2; alert(x); <!-- comment --></script><p>Hi</p>
<svg width="100%" height="100%" viewBox="0 0 100 100"
     xmlns="http://www.w3.org/2000/svg">
  <style><![CDATA[
    circle {
      fill: orange;
      stroke: black;
      stroke-width: 10px; /* Note: The value of a pixel depends
                             on the view box */
  }]]></style>

  <circle cx="50" cy="50" r="40" />
</svg>
<script src="hi"></script>
'
x = htmlParse(html, asText=TRUE)
saveXML(x)

nodes = querySelectorAll(x, "script")
replace.script.node(nodes[[1]],"1<12;",attr=NULL)
replace.script.node(nodes[[2]],"1>12;",attr=NULL)

xml2html(x)
}

replace.script.node = function(node, value="", attr=NULL) {
  restore.point("replace.script.node")
  nn = xmlNode(name="script",xmlCDataNode(value), attrs=attr)
  invisible(replaceNodes(node,nn))
}

replace.link.with.style.node = function(node, value="", attr=NULL) {
  nn = xmlNode(name="style",xmlCDataNode(value), attrs=attr)
  invisible(replaceNodes(node,nn))
}

set.node.css = function(node, ...) {
  new.css = list(...)
  restore.point("set.node.css")


  attrs = xmlAttrs(node)
  style = attrs["style"]
  if (is.na(style)) {
    css = list()
  } else {
    css=style.string.to.list(style)
  }
  css[names(new.css)] = new.css
  str = style.list.to.string(css)
  attrs["style"] = str
  'xmlAttrs<-'(node, append = TRUE,value=attrs)
  #xmlAttrs(node,append=FALSE) <- attrs
}

style.string.to.list = function(style) {
  restore.point("css.string.to.list")
  el = strsplit(merge.lines(style),";",fixed=TRUE)[[1]]
  names = str.trim(str.left.of(el,":"))
  vals = str.trim(as.list(str.right.of(el,":")))
  names(vals) = names
  vals
}
style.list.to.string = function(style) {
  restore.point("style.list.to.string")

  style=style[!sapply(style, is.null)]
  paste0(names(style),":",style,collapse=";")
}


xml2html = function(x) saveXML(x)
