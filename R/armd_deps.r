
# will be called when a chunk is rendered
# mainly relevant if a html widget is rendered
armd.add.meta = function(am, meta) {
  restore.point("armd.add.meta")

  is.head = unlist(lapply(meta, function(el) is(el,"shiny_head")))
  is.dep = unlist(lapply(meta, function(el) is(el,"html_dependency")))

  am$dependencies = c(am[["dependencies"]], meta[is.dep])
  am$header.tags  = c(am$header.tags, meta[is.head])

}
