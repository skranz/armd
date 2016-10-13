get.bt = function(type,am) {
  am$bt.df[match(type,am$bt.df$type),]
}


armd.block.types.df = function(...) {
  restore.point("armd.block.types.df")

  types = c(
    "armd",
    "chunk","frame","section","subsection","subsubsection",
    "exercise",
    "preknit","precompute","portrait", "image", "solved",
    "column","row","info","note","award","references",
    "show","notest","show_notest","hint","test","test_args",
    "settings","css","head","layout",
    "toc", "figure",
    "gv",
    "theorem","assumption","condition","fact","conjecture",
    "proposition","proof","lemma","remark"
  )
  parent.types = c("frame","row", "column","chunk","preknit","precompute","knit","compute","info","note", "section","subsection","exercise")

  container.types = c("award","frame","info","note", "section","subsection","exercise","row", "column")

  n = length(types)
  bt.df = data_frame(type=types, package="armd", is.widget=FALSE, parse.inner = (type!="chunk"), is.parent=types %in% parent.types, is.container = types %in% container.types, arg.li = vector("list",n))
  bt.df
}
# data frame that specifies background information on blocktypes
make.am.block.types.df = function(am,opts=am$opts) {
  restore.point("make.am.block.type.df")

  bps = opts$block.packages
  li = lapply(seq_along(bps), function(i) {
    df = call.from.pkg(bps[[i]],"armd.block.types.df")

    df$package.pos = i
    df = add.cols.if.missing(df, is.widget=FALSE, is.parent=FALSE, parse.inner=TRUE, is.container=FALSE)
    df = df[,c("type","package","package.pos", "is.widget","is.parent","is.container", "parse.inner","arg.li")]
    df
  })
  # all block types
  all.df = bind_rows(li)
  # only the block types with highest priority
  df = group_by(all.df, type) %>% mutate(min.pos = min(package.pos)) %>% ungroup() %>% filter(package.pos == min.pos)

  am$all.bt.df = all.df
  am$bt.df = df
}


call.from.pkg = function(.pkg,.fun.name, ..., .access="::") {
  args = list(...)
  restore.point("call.from.pkg")
  fun = eval(substitute(`::`(.pkg, .fun.name), nlist(.pkg,.fun.name)))
  fun(...)
}

add.cols.if.missing = function(df, ...) {
  args = list(...)
  #restore.point("add.cols.if.missinf")

  for (col in names(args)) {
    if (!has.col(df,col)) df[[col]] = args[[col]]
  }
  df
}

