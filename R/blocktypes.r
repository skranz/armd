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
    "column","row","info","note","references",
    "show","notest","show_notest","hint","test","test_args",
    "settings","css","head","layout",
    "toc", "figure", "spoilerNote",
    "gv",
    "tab",
    "definition", "theorem","assumption","condition","fact","conjecture",
    "proposition","proof","lemma","define",
    "box",
    "define"
  )
  parent.types = c("frame","row", "column","chunk","preknit","precompute","knit","compute","info","note", "section","subsection","exercise","required")

  container.types = c("frame","info","note","spoilerNote", "section","subsection","exercise","row", "column")

  dot.levels = c(
    armd = -1000,
    section = -3,
    subsection = -2,
    subsubsection = -1,
    frame = 1,
    row = 2,
    references = 2,
    column = 3,
    success = 3,
    when = 3
  )
  dot.level = dot.levels[types]

  n = length(types)
  bt.df = data_frame(type=types, package="armd", is.widget=FALSE, parse.inner.blocks = (type!="chunk"), remove.inner.blocks=FALSE, is.parent=types %in% parent.types, is.container = types %in% container.types, dot.level=dot.level, arg.li = vector("list",n))

  bt.df
}
# data frame that specifies background information on blocktypes
make.am.block.types.df = function(am,opts=am$opts) {
  restore.point("make.am.block.type.df")

  bps = opts$block.packages
  bl = opts$block.libs

  packages = unique(c(bps, unlist(bl)))
  li = lapply(seq_along(packages), function(i) {
    pkg = packages[[i]]
    if (!require(pkg,character.only = TRUE)) {
      stop("You need to install the R package '",pkg,"' to compile this file. The package may not be on CRAN and possibly must be installed from Github. Best google the package name for installation instructions.")
    }
    df = call.from.pkg(pkg,paste0(pkg,".block.types.df"))

    df$package.pos = i
    df = add.cols.if.missing(df, is.widget=FALSE, is.parent=FALSE, parse.inner.blocks=TRUE, remove.inner.blocks=FALSE, is.container=FALSE, dot.level=NA, arg.li = vector("list",NROW(df)), header.fun="")
    df = df[,c("type","package","package.pos", "is.widget","is.parent","is.container", "parse.inner.blocks","remove.inner.blocks", "arg.li","header.fun")]
    df
  })
  # all block types
  all.df = bind_rows(li)
  for (type in names(bl)) {
    row = which(all.df$type == type & all.df$package == bl[[type]])[1]
    if (is.na(row)) {
      stop(paste0("In your setting block.libs you assign the type ", type, " to the package ", bl[[type]], " but this type is not defined in that package."))
      next
    }
    all.df$package.pos[[row]] = 0
  }

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

# Some default packages for addons
# Can be used to automatically load packages
armd.block.type.default.packages = function() {
  c(pane="EconCurves",plotpane="EconCurves",panequiz="EconCurves",
    preknit="RTutor3",precompute="RTutor3",award="RTutor3",
    quiz="RTutor3")

}

add.block.default.packages.to.opts = function(types, opts) {
  pkg = armd.block.type.default.packages()
  pkg = pkg[names(pkg) %in% types]
  opts$block.packages = unique(c(opts$block.packages,pkg))
  opts
}

