get.bt = function(type,am) {
  am$bt.df[match(type,am$bt.df$type),]
}


armd.block.types.df = function(...) {
  restore.point("armd.block.types.df")

  types = c(
    "armd",
    "chunk","frame","chapter", "section","subsection","subsubsection",
    "exercise",
    "preknit","precompute","portrait", "image", "solved",
    "info","note","references",
    "show","notest","show_notest","hint","test","test_args",
    "settings","css","head","layout",
    "toc", "figure", "spoilerNote",
    "gv",
    "tab",
    "definition", "theorem","assumption","condition","fact","conjecture",
    "proposition","proof","lemma","define",
    "box",
    "define","html"
  )
  parent.types = c("frame","chunk","preknit","precompute","knit","compute","info","note","chapter", "section","subsection","exercise","required")

  container.types = c("frame","info","note","spoilerNote","chapter", "section","subsection","exercise")

  dot.levels = c(
    armd = -1000,
    chapter = -4,
    section = -3,
    subsection = -2,
    subsubsection = -1,
    frame = 1,
    references = 2,
    sucess = 3,
    when = 4
  )
  dot.level = dot.levels[types]

  n = length(types)
  bt.df = data_frame(type=types, package="armd", is.widget=FALSE, parse.inner.blocks = (type!="chunk"), remove.inner.blocks=FALSE, is.parent=types %in% parent.types, is.container = types %in% container.types, dot.level=dot.level, arg.li = vector("list",n))

  bt.df
}


armd.dot.levels = function(am) {
  dot.levels = c(
    armd = -1000,
    chapter = -4,
    section = -3,
    subsection = -2,
    subsubsection = -1,
    frame = 1,
    references = 2,
    success = 3,
    when = 3
  )
  backto = dot.levels+1
  names(backto) = paste0("back_to_",names(dot.levels))
  lev = c(dot.levels, backto)
  lev = rank(lev,ties.method = "min")
  lev
}



# data frame that specifies background information on blocktypes
make.am.block.types.df = function(am,opts=am$opts) {
  restore.point("make.am.block.type.df")

  bl = opts$block.libs

  packages = unique(c("armd",if (isTRUE(opts$rtutor)) "RTutor2", unlist(bl)))

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

  # type-package combinations which are explicitly listed in block.libs
  # get priority.
  # We set the package.pos of those rows to 0, i.e. they get priority.
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
armd.block.type.default.libs = function(opts) {
  restore.point("armd.block.type.default.libs")
  pkgs = c(pane="EconCurves",plotpane="EconCurves",panequiz="EconCurves",award="RTutor2")
  if (isTRUE(opts$use.clicker)) {
    pkgs = c(pkgs, c(quiz="courserClicker"))
  } else {
    pkgs = c(pkgs, c(quiz="RTutor2"))
  }
  pkgs
}

add.block.default.libs.to.opts = function(types, opts) {
  restore.point("add.block.default.libs.to.opts")

  pkg = armd.block.type.default.libs(opts=opts)
  # remove default packaes from unused types
  pkg = pkg[names(pkg) %in% types]
  # remove default packages from types with manually specified packages
  pkg = pkg[!names(pkg) %in% names(opts$block.lib)]

  opts$block.libs = c(unlist(opts$block.libs),pkg)
  opts
}

