examples.parse.am = function() {
  setwd("D:/libraries/armd/examples")
  am = parse.armd(file="test.Rmd")
  preview.armd(am)
}

preview.armd = function(am=NULL, am.file=NULL,rmd.file=NULL,...) {
  fetch.am(am, am.file, rmd.file)
  ui = armd.ui(am = am)
  view.html(ui=ui,...)
}


#' Generate a problem set from a solution file
#'
#' Generates  .am file, and .rmd files for empty am , sample solution, and output solution
#'
#' @param txt the content of the sol.rmd file as text lines
#' @param file filename of the _sol.rmd file that specifies the problem set
#' @param name the name of the problem set
#' @param dir the directory in which all files are found and wil be saved to
#' @param extra.code.file the name of an r file that contains own functions that will be accessible in the problme set
#' @param var.txt.file name of the file that contains variable descriptions (see thee vignette for an explanation of the file format)
#' @param am.has.sol shall the sample solution be stored in the .am file. Set this option to FALSE if you use problem sets in courses and don't want to assess students the sample solution easily
#' @param use.memoise shall functions like read.csv be memoised? Data sets then only have to be loaded once. This can make problem sets run faster. Debugging may be more complicated, however.
#' @export
parse.armd = function(txt=read.as.utf8(file),file = NULL,name = NULL, am.id= NULL, bdf.filter = NULL,dir=getwd(), figure.dir=paste0(dir,"/",figure.sub.dir), figure.sub.dir = "figure", cache.dir = file.path(dir,"cache"), plugins=c("stats","export","dataexplorer"),catch.errors=TRUE, priority.opts=list(), figure.web.dir = "figure", filter.line=NULL, filter.type="auto", show.line=NULL, start.slide = NULL, source.file="main", libs=NULL, check.old.armd.sol=TRUE, extra.code.file=NULL, use.memoise = NA, refreshable.content.ui = FALSE, offline=FALSE,...) {
  restore.point("parse.armd")

  am = new.env()


  am$offline = offline
  am$version = 0.1
  am$refreshable.content.ui = refreshable.content.ui
  am$figure.web.dir = figure.web.dir
  am$figure.sub.dir = figure.sub.dir
  if (!dir.exists(figure.dir))
    dir.create(figure.dir)
  if (!dir.exists(cache.dir))
    dir.create(cache.dir)



  am$Addons = list()
  am$dir = dir
  am$figure.dir = figure.dir
  am$cache.dir = cache.dir
  am$plugins = plugins
  am$css = am$head = NULL

  am$header.tags = am$dependencies = list()

  if (length(txt)==1)
    txt = sep.lines(txt)

  cat("\n\nParse academic rmarkdown", name, "...")


  #Encoding(txt) = "UTF8"
  txt = mark_utf8(txt)


  adapt.ignore.include(am=am,txt=txt, source.file=source.file)
  txt = am$txt

  # add outer container
  txt = c("#. armd",txt)
  am$text.info = rbind(data_frame(line=NA,source=NA),am$text.info)

  txt = fast.name.rmd.chunks(txt)

  dot.levels = armd.dot.levels()
  df = find.rmd.nested(txt, dot.levels)


  # first load settings in opts
  # settings in rmd file overwrite opts
  bis = which(df$type == "settings")
  opts = list(offline = am$offline)
  if (!is.na(use.memoise)) opts$use.memoise = use.memoise
  for (bi in bis) {
    yaml = paste0(txt[(df$start[bi]+1):(df$end[bi]-1)], collapse="\n")
    so = read.yaml(text=yaml, keep.quotes=FALSE)
    opts[names(so)] = so
  }

  name = first.non.null(name, opts[["name"]])
  if (is.null(name) & !is.null(file)) {
    short.file = basename(file)
    name = tools::file_path_sans_ext(short.file)
  }

  if (!is.null(name)) opts$name = name
  if (is.null(opts[["name"]])) {
    if (!is.null(file)) {
      opts$name = tools::file_path_sans_ext(basename(file))
    }
  }
  if (!is.null(am.id)) opts$id = am.id
  if (is.null(opts[["id"]])) opts$id = opts$name

  opts$title = first.non.null(opts$title, opts$name)

  # special treatment for rtutor
  if (isTRUE(opts$rtutor)) {
    library(RTutor2)
    opts = do.call(default.ps.opts, opts)
    am$css = paste0(readLines(system.file("defaults/default.css",package="RTutor2"),warn = FALSE), collapse="\n")

  } else {
    am$css = paste0(readLines(system.file("defaults/default.css",package="armd"), warn=FALSE), collapse="\n")
  }

  # now install default opts given the settings
  opts = do.call(default.armd.opts, opts)

  am$name = opts$name
  #am$am.name = opts$name
  am$am.id = opts$id




  df = find.rmd.nested(txt, dot.levels)

  opts[names(priority.opts)] = priority.opts

  opts = add.block.default.libs.to.opts(df$type, opts)

  # priority opts overwrite settings
  opts[names(priority.opts)] = priority.opts
  set.armd.opts(opts)
  am$opts = opts



  make.am.block.types.df(am, opts)

  # remove blocks inside blocks that shall be removed
  remove.inner.blocks = get.bt(df$type,am)$remove.inner.blocks
  remove.inner.blocks[is.na(remove.inner.blocks)] = FALSE
  lev.par = get.levels.parents(df$level,remove.inner.blocks)
  del.rows = which(lev.par>0)
  df = del.rows.and.adapt.refs(df,del.rows,ref.cols = "parent")

  # check for undefined block types
  undefined = which(is.na(match(df$type,am$bt.df$type)))
  if (length(undefined)>0) {
    msg = paste0("Your Rmd file contains undefined blocks: ", paste0(unique(df$type[undefined]), collapse=", "),"\n\n")
    sources = sapply(undefined, function(bi) {
      msg = merge.lines(block.source.msg(bi,am=am,bdf=df))
    })
    msg = paste0(msg, "\n", paste0("unkown block '", df$type[undefined], "' in ", sources,collapse="\n"))
    stop(msg,.call=NULL)
  }



  am$slides = opts$slides
  am$slide.type = opts$slide.type
  if (am$slide.type=="auto")
    am$slide.type = find.bdf.auto.slide.type(df)

  if (am$slides) {
    am$hidden.container.types = am$slide.type
  } else {
    am$hidden.container.types = opts$menu.levels
  }

  am$txt = txt

  df = adapt.for.back.to.blocks(df,am=am)
  df$stype = df$type

  parent.types = am$bt.df$type[am$bt.df$is.parent]
  pt = get.levels.parents.by.types(df$level, df$type, parent.types)
  bdf = cbind(data.frame(index = 1:NROW(df)),df,pt) %>% as_data_frame
  bdf$obj = bdf$ui = bdf$inner.ui = vector("list", NROW(bdf))
  bdf = mutate(bdf,
    stype.ind = 0,
    id = paste0(type,"__",index,"__",am.id),
    name = NA_character_,
    container.ind = 0,
    div.id = "",
    output.id=  "",
    rmd = vector("list", NROW(bdf))
  )
  bdf = cbind(bdf, select(get.bt(bdf$type,am),-type,-remove.inner.blocks))

  bdf$parse.inner.block[is.na(bdf$parse.inner.block)] = TRUE
  lev.par = get.levels.parents(bdf$level,!bdf$parse.inner.block)
  bdf$parse.block = lev.par == 0

  am$bdf = bdf
  bdf = make.section.numbering(am=am)

  # Filter bdf if only a subset of elements shall be compiled / shown
  if (!is.null(filter.line)) {
    # filter problem set
    line = source.line.to.line(filter.line,am = am,source = 1)
    bdf = bdf.part.filter(line=line)(bdf=bdf)
    bdf = shorten.bdf.index(bdf)
    lines = c(unlist(lapply(2:NROW(bdf), function(row) bdf$start[row]:bdf$end[row])))
    am$txt[-lines] = ""
  } else if (!is.null(start.slide) & isTRUE(am$slides)) {
    am$start.slide = start.slide

  } else if (!is.null(show.line) & isTRUE(am$slides)) {
    # set start slide to current source line
    bi = source.line.to.bi(line = show.line,source = 1,bdf=bdf,am=am,type = am$slide.type)
    am$start.slide = sum(seq_len(NROW(bdf))<=bi & bdf$stype == am$slide.type)
  }

  am$bdf = bdf

  # Load libs and create envs
  am$opts$libs = unique(c(am$opts$libs,libs))
  # on build time we always need all libs
  load.packages(am$opts$libs, need.all.libs=TRUE)

  am$init.env = new.env(parent=parent.env(globalenv()))

  # overwrite some R functions in offline mode
  # currently we overwrite ggplotly and config
  if (am$offline) {
    copy.into.env(dest=am$init.env, source = armd.offline.funs(am))
  }

  am$pre.env = new.env(parent=am$init.env)

  if (isTRUE(am$opts$use.memoise)) {
    am$memoise.fun.li = memoise.fun.li(am$opts$memoise.funs)
    copy.into.env(dest=am$init.env, source=am$memoise.fun.li)
  }

  source.extra.code.file(extra.code.file = extra.code.file, am)

  am$opts$has.widgets = am$has.widgets = any(bdf$is.widget)
  am$rtutor = am$opts$rtutor
  if (am$opts$has.widgets | am$opts$rtutor) {
    library(RTutor2)
    RTutor2::rtutor.init.widgets(am)
  }

  # set knitr output for data frames
  do.call(set.knit.print.opts, c(list(output="html"),am$opts$knit.print.param))

  # Additional information for slides
  if (am$slides) {
    restore.point("am.slides.later")
    am$num.slides = sum(am$bdf$type==am$slide.type)
    if (am$num.slides==0) {
      stop(paste0("I cannot find a block with your slide type", am$slide.type))
    }
    am$slide.bis = which(am$bdf$type==am$slide.type)

  }


  # Preparse blocks from outer to inner,
  # i.e. ordered by start
  binds = order(bdf$start)
  bi = binds[1]
  for (bi in binds) {
    if (catch.errors) {
      res = try(armd.preparse.block(bi,am), silent=TRUE)
      if (is(res,"try-error")) {
        br = am$bdf[bi,]
        source = block.source.msg(bi=bi,am=am)
        msg = paste0("Error when preparsing ",br$type," block. Source: \n",source, "\n\n", paste0(as.character(res), collape="\n"))
        stop(msg)
      }
    } else {
      res = armd.preparse.block(bi,am)
    }
  }



  # Go through blocks and chunks, ordered by end
  binds = order(bdf$end, -bdf$start)
  bi = binds[1]
  for (bi in binds) {
    restore.point("inner.make.am")
    if (catch.errors) {
      #res = armd.parse.block(bi,am)
      res = try(armd.parse.block(bi,am), silent=TRUE)
      if (is(res,"try-error")) {
        br = am$bdf[bi,]
        source = block.source.msg(bi=bi,am=am)
        msg = paste0("Error when parsing ",br$type," block. Source: \n",source, "\n\n", paste0(as.character(res), collape="\n"))
        stop(msg)
      }
    } else {
      res = armd.parse.block(bi,am)
    }

    # relevant if only some frames are shown
    # and for blocks that are computed due
    # to their side effects but shall not be shown
    if (has.col(am$bdf, "dont.show")) {
      if (am$bdf$dont.show[[bi]]) {
        am$bdf$ui[[bi]] = tags$div(style="display: none;",am$bdf$ui[[bi]])
      }
    }
  }

  am$bdf$stype.ind = compute.value.index(am$bdf$stype)

  # specify containers
  am$bdf$container.ind = cumsum(am$bdf$is.container) * am$bdf$is.container

  am$bdf$parent_container = get.levels.parents(am$bdf$level, am$bdf$is.container)

  am$navbar.ui = armd.navbar(am=am, nav.levels = opts$nav.levels)



  make.am.rmd(am=am)

  # tests
  ids = am$bdf$id
  if (any(duplicated(ids))) {
    stop(paste0("The bdf id ", paste0(ids[duplicated(ids)], collapse="\n"), " are duplicated. Check your addons, chunk id, or block id."))
  }
  #write.am(am=am,dir=dir)

  # copy into global env for convenience
  copy.into.env(am$pre.env,dest = .GlobalEnv)

  am
}

source.line.to.line = function(line, am, source=1) {
  restore.point("source.line.to.line")
  lines = (which(am$txt.lines <= line & am$txt.source == source))
  if (length(lines)==0) return(1)
  return(max(lines))
}

source.line.to.bi = function(line,bdf=am$bdf,am, source=1,type=NULL) {
  restore.point("source.line.to.bi")
  line = source.line.to.line(line,am,source)

  if (is.null(type)) {
    bi = max(which(bdf$start <= line))
  } else {
    bis = which(bdf$start <= line & bdf$type==type)
    if (length(bis)==0) {
      bi = min(which(bdf$type==type))
    } else {
      bi = max(bis)
    }
  }
  bi
}

block.source.msg = function(bi, am, bdf = am$bdf) {
  restore.point("block.source.msg")

  br = bdf[bi,]
  lines = br$start:br$end
  df = data_frame(line=am$txt.lines[lines],source=am$txt.source[lines])
  sdf = summarise(group_by(df, source), start=min(line),end=max(line))
  sdf$file = am$source.files[sdf$source]

  paste0(sdf$file, " lines ",sdf$start, " to ", sdf$end, collapse="\n")

}



adapt.ignore.include = function(am, txt=am$txt, source.file="main") {
  restore.point("adapt.ignore.include")

    # Only capture elements between the lines <!-- START --> and <!-- END -->
  res = rmd.between.start.end.lines(txt,return.start.end = TRUE)
  txt = res$txt
  am$txt.lines = seq_along(txt)+res$start-1
  am$txt.source = rep(1, length(am$txt.lines))
  am$source.files = source.file

  changed.ig = changed.in = TRUE
  counter = 0
  am$txt = txt
  while(changed.ig | changed.in) {
    counter = counter+1
    changed.ig = adapt.ignore(am, keep.ignore.if.included=counter==1)
    changed.in = adapt.include(am)
    if (counter > 300) {
      stop("#. include statements were nested deeper than 300 levels. Probably there is a recursion...")
    }
  }
  invisible()
}

adapt.ignore = function(am,txt=am$txt, keep.ignore.if.included=TRUE) {
  restore.point("adapt.ignore")

  # all rows that will be deleted
  # in this precompilation state
  del.lines = NULL
  df = find.rmd.blocks(txt)

  # remove content in ignore blocks
  ig.rows = which(df$type=="ignore")
  if (length(ig.rows)>0) {
    del.lines = c(del.lines,unlist(lapply(ig.rows, function(ig.row) df$start[ig.row]:df$end[ig.row])))
  }

  # deal with ignore_if_included blocks
  ig.rows = which(df$type=="ignore_if_included")
  if (length(ig.rows)>0) {
      # only remove block.start and end
    if (keep.ignore.if.included) {
      del.lines = c(del.lines,unlist(lapply(ig.rows, function(ig.row) c(df$start[ig.row],df$end[ig.row]))))
    } else {
      # remove whole block
      del.lines = c(del.lines,unlist(lapply(ig.rows, function(ig.row) df$start[ig.row]:df$end[ig.row])))
    }
  }


  if (length(del.lines)==0) return(FALSE)
  del.lines =unique(del.lines)
  txt = txt[-del.lines]
  am$txt.lines = am$txt.lines[-del.lines]
  am$txt.source = am$txt.source[-del.lines]

  am$txt = txt
  return(TRUE)
}

adapt.include = function(am,txt=am$txt) {
  restore.point("adapt.include")
  lines = which(str.starts.with(txt,"#. include "))
  if (length(lines)==0) return(FALSE)

  files = str.trim(str.right.of(txt[lines],"#. include "))

  i = 1
  for (i in seq_along(lines)) {
    file = files[i]
    source = match(file, am$source.files)
    if (is.na(source)) {
      am$source.files = c(am$source.files, file)
      source = length(am$source.files)
    }
    line = lines[i]
    ntxt = readLines(paste0(am$dir,"/",file),warn=FALSE,encoding = "UTF8")
    ntxt = mark_utf8(ntxt)
    am$txt = insert.into.vec(txt,ntxt,pos=line, replace=TRUE)
    am$txt.lines = insert.into.vec(am$txt.lines,seq_along(ntxt),pos=line, replace=TRUE)

    am$txt.source = insert.into.vec(am$txt.source,rep(source,length(ntxt)),pos=line, replace=TRUE)
    lines = lines+length(ntxt)-1
    txt = am$txt
  }
  return(TRUE)
}


adapt.for.back.to.blocks = function(bdf,am, line.start=am$txt.start) {
  restore.point("adapt.for.back.to.blocks")

  is_backto = str.starts.with(bdf$type,"back_to_")
  bttype = str.right.of(bdf$type[is_backto],"back_to_")
  rows = which(is_backto)
  if (length(rows)==0) return(bdf)
  i = 1
  for (i in seq_along(rows)) {
    row = rows[i]
    parent = bdf$parent[row]
    do.stop = parent == 0
    if (!do.stop) do.stop = (bdf$type[parent] != bttype[i])
    if (do.stop) {
      msg = paste0("In line ", bdf$start[row]+line.start-1, " you have a ", bdf$type[row], " statement without a ", bttype[row]," parent.")
      stop(msg)
    }
    bdf$parent[bdf$parent==row] = parent
  }
  # remove from txt
  am$txt[bdf$start[rows]] = ""

  # adapt parent indexes for removed rows
  old.ind = 1:NROW(bdf)
  new.ind = c(0,old.ind - cumsum(is_backto))
  bdf$parent = new.ind[bdf$parent+1]

  bdf = bdf[-rows,]

  bdf
}



shorten.bdf.index = function(bdf, new = 1:NROW(bdf), old = bdf$index) {
  restore.point("shorten.bdf.index")

  long = rep(NA_integer_,max(old))
  long[old] = new
  bdf$index = new
  cols = c("parent",colnames(bdf)[str.starts.with(colnames(bdf),"parent_")])
  pmat = as.matrix(bdf[,cols])
  pvec = as.numeric(pmat)
  pvec[pvec==0] = NA_integer_
  #pvec[pvec==0] = 0
  nmat = matrix(long[as.numeric(pvec)],NROW(pmat),ncol(pmat))
  nmat[pmat==0] = 0
  nmat[is.na(nmat)] = 0

  bdf[,cols] = nmat
  rows = bdf$parent == 0 & bdf$index > 1
  bdf$parent[rows] = 1
  bdf$level[rows] = 2

  bdf
}

armd.preparse.block = function(bi,am, opts = am$opts) {
  restore.point("armd.preparse.block")
  type = am$bdf$type[[bi]]

  # important for setting task.lines
  if (isTRUE(am$rtutor) | isTRUE(am$has.widgets)){
    rtutor.preparse.block(bi, am, opts=opts)
  }

}


armd.parse.block = function(bi,am) {
  restore.point("armd.parse.block")

  bdf = am$bdf
  if (!bdf$parse.block[[bi]]) return()

  if (bdf$is.widget[[bi]]) {
    return(rtutor.parse.widget(bi,am))
  }

  type = bdf$type[[bi]]
  fun.name = paste0("armd.parse.",type)
  pkg =get.bt(type, am)$package
  if (is.na(pkg)) {
    stop("Unknown block type '", type,"'.")
  }

  fun.call = parse(text=paste0(pkg,"::armd.parse.",type,"(bi,am)"))
  res = eval(fun.call)
  res
  #fun(bi,am)
}

armd.parse.settings = function(bi, am,...) {

}

armd.parse.css = function(bi, am,...) {
  css = am$txt[(am$bdf$start[bi]+1):(am$bdf$end[bi]-1)]
  am$css = paste0(c(am[["css"]],css),collapse="\n")
}


armd.parse.head = function(bi, am,...) {
  head = am$txt[(am$bdf$start[bi]+1):(am$bdf$end[bi]-1)]
  am$head = paste0(c(am[["head"]],head),collapse="\n")
}


armd.parse.layout = function(bi, am,...) {
  restore.point("armd.parse.layout")
  br = am$bdf[bi,]
  name = br$arg.str
  am$bdf$name[bi] = name
  inner = am$txt[(am$bdf$start[bi]+1):(am$bdf$end[bi]-1)]
  li = parse.hashdot.yaml(inner, hashdot="##. ")
  li$name = name
  am$bdf$obj[[bi]] = li
  if (is.null(am[["layouts"]]))
    am$layouts = list()

  am$layouts[[name]] = li
}


armd.parse.chunk = function(bi,am, opts=armd.opts()) {
  restore.point("armd.parse.chunk")
  bdf = am$bdf; br = bdf[bi,]; str = am$txt[br$start:br$end]
  args = parse.chunk.args(header = str[1])

  if (!is.null(args$label))
    am$bdf$name[bi] = args$label

  code = str[-c(1,length(str))]
  mstr = merge.lines(str)
  armd.set.rmd(bi, list(rmd=mstr))
  args$comment = NA

  chunk.preknit = isTRUE(args$preknit) | isTRUE(br$parent_info) | isTRUE(br$parent_preknit) | isTRUE(opts$chunk.preknit)

  chunk.precompute = (br$parent_precompute >0 | isTRUE(args$precompute)) & !chunk.preknit

  if (chunk.precompute) {
    # precompute does not show chunk
    restore.point("parse.precompute.chunk")
    am$bdf$stype[[bi]] = "precompute_chunk"
    expr = parse(text=code)
    res = eval(expr,am$pre.env)
  } else {
    # knitted chunk
    rmd = code.to.rmd.chunk(code,args=args)
    ui = knit.chunk(rmd,envir = am$pre.env,out.type="shiny", deps.action="ignore", use.commonmark = TRUE, add.meta.attr=TRUE)

    meta = attr(ui,"knit_meta")
    armd.add.meta(am=am, meta=meta)

    ui = tagList(ui, highlight.code.script())
    set.bdf.ui(ui, bi,am)
  }


}

code.to.rmd.chunk = function(code, args, label=args$label) {
  restore.point("code.to.rmd.chunk")

  if (is.null(label)) {
    label = "chunk"
  }
  args = args[setdiff(names(args),"label")]
  args = lapply(args, function(arg) {
    if (is.character(arg)) return(paste0("'",arg,"'"))
    arg
  })
  if (length(args)>0) {
    head = paste0('```{r "',label, '", ',paste0(names(args)," = ", args, collapse=", "),'}')
  } else {
    head = paste0('```{r "',label, '" }')
  }
  c(head,code,"```")
}

armd.parse.preknit = function(bi,am) {
  restore.point("armd.parse.preknit")
  bdf = am$bdf; br = bdf[bi,];

  # new code: children are knitted
  children = bdf$parent == bi
  res = get.children.and.fragments.ui.list(bi,am, keep.null=FALSE, children=children)
  ui.li = res$ui.li
  set.bdf.ui(ui.li,bi,am)
  armd.set.rmd(bi=bi, am=am, rmd=res$rmd, newline=FALSE)
  return()

}

armd.parse.precompute = function(bi,am) {
  restore.point("armd.parse.precompute")
  # all work is done in the chunks inside
  armd.parse.as.container(bi,am)
}

armd.parse.define = function(bi,am) {
  restore.point("armd.parse.define")
  # all work is done in the chunks inside
  armd.parse.as.container(bi,am,is.hidden = TRUE)
}



armd.parse.gv = function(bi, am) {
  restore.point("armd.parse.gv")
  arg.str= am$bdf$arg.str[[bi]]

  bdf = am$bdf; br = bdf[bi,];
  txt = get.bi.inner.txt(bi=bi, am=am)
  library(svgdiagram)
  svg = gv.to.svg(gv=txt, to.clipboard = FALSE)
  ui = HTML(svg)
  set.bdf.ui(ui,bi,am)
}

# pure html block, not markdown
armd.parse.html = function(bi, am) {
  restore.point("armd.parse.tab")

  txt = get.bi.inner.txt(bi,am=am)
  args = get.bi.args(bi=bi, am=am)
  ui = HTML(txt)
  set.bdf.ui(ui=ui, bi=bi,am=am)

}

armd.parse.tab = function(bi, am) {
  restore.point("armd.parse.tab")

  txt = get.bi.inner.txt(bi,am=am)
  args = get.bi.args(bi=bi, am=am)

  sep = first.non.null(args[["sep"]],am$opts[["tab.sep"]],",")
  bg = first.non.null(args[["bg"]],am$opts[["tab.bg"]],"#fff")
  line.end = first.non.null(args[["\\"]],am$opts[["tab.line.end"]],"\\")
  class = first.non.null(args[["class"]],"")


  if (nchar(line.end)>0) {
    rows = str.ends.with(txt, line.end)
    txt[rows] = substring(txt[rows],1,nchar(txt[rows])-nchar(line.end))
  }

  df = read.csv(textConnection(txt),check.names=FALSE,sep=sep)

  ui = div(style="margin: auto",HTML(html.table(df,bg.color = bg, table.style="margin: auto;", table.class=class)))
  set.bdf.ui(ui=ui, bi=bi,am=am)
}

armd.parse.figure = function(bi,am) {
  restore.point("armd.parse.figure")
  bdf = am$bdf; br = bdf[bi,];
  args = list()
  txt = get.bi.inner.txt(bi,am=am)
  layout = get.am.layout("figure", am=am, load.defaults=TRUE)
  args$layout.txt = txt.to.layout(txt, layout)
  armd.parse.as.container(bi=bi,am=am,args = args)

}


armd.parse.portrait = function(bi,am) {
  restore.point("armd.parse.image")
  bdf = am$bdf; br = bdf[bi,];

  args = get.yaml.block.args(bi,am)
  if (is.null(args$height)) args$height = "auto"

  if (is.null(args$width)) args$width = "70px"
  if (is.null(args$height)) args$height = "auto"
  if (is.null(args$align)) args$align = "left"

  if (!is.null(args$file)) {
    file.path = paste0(am$figure.dir,"/",args$file)
    has.file = file.exists(file.path)
  } else {
    has.file = FALSE
  }
  wh = NULL
  if (!is.null(args$width)) wh = paste0(wh," width ='",args$width,"'")
  if (!is.null(args$height)) wh = paste0(wh," height ='",args$height,"'")

   if (has.file) {
    html = paste0('<img src=figure/',args$file,wh,'>')
  } else if (!is.null(args$url)) {
    html = paste0('<img src=',args$url,wh,'>')
  } else {
    html('<p>...figure here...</p>')
  }
  if (!is.null(args$link)) {
    html = paste0("<a href='",args$link,"' target='_blank'>",html,"</a>")
  }

  if (is.null(args$name)) {
    name.html = ""
  } else {
    args$name = gsub("\n","<br>",args$name, fixed=TRUE)
    name.html = paste0("<tr><td style='padding-bottom: 2px;padding-left: 5px;padding-right: 5px;text-align: center;white-space: normal;word-wrap: break-all;'><font size=1>",args$name,"</font></td></tr>")
  }
  tab = paste0("<table  align='",args$align,"'><tr><td STYLE='padding-left:5px;padding-right:5px;'>",html,'</td></tr>',name.html,"</table>")
  set.bdf.ui(HTML(tab),bi,am)
}

armd.parse.image = function(bi,am, download.image=TRUE) {
  restore.point("armd.parse.image")
  bdf = am$bdf; br = bdf[bi,];

  args = get.yaml.block.args(bi,am)
  if (is.null(args$height)) args$height = "auto"

  if (!is.null(args$file)) {
    file.path = paste0(am$figure.dir,"/",args$file)
    has.file = file.exists(file.path)
  } else {
    has.file = FALSE
  }
  wh = NULL
  #if (!is.null(args$width)) wh = paste0(wh," width ='",args$width,"'")
  #if (!is.null(args$height)) wh = paste0(wh," height ='",args$height,"'")

  style = paste0(names(args),": ",args,collapse="; ")

  if (has.file) {
    html = paste0('<img style="',style,'" src=figure/',args$file,wh,' >')
  } else if (!is.null(args$url)) {
    html = paste0('<img style="',style,'"  src=',args$url,wh,'>')
  } else {
    html = '<p>...figure here...</p>'
  }
  set.bdf.ui(HTML(html),bi,am)
}


armd.parse.solved = function(bi,am) {
  restore.point("armd.parse.solved")
  armd.parse.as.container(bi,am)
}

armd.parse.armd = function(bi,am) {
  restore.point("armd.parse.am")
  armd.parse.as.container(bi,am, only.children.ui = TRUE)
}



armd.parse.exercise = function(bi,am) {
  restore.point("armd.parse.exercise")
  armd.parse.as.section(bi,am,type="exercise", rmd.prefix="## Exercise")
}

armd.parse.chapter = function(bi,am) {
  restore.point("armd.parse.chapter")
  armd.parse.as.section(bi,am,type="chapter", rmd.prefix="# Chapter", title.fun=h1, title.display = "block")

}


armd.parse.section = function(bi,am) {
  restore.point("armd.parse.section")

  armd.parse.as.section(bi,am,type="section", rmd.prefix="## Section", title.fun=h2, title.display = "block")
}

armd.parse.subsection = function(bi,am) {
  restore.point("armd.parse.subsection")
  armd.parse.as.section(bi,am,type="subsection", rmd.prefix="### Subsection",title.fun=h3, title.display = "block")
}


armd.parse.subsubsection = function(bi,am) {
  restore.point("armd.parse.subsection")


  armd.parse.as.section(bi,am,type="subsubsection", rmd.prefix="####  Subsubsection",title.fun=h4, title.display = "inline")

}

armd.parse.frame = function(bi,am) {
  restore.point("armd.parse.frame")

  armd.parse.as.section(bi,am,type="frame", rmd.prefix="### Frame",title.fun=h4, title.display = "block")
}

armd.parse.as.section = function(bi, am, type="section", rmd.prefix="# Section", title.fun=h4, title.display="inline", title.prefix = if (type %in% am$opts$auto.number) am$bdf$part.prefix[[bi]]) {
  restore.point("armd.parse.as.section")
  bdf = am$bdf; br = bdf[bi,];
  arg.str= am$bdf$arg.str[[bi]]

  args = parse.block.args(arg.str =arg.str, allow.unquoted.title = TRUE)

  # extract layout in [ ]
  if (str.starts.with(arg.str,"[")) {
    args$layout.name = str.between(args$name,"[","]")
    args$layout = am$layouts[[args$layout.name]]
    if (is.null(args[["layout"]])) {
      cat("\nWarning could not find layout", args$layout.name)
    } else {
      inner = get.bi.inner.txt(bi,am=am)
      args$layout.txt = sep.lines(txt.to.layout(txt=inner,layout=args$layout))
    }
    args$name = str.trim(str.right.of(args$name,']'))
  }



  title = first.non.null(args$title, args$name)
  title  = paste0(title.prefix," ", title)

  if (isTRUE(am$opts$verbose.compile)) {
    num = sum(am$bdf$type[1:bi] == type)
    cat(paste0("\nparse ",type, " ", num,": ",title))
  }


  type = am$bdf$stype[[bi]]
  #if (isTRUE(type %in% am$opts$hide_title)) title = NULL

  armd.parse.as.container(bi,am,args = args, rmd.prefix=paste0(rmd.prefix," ",title,"\n"), title = title, anchor=paste0("part",bi), type=type, title.fun=title.fun, title.display=title.display)
  if (is.null(args$title.offset)) args$title.offset=0
  button.label = str.left.of(args$name," --")
  am$bdf$obj[[bi]] = list(title = args$name,button.label = button.label, args=args)

}


armd.parse.as.container = function(bi, am,args=NULL, inner.ui = NULL, rmd=NULL, highlight.code = is.widget, is.widget=get.bt(type,am)$is.widget, rmd.head=NULL, rmd.prefix="", rmd.postfix="", ui.fun=NULL, title = am$bdf$obj[[bi]]$title, is.hidden = am$bdf$type[[bi]] %in% am$hidden.container.types, extra.class = "", only.children.ui = FALSE, anchor=NULL, type=am$bdf$type[[bi]], title.fun=h4, title.display="block", title.hide=isTRUE(type %in% am$opts$hide_title)) {
  restore.point("armd.parse.as.container")
  bdf = am$bdf; br = bdf[bi,];
  if (is.null(inner.ui) | is.null(rmd)) {
    if (only.children.ui) {
      res = get.children.ui.list(bi,am,keep.null=TRUE, layout.txt = args$layout.txt)
    } else {
      res = get.children.and.fragments.ui.list(bi,am,keep.null=TRUE, layout.txt = args$layout.txt)
    }
    if (is.null(inner.ui)) inner.ui = res$ui.li
    if (is.null(rmd)) rmd = res$rmd
  }
  if (!is.null(ui.fun)) {
    inner.ui = ui.fun(inner.ui)
  }
  if (!is.null(inner.ui)) {
    inner.ui = tagList(
      inner.ui,
      if (highlight.code) highlight.code.script() else NULL
    )
  }

  header = NULL
  # Add slide header
  if (am$slides & identical(am$slide.type,type)) {
    slide.ind = sum(am$bdf$type[1:bi]==am$slide.type)
    header = slide.title.bar.ui(title = title,slide.ind=slide.ind,num.slides = am$num.slides,am=am, bi=bi, args=args)

  # Add title as header
  } else {
    if (!is.null(title))
      header = container.title.html(title = title, anchor=anchor, type=type, title.fun=title.fun, display=title.display, hide=title.hide)
  }
  if (!is.null(header)) {
    inner.ui = list(header, inner.ui)
  }

  armd.set.rmd(bi=bi, am=am, rmd=rmd, rmd.prefix=rmd.prefix, rmd.postfix=rmd.postfix)

  # A widget will be loaded in an uiOutput
  if (is.widget) {
    am$bdf$inner.ui[[bi]] = inner.ui
    set.container.div.and.output(bi,am, is.hidden=is.hidden)
  # A static container will not be loaded in a uiOutput
  } else {
    restore.point("jnxjkfhiufhriuhf")

    style = ""
    if (is.hidden) style = "display: none;"

    am$bdf$div.id[[bi]] = div.id = paste0(am$prefix, br$id,"_div")

    div.class = paste0("armd-static-container-div ",type,"-container-div")
    if (isTRUE(am$slides)) {
      if (type == am$slide.type) {
        div.class = paste0(div.class," slide-container-div")
      }
    }

    am$bdf$ui[[bi]] = div(id=div.id,class=div.class,  style=style,
      inner.ui
    )
  }
  am$bdf$is.container[[bi]] = TRUE
}

container.title.html = function(title,type=NULL, am=NULL, class=paste0("container-title ",type,"-title", " container-",display,"-title"), anchor=NULL, title.fun=h4, hide = isTRUE(type %in% am$opts$hide_title), display="inline") {
  if (is.null(title)) return(NULL)
  if (title=="") return(NULL)
  res = title.fun(title)
  if (!is.null(anchor)) {
    res = a(name=anchor, class="title-anchor", res)
  }
  if (hide) {
    style = "display: none"
  } else {
    style = paste0("display: ", display)
  }
  res = span(class=class, style=style, res)
  res
}

set.container.div.and.output = function(bi, am, is.hidden = am$bdf$type[bi] %in% am$hidden.container.types) {
  bdf = am$bdf; br = bdf[bi,];

  style = ""
  if (is.hidden) style = "display: none;"


  am$bdf$div.id[[bi]] = div.id = paste0(am$prefix, br$id,"_div")
  am$bdf$output.id[[bi]] = output.id = paste0(am$prefix, br$id,"_output")
  type = br$stype
  div.class = paste0("armd-container-div ",type,"-container-div")
  if (isTRUE(am$slide)) {
    if (type == am$slide.type) {
      div.class = paste0(div.class," slide-container-div")
    }
  }
  am$bdf$ui[[bi]] = div(id=div.id,class=div.class, style=style,
    uiOutput(output.id)
  )

}

get.container.default.rmd = function(bi,am) {
  title = paste0("## Frame ", args$name)

}

parse.container.inner.ui.and.rmd = function(bi, am) {
  restore.point("armd.parse.frame")
  #stop()
  bdf = am$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)

  children = bdf$parent == bi
  res = get.children.and.fragments.ui.list(bi,am, children=children, keep.null=TRUE)

  ui.li = res$ui.li
  is.child = !res$is.frag

  title = args$name

  armd.set.rmd(bi=bi, am=am, rmd=rmd, rmd.prefix = title)

  if (is.null(args$title.offset)) args$title.offset=0
  if (!is.null(args$name)) {
    title = fluidRow(column(offset=args$title.offset,width=12-args$title.offset,h4(args$name)))
  } else {
    title = NULL
  }
  ui = tagList(
    ui.li,
    highlight.code.script()
  )
  am$bdf$obj[[bi]] = list(title = args$name, args=args)
  set.bdf.ui(ui,bi,am)

}


armd.parse.info = function(bi,am) {
  restore.point("armd.parse.info")
  armd.parse.as.collapse(bi,am,title.prefix="Info")
}


armd.parse.box = function(bi,am) {
  restore.point("armd.parse.box")
  armd.parse.as.collapse(bi,am, open=TRUE)
}


armd.parse.note = function(bi,am) {
  restore.point("armd.parse.note")
  armd.parse.as.collapse(bi,am)
}


armd.parse.spoilerNote = function(bi,am) {
  restore.point("armd.parse.spoiler")
  armd.parse.as.collapse(bi,am, content.wrapper=function(...) div(class="spoilerNote",...))
}

armd.parse.references = function(bi,am) {
  restore.point("references.block.render")
  title = "References"
  if (isTRUE(am$opts$lang=="de")) {
    title = "Referenzen"
  }
  armd.parse.as.collapse(bi,am, title=title)
}

armd.parse.as.collapse  =  function(bi,am,title.prefix=NULL, title=args$name, rmd.head=paste0("### ", title), rmd.foot="---",args=get.bi.args(bi=bi,am=am,allow.unquoted.title=TRUE),content.wrapper=NULL, open=FALSE,...) {
  restore.point("armd.parse.as.collapse")
  #stop()
  bdf = am$bdf; br = bdf[bi,];
  children = bdf$parent == bi
  res = get.children.and.fragments.ui.list(bi,am, children=children, keep.null=TRUE)

  ui.li = res$ui.li
  is.child = !res$is.frag

  if (is.null(title)) title = str.trim(paste0(title.prefix, " ",args$name))
  if (is.null(title)) title = bs$type[[bi]]
  if (!is.null(content.wrapper))
    ui.li = content.wrapper(ui.li)
  inner.ui = make.armd.collapse.note(id=paste0(am$bdf$type[[bi]],"_collapse_",bi),content=ui.li, title=title, open=open)

  rmd = lapply(res$rmd, function(txt) {
    merge.lines(c("",rmd.head,txt,rmd.foot))
  })
  armd.parse.as.container(bi,am,args=args, inner.ui=inner.ui, rmd=rmd,...)
}


get.bi.am.str = function(bi,am, remove.header.footer=TRUE) {
  restore.point("get.bi.am.str")

  start = (am$bdf$start[bi]+1)
  end = am$bdf$end[bi]-1*(am$bdf$form[[bi]]=="block")
  if (end<start) return(NULL)
  str = am$txt[start:end]
  str
}

make.armd.collapse.note = function(id, html, title="Note", content=NULL, open=FALSE) {
  if (is.null(content))
    content = HTML(paste0(html, collapse="\n"))

  #shinyBS::bsCollapse(id =id, shinyBS::bsCollapsePanel(title=title,content))
  slimCollapsePanel(title, content, open=open, padding="5px")
  #shinyBS::bsCollapse(id =id, shinyBS::bsCollapsePanel(title=title,content))

}

set.in.bdf = function(bi,am,...) {
  args = list(...)
  restore.point("set.in.bdf")

  cols = match(names(args),colnames(am$bdf))
  if (is(am$bdf,"data.table")) {
    for (j in seq_along(cols)) {
      set(am$bdf,bi,cols[j],args[j])
    }
  } else {
    for (j in seq_along(cols)) {
      am$bdf[bi,cols[j]]=args[[j]]
    }
  }
}


set.bdf.ui = function(ui,bi,am=get.am()) {
  am$bdf$ui[[bi]] = ui
  #am$bdf$has.ui[[bi]] = TRUE
}

fragment.to.html = function(txt, bi, am) {
  restore.point("fragment.to.html")

  if (isTRUE(am$opts$use.whiskers)) {
    .whiskers = am$pre.env[[".whiskers"]]
    if (length(.whiskers)>0)
      txt = replace.whiskers(paste0(txt,collapse="\n"),.whiskers)
  }

  #txt = unicode.html.math(txt)

  HTML(md2html(txt, fragment.only = TRUE))
}

get.children.ui.list = function(bi,am,bdf=am$bdf, keep.null=TRUE, empty.as.null=FALSE, children=am$bdf$parent == bi, layout.txt=NULL) {
  restore.point("get.children.ui.list")


  ui = lapply(which(children), function(ind) {
    bdf$ui[[ind]]
  })

  inds = which(children)

  rmd.li=bdf$rmd[children]

  rmd =armd.bind.rmd.li(rmd.li)

  if (!keep.null) {
    null.ui = sapply(ui, is.null)
    ui = ui[!is.null(ui)]
  }
  names(ui) = NULL
  list(ui.li=ui, rmd=rmd, is.frag=rep(FALSE,length(ui)))
}


get.children.and.fragments.ui.list = function(bi,am,bdf=am$bdf, keep.null=TRUE, empty.as.null=FALSE, children=am$bdf$parent == bi, layout.txt=NULL) {
  restore.point("get.children.and.fragments.ui.list")

  res = get.non.children.fragments(bi,am, child.ind = which(children), layout.txt=layout.txt)
  is.frag = res$is.frag
  is.child = !is.frag
  ui = res$frag
  rmd.li = lapply(res$frag, function(txt) {
    armd.expand.rmd.modes(txt,am=am)
  })

  ui[is.frag] = lapply(ui[is.frag], function(txt) {
    fragment.to.html(txt=txt, bi=bi, am=am)
  })

  ui[is.child] = lapply(which(children), function(ind) {
    bdf$ui[[ind]]
  })
  rmd.li[is.child] = bdf$rmd[children]

  if (!keep.null) {
    null.ui = sapply(ui, is.null)
    ui = ui[!is.null(ui)]
    is.frag = is.frag[null.ui]
  }
  names(ui) = NULL
  rmd = armd.bind.rmd.li(rmd.li)
  list(ui.li=ui, rmd=rmd, is.frag=is.frag)
}

get.child.and.fragment.txt.li = function(bi,am,bdf=am$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE) {
  restore.point("get.child.and.fragment.txt.li")

  cpos = cbind(bdf$start[child.ind],bdf$end[child.ind])
  has.footer = bdf$form[[bi]] != "dotblock"
  start = bdf$start[bi]+ (1-keep.header.footer)
  end = bdf$end[bi] - (1- (keep.header.footer | !has.footer))

  pos = pos.complement(cpos, is.sorted=TRUE, keep.pos=TRUE, start=start, end = end)
  is.frag = attr(pos,"complement")

  # we may have one end line too much
  valid = pos[,1]<=pos[,2]
  pos = pos[valid,,drop=FALSE]
  is.frag = is.frag[valid]

  txt.li = lapply(1:NROW(pos), function(row) {
    am$txt[pos[row,1]:pos[row,2]]
  })
  list(txt.li = txt.li, is.frag=is.frag)
}

get.non.children.fragments = function(bi,am,bdf=am$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE, layout.txt = NULL) {
  restore.point("get.non.children.fragments")

  if (!is.null(layout.txt)) {
    return(get.non.children.fragments.from.layout.txt(bi,am,bdf, child.ind, keep.header.footer, layout.txt))
  }
  cpos = cbind(bdf$start[child.ind],bdf$end[child.ind])
  has.footer = bdf$form[[bi]] != "dotblock"
  start = bdf$start[bi]+ (1-keep.header.footer)
  end = bdf$end[bi] - (1- (keep.header.footer | !has.footer))

  pos = pos.complement(cpos, is.sorted=TRUE, keep.pos=TRUE, start=start, end = end)
  is.frag = attr(pos,"complement")

  # we may have one end line too much
  valid = pos[,1]<=pos[,2]
  pos = pos[valid,,drop=FALSE]
  is.frag = is.frag[valid]

  if (NROW(pos)==0) return(NULL)

  frag.li = lapply(1:NROW(pos), function(row) {
    if (!is.frag[row]) return(NULL)
    merge.lines(am$txt[pos[row,1]:pos[row,2]])
  })
  list(frags = frag.li, is.frag=is.frag)
}

get.non.children.fragments.from.layout.txt = function(bi,am,bdf=am$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE, layout.txt = NULL) {
  restore.point("get.non.children.fragments.from.layout.txt")
  child.header = am$txt[bdf$start[child.ind]]

  child.start = match(child.header, layout.txt)
  child.len = bdf$end[child.ind]-bdf$start[child.ind]+1
  child.end = child.start + child.len -1

  cpos = cbind(child.start, child.end)
  pos = pos.complement(cpos, is.sorted=TRUE, keep.pos=TRUE, start=1, end = length(layout.txt))
  is.frag = attr(pos,"complement")

  # we may have one end line too much
  valid = pos[,1]<=pos[,2]
  pos = pos[valid,,drop=FALSE]
  is.frag = is.frag[valid]

  if (NROW(pos)==0) return(NULL)

  frag.li = lapply(1:NROW(pos), function(row) {
    if (!is.frag[row]) return(NULL)
    merge.lines(layout.txt[pos[row,1]:pos[row,2]])
  })
  list(frags = frag.li, is.frag=is.frag)
}

make.am.ui = function(am, bdf=am$bdf) {
  rows = which(bdf$parent == 0)
  ui.li = bdf$ui[rows]
}

default.navbar.link.fun = function(title, level, bi=NULL) {
  return(title)
  #paste0("<a role='button'>", title,"</a>")
}

default.navbar.li.fun = function(titles, child.li, levels=NULL,bis=NULL) {
  restore.point("default.navbar.li.fun")

  li = lapply(seq_along(titles), function(i) {
    child.ui = child.li[[i]]
    tabPanel(title = titles[i],value = paste0("armd_menu_tab_",bis[i]), child.ui)
  })
  ui = do.call(tabsetPanel,li)
  return(ui)
  #inner = paste0("<li>", titles, "\n", child.li, "</li>")
  #paste0("<ul>", paste0(inner, collapse="\n"),"</ul>")

}

del.rows.and.adapt.refs = function(df, del.rows, ref.cols=NULL) {
  restore.point("del.rows.and.adapt.refs")

  if (NROW(df)==0 | NROW(del.rows)==0) return(df)
  if (!is.logical(del.rows))
    del.rows = (1:NROW(df)) %in% del.rows

  cum.del = cumsum(del.rows)
  #rbind(row = 1:NROW(df), parent = df[[col]], del.rows, cum.del, pcum.del = cum.del[ df[[col]] ] )

  for (col in ref.cols) {
    ref = df[[col]]
    valid = ref %in% 1:NROW(df)
    ref = ref[valid]
    df[[col]][valid] = ref - cum.del[ ref ]
  }
  df = df[!del.rows,,drop=FALSE]
  rownames(df) = NULL
  df
}

get.bi.inner.txt = function(bi,txt = am$txt, am=get.am()) {
  restore.point("get.bi.inner.txt")
  bdf = am$bdf
  has.footer = bdf$form[[bi]] != "dotblock"
  lines = (bdf$start[bi]+1):(bdf$end[bi]-has.footer)
  am$txt[lines]
}

get.bi.args = function(bi, am=get.am(),allow.unquoted.title=FALSE,...) {
  parse.block.args(arg.str = am$bdf$arg.str[[bi]],allow.unquoted.title=allow.unquoted.title,...)
}

error.in.bi = function(err.msg, bi,line= paste0(am$bdf$start[bi])[1], just.start.line=TRUE, am=get.am()) {
  restore.point("error.in.bi")

  org.line = am$txt.lines[line]
  file = am$source.files[am$txt.source[line]]
  msg = paste0(err.msg,"\nSee line ", org.line, " in file ", file)
  stop(msg,call. = FALSE)
}


write.am = function(am, file.name=paste0(dir,"/",am$name,".am"), dir=getwd()) {
  restore.point("write.am")

  suppressWarnings(saveRDS(am, file.name))
}

read.am = function(file.name=paste0(dir,"/",name,".am"), dir=getwd(), name="") {
  readRDS(file.name)
}

source.extra.code.file = function(extra.code.file, am) {
  restore.point("source.extra.code.file")
  # Source extra.code
  am$extra.code.file = extra.code.file
  if (!is.null(extra.code.file)) {
    for (file in extra.code.file)
      source(extra.code.file, local = am$init.env)
  }
}



