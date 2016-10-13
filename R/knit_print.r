set.knit.print.opts = function(output=try(knitr:::pandoc_to()), html.data.frame=TRUE,table.max.rows=25, round.digits=8, signif.digits=8, show.col.tooltips = TRUE, env=.GlobalEnv,...) {
  restore.point("set.knit.print.opts")
  #cat(output)
  if (is(output,"try-error")) output="html"
  if (is.null(output)) output = "html"
  opts = make.knit.print.opts(html.data.frame,table.max.rows, round.digits, signif.digits, show.col.tooltips = FALSE, output=output)
 
  for (opt in opts) {
    fun.names = paste0("knit_print.",opt$classes)
    if (!is.null(opt$fun)) {
      for (fun.name in fun.names)
        env[[fun.name]] = opt$fun
    }
  }

}

make.knit.print.opts = function(html.data.frame=TRUE,table.max.rows=25, round.digits=8, signif.digits=8, show.col.tooltips = TRUE,output="html") {
  opts = list()
  restore.point("make.knit.print.opts")
  
  #attr(opts,"knit.params") = nlist(html.data.frame,table.max.rows, round.digits, signif.digits, show.col.tooltips)
  
  if (html.data.frame) {
    opts[["data.frame"]] = list(
      fun= function(x, options=NULL, ...) {
        restore.point("ndnfhdubfdbfbfbh")
        
        armd.knit_print.data.frame(x,table.max.rows=table.max.rows, round.digits=round.digits, signif.digits=signif.digits, show.col.tooltips=show.col.tooltips, options=options,output=output,...)  
      },
      classes=c("data.frame","matrix")
    )
  }  
  opts
}

make.knit.print.funs = function(knit.print.opts, parent.env = globalenv()) {
  env = new.env(parent=parent.env)
  for (opt in knit.print.opts) {
    fun.names = paste0("knit_print.",opt$classes)
    if (!is.null(opt$fun)) {
      for (fun.name in fun.names)
        env[[fun.name]] = opt$fun
    }
  }
  as.list(env)
}

add.htmlwidget.as.shiny <- function(x,
   outputId=paste0("htmlwidget_output",sample.int(100000,1)),
   session = getDefaultReactiveDomain(), app=getApp()) 
{
  restore.point("add.htmlwidget.as.shiny")

  widget.name = class(x)[1]
  
  # output function for widget
  outputFun <- function(outputId, width = "100%", height = "400px",...) {
    htmlwidgets::shinyWidgetOutput(outputId, widget.name, width, height)
  }

  # add renderer that simply evaluates the htmlwidget x
  env = new.env(parent=globalenv())
  env$x = x
  
  renderer = htmlwidgets::shinyRenderWidget(quote(x), outputFunction=outputFun, env=env, quoted=TRUE)
  app$output[[outputId]] = renderer 
  if (!is.null(session))
    session$output[[outputId]] = renderer
  
  # create ui
  ui = outputFun(outputId)

  return(ui)
}

# overwrite knit_print for htmlwidget
armd.knit_print.htmlwidget = function(x,...) {
  restore.point("armd.knit_print.htmlwidget")
    
  am = get.am()
  chunk.ind = am$chunk.ind
  outputId=paste0("chunk_htmlwidget_",am$cdt$nali[[chunk.ind]]$name)

  ui = add.htmlwidget.as.shiny(x, outputId = outputId)
  ui = add.htmlwidget.as.shiny(x)
  restore.point("ndjndhvbrubr")
  
  # knitr shall output ui
  knit_print.shiny.tag.list(ui)
}

armd.knit_print.shiny.tag.list = function (x, ...) 
{
  restore.point("armd.knit_print.shiny.tag.list")
  
  x <- htmltools:::tagify(x)
  output <- surroundSingletons(x)
  deps <- resolveDependencies(htmltool:::findDependencies(x))
    content <- htmltool:::takeHeads(output)
    head_content <- htmltool:::doRenderTags(tagList(content$head))
    meta <- if (length(head_content) > 1 || head_content != "") {
        list(structure(head_content, class = "shiny_head"))
    }
    meta <- c(meta, deps)
    knitr::asis_output(HTML(format(content$ui, indent = FALSE)), 
        meta = meta)
}

armd.knit_print.data.frame = function(x, table.max.rows=25, round.digits=8, signif.digits=8, html.data.frame=TRUE, show.col.tooltips=TRUE, col.tooltips=NULL, output="html", options=NULL, ...) {
  restore.point("armd.knit_print.data.frame")
  
  # chunk options have precedent over passed arguments
  copy.non.null.fields(dest=environment(), source=options, fields=c("table.max.rows","round.digits","signif.digits","html.data.frame","show.col.tooltips"))
  
  #col.tooltips = NULL
  if (show.col.tooltips & is.null(col.tooltips)) {
    var.dt = get.am()$am$var.dt
    if (!is.null(var.dt)) {
      vars = colnames(x)
      col.tooltips = get.var.descr.dt(vars=vars, var.dt=var.dt)$descr
      col.tooltips = paste0(vars, ": ", col.tooltips)
      col.tooltips = sapply(col.tooltips,USE.NAMES = FALSE, function(str) {
        paste0(strwrap(str, width=30), collapse="\n")
      })
    }
  }
  
  
  MAX.ROW = table.max.rows
  if (NROW(x)>MAX.ROW) {
    rows = 1:MAX.ROW
    
    if (html.data.frame) {
      missing.txt = paste0("... only ", MAX.ROW ," of ", NROW(x), " rows  shown  ...")
      if (output=="html") {
        h1 = armd:::html.table(x[rows,],round.digits=round.digits, signif.digits=signif.digits, col.tooltips=col.tooltips,...)
        html = c(h1, as.character(p(missing.txt)))
      } else {
        dat = format.data.frame(x[rows,],signif.digits = signif.digits, round.digits = round.digits) 
        html = paste0(c(kable(dat),missing.txt),collapse="\n")
      }
    } else {
      dat = format.data.frame(x[rows,],signif.digits = signif.digits, round.digits = round.digits) 
      txt = capture.output(print(dat))
      txt = c(paste0(txt,collapse="\n"),paste0("... only ", MAX.ROW ," of ", NROW(x), " rows shown ..."))
      
      return(txt)
    }

  } else {
    if (html.data.frame) {
      html = armd:::html.table(x,round.digits=round.digits, signif.digits=signif.digits, col.tooltips=col.tooltips, ...)
    } else {
      restore.point("ndjhdbfdub")
      
      dat = format.data.frame(x,signif.digits = signif.digits, round.digits = round.digits) 
      txt = paste0(capture.output(print(dat)), collapse="\n")
      return(txt)
    }
  }
  asis_output(html)
}

format.vals = function(vals, signif.digits=NULL, round.digits=NULL) {
  if (is.numeric(vals)) {
    if (is.null(signif.digits) & is.null(round.digits)) {
      return(vals)
    } else if (!is.null(signif.digits) & is.null(round.digits)) {
      return(signif(vals, signif.digits))
    } else if (is.null(signif.digits) & !is.null(round.digits)) {
      return(round(vals, signif.digits))
    } else {
      return(signif(round(vals, round.digits), signif.digits))
    }
  }
  vals
}

format.data.frame = function(x, signif.digits=NULL, round.digits=NULL) {
  as.data.frame(lapply(x, format.vals, signif.digits=signif.digits, round.digits=round.digits))
}
