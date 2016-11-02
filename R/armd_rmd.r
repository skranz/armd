# we need to deal with rmd that are lists of lists
armd.set.rmd = function(bi, rmd=NULL, am = get.am(), rmd.prefix="", rmd.postfix="", newline=TRUE,opts=am$opts, modes = opts$rmd.modes) {
  restore.point("armd.set.rmd")
  #stop()
  if (length(rmd)==0) {
    am$bdf$rmd[[bi]] = list()
    return()
  }

  rmd = armd.expand.rmd.modes(rmd,modes=modes)

  nl = if (newline) opts$newline.code else NULL

  rmd = lapply(rmd, function(txt) {
    restore.point("hdhdgfzefg")
    paste0(nl,rmd.prefix,merge.lines(txt),rmd.postfix,nl)
  })
  am$bdf$rmd[[bi]] = rmd
}

armd.expand.rmd.modes = function(rmd, am=get.am(),opts=am$opts, modes = opts$rmd.modes) {
  restore.point("armd.expand.rmd.modes")

  if (!is.list(rmd)) rmd = list(rmd=rmd)

  for (m in setdiff(modes, names(rmd))) rmd[[m]] = rmd$rmd
  rmd = rmd[modes]
  rmd
}

armd.get.rmd = function(bi, mode = "rmd", am=get.am()) {
  rmd = am$bdf$rmd[[bi]]
  if (!mode %in% names(rmd)) mode="rmd"
  rmd[[mode]]
}


set.bdf.rmd = function(bi, am, shown.rmd=rmd.li$shown.rmd, sol.rmd=rmd.li$sol.rmd, out.rmd = rmd.li$out.rmd, rmd.li=NULL, rmd.prefix="", rmd.postfix) {
  restore.point("set.bdf.rmd")
  rmd = list(
    shown = merge.lines(c(rmd.prefix,unlist(shown.rmd),rmd.postfix)),
    sol = merge.lines(c(rmd.prefix,unlist(sol.rmd),rmd.postfix)),
    rmd = merge.lines(c(rmd.prefix,unlist(out.rmd),rmd.postfix))
  )
  armd.set.rmd(bi=bi, rmd=rmd, am=am)
}

armd.bind.rmd.li = function(rmd.li) {
  restore.point("armd.bind.rmd.li")
  rmd.li = rmd.li[sapply(rmd.li, length)>0]

  df = as_data_frame(data.table::rbindlist(rmd.li))
  rmd = lapply(df, paste0, collapse="")
  rmd
}

make.am.rmd = function(am=get.am(), opts=am$opts) {
  restore.point("make.am.rmd")

  rmd = am$bdf$rmd[[1]]

  nl = opts$newline.code
  nl2 = paste0(nl,nl)

  am$rmd = lapply(rmd, function(txt) {
    txt = merge.lines(txt)
    txt = gsub(nl2,nl,txt,fixed=TRUE)
    txt = gsub(nl2,nl,txt,fixed=TRUE)
    txt = gsub(nl2,nl,txt,fixed=TRUE)
    txt = gsub(nl,"\n",txt, fixed=TRUE)
    txt
  })
  invisible(am$rmd)
}