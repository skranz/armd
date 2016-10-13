
show.container = function(am,type="armd",  type.ind=1, bi=NULL,anim=FALSE) {
  restore.point("show.container")
  bdf = am$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)

  div.id = am$bdf$div.id[[bi]]
  setHtmlShow(id=div.id)

  #shinyjs::show(div.id,anim=anim)

  if (!is.null(am$cont.state))
    am$cont.state$hidden[bi] = FALSE

}

hide.containers = function(am, type, type.ind=NULL, bi=NULL) {
  if (is.null(bi)) {
    bis = which(am$bdf$type == type)
    if (!is.null(type.ind)) bis = bis[type.ind]
  }
  for (bi in bis) {
    hide.container(am,bi=bi,anim=FALSE)
  }
}

hide.container = function(am,type="armd",  type.ind=1, bi=NULL, anim=FALSE,animType="fade") {
  restore.point("hide.container")
  bdf = am$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)

  div.id = am$bdf$div.id[[bi]]
  setHtmlHide(div.id)

  #shinyjs::hide(div.id,anim=anim, animType="animType")
  if (!is.null(am$cont.state))
    am$cont.state$hidden[bi] = TRUE

}


render.container = function(am, type="armd",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=isTRUE(am$use.mathjax), render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE, is.rendered=NA, only.return.ui=FALSE, ui.fun = NULL, ...) {
  restore.point("render.container")

  bdf = am$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  cat("\nrender container ",bi, am$bdf$id[bi])

  if (is.na(skip.if.rendered)) {
    skip.if.rendered = TRUE
  }

  if (is.na(is.rendered)) {
    is.rendered = isTRUE(try(is.cont.rendered(bi=bi,am=am)))
  }

  if (!is.null(am$cont.state))
    am$cont.state$rendered[bi] = TRUE


  is.widget =am$bdf$is.widget[[bi]]
  # For static container only render descendants if output.id is not given
  no.render = (!only.return.ui) &
              ((is.widget & is.null(output.id)) |
              (skip.if.rendered & is.rendered))

  if (no.render) {

    if (render.desc) {
      render.container.descendants(am=am,bi=bi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered, add.desc.title=add.desc.title)
    }
    return()
  }

  if (is.null(output.id)) {
    output.id = bdf$output.id[[bi]]
  }

  if (bdf$is.widget[[bi]]) {
    render.armd.widget(am,bi=bi)
    return()
  }
  if (is.widget) {
    inner.ui = bdf$inner.ui[[bi]]
  } else {
    inner.ui = bdf$ui[[bi]]
  }

  if (!is.null(ui.fun)) {
    inner.ui = ui.fun(ui=inner.ui, bi=bi, am=am,...)
  }
  if (use.mathjax)
    inner.ui = with.mathjax(inner.ui)

  if (only.return.ui) {
    return(inner.ui)
  }
  setUI(output.id, inner.ui)
  am$cont.state$rendered[bi] = TRUE

  if (render.desc) {
    render.container.descendants(am=am,bi=bi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.desc.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
  }


}

render.container.descendants = function(am, type="armd",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=TRUE, render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE,...) {
  restore.point("render.container.descendants")
  bdf = am$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)

  child.inds = which(bdf$parent_container==bi)

  for (cbi in child.inds) {
    render.container(am=am,bi=cbi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
  }
}
