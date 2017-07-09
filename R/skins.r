
armd.navbar = function(am, opts=armd.opts(), nav.levels = c("section","subsection","frame")) {
  restore.point("armd.navbar")

  bdf = am$bdf
  nav.levels = intersect(nav.levels, bdf$type)


  get.raw.selector = function(nav.levels=nav.levels, parent.bi=NULL, level=1, name=nav.levels[[1]]) {
    restore.point("get.raw.selector")
    #stop()
    if (is.null(parent.bi)) {
      bis = which(bdf$type == nav.levels[1])
    } else {
      bis = which(bdf$parent_container==parent.bi)
    }
    types = bdf$type[bis]
    ignore = !types %in% nav.levels
    bis = bis[!ignore]
    types = types[!ignore]
    if (length(bis)==0) {
      return(NULL)
    }

    titles = sapply(seq_along(bis), function(i) {
      bi = bis[i]
      obj = bdf$obj[[bi]]
      title = first.non.null(obj$button.label, obj$title, obj$name, paste0(bdf$type[[bi]]," ",bdf$stype.ind[[bi]]))
      title
    })
    choices = bis
    names(choices) = titles

    contents = bdf$div.id[bis]
    children = as.list(paste0("s",bis))
    names(children) = bis


    children.sel = lapply(bis, function(bi) {
      child.sel.li = get.raw.selector(nav.levels=nav.levels,parent.bi = bi,level=level+1, name=paste0("s",bi))[[1]]
    })
    names(children.sel) = paste0("s",bis)
    is.child = !sapply(children.sel, is.null)
    children = children[is.child]
    children.sel = children.sel[is.child]

    sel = list(list(
      bis = bis,
      choices = choices,
      children = children,
      div = as.character(level),
      contents = contents
    ))
    names(sel)=name
    c(sel,children.sel)
  }

  am$menu.selectors = get.raw.selector(nav.levels=nav.levels, level=1, name="armd")

  am$menu.sel.ui = nestedSelector(id="rtNavbarSelector", btn.size="xs", am$menu.selectors, input.type="radioBtnGroup", scroll.top.sel = "#mainLayout_center")



  title = first.non.null(opts$menu.title, "Sections")
  am$navbar.ui = div(
    #style = "background-color: #eeeeee;",
    HTML("<table><tr><td style='padding-left: 5px; padding-right: 10px; vertical-align: top;'>"),
    HTML(title),
    HTML("</td><td>"),
    am$menu.sel.ui$ui,
    HTML("</td></tr></table>")
   )
  am$navbar.ui
}

slide.title.bar.ui = function(title, slide.ind, num.slides, am, bi) {

  restore.point("slide.title.bar.ui")
  cp =am$opts[["slide.counter.parent"]]
  bdf = am$bdf
  if (is.null(cp)) {
    ind.str = paste0(slide.ind, " / ",num.slides)
  } else {
    parent.col =paste0("parent_",cp)
    parent.bi = bdf[[parent.col]][[bi]]
    rows = bdf$type==am$slide.type & bdf[[parent.col]]==parent.bi
    num.slides = sum(rows)
    slide.ind = sum(rows & bdf$index <= bi)
    prefix = bdf$part.prefix[[parent.bi]]
    if (!is.null(prefix)) {
      ind.str = paste0(prefix, "-", slide.ind)
    } else {
      ind.str = paste0(slide.ind, " / ",num.slides)
    }
  }

  div(class="armd-slide-title-bar",
    HTML("<table width='100%'><tr><td>"),
    h4(class="slide_title",title),
    HTML("</td><td align='right' valign='top' nowrap>"),
    HTML("<table><tr><td valign='center' nowrap>"),
    div(class="nav_buttons_div remove_offline_print",  armd.navigate.btns()),
    HTML("</td><td valign='center' nowrap style='padding-left: 5px'>"),
    HTML(ind.str),
    HTML("</td></tr></table>"),
    HTML("</td></tr></table>")
  )
}
