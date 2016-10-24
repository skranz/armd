find.bdf.auto.slide.type = function(bdf) {
  restore.point("find.bdf.auto.slide.type")

  if ("frame" %in% bdf$type) return("frame")
  if ("exercise" %in% bdf$type) return("exercise")
  if ("section" %in% bdf$type) return("section")
  return("armd")
}


armd.navigate.btns = function() {
  btns = tagList(
    smallButton("rtPrevBtn","<",size = "extra-small"),
    smallButton("rtNextBtn",">",size = "extra-small"),
    smallButton("rtForwardBtn",">>",size = "extra-small"),
    bsButton("rtSlideMenuBtn","", size="extra-small", icon=icon(name="bars", lib="font-awesome"))
  )
  btns
}

smallButton = function(id,label, icon=NULL, size="extra-small", extra.class=id) {
  class = paste0(c(extra.class,"btn btn-default action-button btn-xs shiny-bound-input"), collapse=" ")
  tags$button(id=id, type="button", class=class, label)
}
