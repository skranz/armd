
render.armd.widget = function(am, bi,  ts = get.ts(bi=bi), init.handlers=TRUE) {
  restore.point("render.armd.widget")
  cat("\n******************************************")
  cat("\nrender.armd.widget")

  ao = ts$ao
  type = am$bdf$type[[bi]]
  Ao = am$Addons[[type]]
  ui = Ao$ui.fun(ts=ts)
  output.id = am$bdf$output.id[[bi]]
  setUI(output.id, ui)
  dsetUI(output.id, ui)
  if (init.handlers)
    Ao$init.handlers(ao=ao,ts=ts,bi=bi)
  #cat("render add on not yet implemented.")
}



update.widget = function(id, bi = which(am$bdf$id==id),am=get.am(),...) {
  restore.point("update.widget")
  cat("\n++++++++++++++++++++++++++++++++++++++++++")
  cat("\nupdate.widget")

  Ao = get.Widget(bi=bi)
  ao = get.widget(bi=bi)
  if (!is.null(Ao[["update"]]))
    Ao$update(ao=ao,bi=bi,...)
  render.armd.widget(bi=bi, am=am, init.handlers = FALSE)
}

get.widget = function(bi, am=get.am()) {
  am$bdf$obj[[bi]]$ao
}

make.addons.list = function(addons="quiz") {
   li = lapply(addons, function(widget) {
     fun = paste0("armd.widget.",widget)
     do.call(fun,list())
   })
   names(li) = addons
  li
}

get.Widget = function(bi=NULL,type=am$bdf$type[[bi]], am=get.am()) {
  am$Addons[[type]]
}

check.Widget = function(Ao) {
  restore.point("check.Widget")
  check.Widget.field(c("type"),type="")
  type = Ao$type
  check.Widget.field(c("package","is.task"),type=type)
  check.Widget.function("parse.fun",type)
  if (Ao$is.task) {
    check.Widget.function(c("init.task.state","init.handlers", "ui.fun"),type)
    check.Widget.field(c("need.task.env","change.task.env"),type=type)
  }



}

check.Widget.function = function(fun.name, type="") {
  for (fun in fun.name) {
    if (is.null(Ao[[fun]])) {
      stop(paste0("The widget ", type, " has not defined the function ", fun))
    }
  }
}
check.Widget.field = function(fields, type="") {
  for (field in fields) {
    if (is.null(Ao[[field]])) {
      stop(paste0("The widget ", type, " has not defined the required field ", field))
    }
  }

}