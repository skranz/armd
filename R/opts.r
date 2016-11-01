
default.armd.opts = function(
  # packages that define blocks
  block.packages = c("armd"),

  newline.code = "§NL§",
  rmd.modes = c("rmd","tex"),
  rtutor = FALSE,
  am.type = "shiny",

  # slides
  slides = identical(am.type,"slides"),
  slide.type = "auto",
  # menu
  show.together = "section",
  menu.levels = c("section", "subsection"),
  toc.levels = c("section", "subsection"),
  number.levels = NULL,

  menu.placement = c("top","fixed"),

  nav.levels = c("section","subsection","frame"),
  is.shiny = TRUE,
  catch.errors = TRUE,
  # parameters related to chunk points
  e.points = 2,
  chunk.preknit = FALSE,
  chunk.precomp = FALSE,
  chunk.min.points=0,
  chunk.points=1,
  show.points = TRUE,
  # relevant for shiny_chunk
  show.line.numbers = TRUE,
  check.whitelist = FALSE,
  use.secure.eval = FALSE,

  noeval = FALSE, # will task_chunks not be evaluated
  preknit = FALSE,
  presolve = FALSE, # shall tasks be solved at compile time

  replace.sol = FALSE,
  show.solution.btn=FALSE,
  show.data.exp=FALSE,
  show.save.btn=FALSE,
  in.R.console=FALSE,
  # Turn off graphics when checking chunk
  use.null.device = TRUE,
  verbose = FALSE,

  #chunk.out.args = default.chunk.out.args(),
  knit.print.params = default.knit.print.params(),

  add.enter.code.here = isTRUE(am.type == "rmd"),

  name = "armd",
  id = "armd",
  hide_title = if (slides) c("section","subsection") else NULL,
  ...
) {
  args = c(as.list(environment()),list(...))
  args
}

default.knit.print.params = function(html.data.frame=TRUE,table.max.rows=40, round.digits=8, signif.digits=8) {
  as.list(environment())
}

set.armd.opts = function(opts) {
  options(.armd.OPTS=opts)
  options(.RTUTOR.OPTS=opts)
}

# Default problem set options
armd.opts = function() {
  getOption(".armd.OPTS")
}
