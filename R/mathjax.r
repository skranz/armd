with.mathjax = function (..., config=c("TeX-AMS_HTML", "TeX-AMS-MML_SVG")[2], typeset.mathjax = !TRUE)
{
  # if exists, take local mathjax version
  if (require(MathjaxLocal))
    return(MathjaxLocal::withMathJax(...))

  return(shiny::withMathJax(...))
  # new version with indiviudal config

  config.js = '
  MathJax.Hub.Config({
  jax: ["input/TeX","output/SVG","output/PreviewHTML"],
  extensions: ["tex2jax.js","fast-preview.js"],
  TeX: {
    extensions: ["AMSmath.js","AMSsymbols.js","noErrors.js","noUndefined.js"]
  }
});
  '
  #config = NULL
  config.js = NULL

  if (!is.null(config)) {
    config = paste0("?config=",config)

  }

  path <- paste0("https://cdn.mathjax.org/mathjax/latest/MathJax.js",config)

    tagList(
      # inline configuration script must come before
      # mathjax.js is loaded
      if (!is.null(config.js)) singleton(tags$head(tags$script(type="text/x-mathjax-config",config.js))),
      tags$head(
        singleton(tags$script(src = path, class="mathjax_load remove_offline", type = "text/javascript"))
      ),
      ...,
      if (typeset.mathjax) tags$script(class="mathjax_typeset remove_offline",HTML("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);"))
    )
}

mathjax.dollars.to.brackets = function(txt) {
  restore.point("mathjax.dollars.to.brackets")
  library(stringtools)
  txt = merge.lines(txt)

  # replace $$ by \[ and \]
  pos = try(str.blocks.pos(txt,start = "$$", end="$$"),silent = TRUE)
  if (!is(pos,"try-error")) {
    txt = str.replace.at.pos(txt,cbind(pos$outer[,1],pos$outer[,1]+1),new = "\\[")
    txt = str.replace.at.pos(txt,cbind(pos$outer[,2]-1,pos$outer[,2]),new = "\\]")
  }

  txt = sep.lines(txt)
  txt = gsub('\\$(.+?)\\$','\\\\(\\1\\\\)',txt)
  txt = merge.lines(txt)
  #cat(txt)
  txt
}

