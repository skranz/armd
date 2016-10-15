with.mathjax = function (..., config="TeX-AMS_HTML")
{
    path <- paste0("https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=",config)

    tagList(
      tags$head(
        singleton(tags$script(src = path, class="mathjax_load remove_offline", type = "text/javascript"))
      ),
        ..., tags$script(class="mathjax_typeset remove_offline",HTML("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}

