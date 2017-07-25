examples.dependencies = function() {
  rmd = '
```{r}
library(ggplot2); library(plotly)
ggplotly(
  ggplot(data=data_frame(x=1:10,y=1:10),aes(x=x,y=y)) + geom_line()
)
```
'
  ui = rmdtools::knit.chunk(text = rmd,out.type = "shiny",deps.action = "ignore")
  findDependencies(ui)
  res = transformTagDependecies(ui, makeDependenciesRelativeToPackage)
  findDependencies(res)

  deps = findDependencies(ui)
  deps = makeDependenciesRelativeToPackage(deps)


  setwd("D:/libraries/armd/examples")
  am = parse.armd(file = "test.Rmd")
  am$dependencies
  ui = armd.ui(am=am)
  findDependencies(ui)
}

# will be called when a chunk is rendered
# mainly relevant if a html widget is rendered
armd.add.meta = function(am, meta) {
  restore.point("armd.add.meta")

  is.head = unlist(lapply(meta, function(el) is(el,"shiny_head")))
  is.dep = unlist(lapply(meta, function(el) is(el,"html_dependency")))

  deps = meta[is.dep]
  # need to make dependencies relative to package
  # so that they can be loaded on a different computer
  deps = lapply(deps, makeDependencyRelativeToPackage)

  am$dependencies = unique(c(am[["dependencies"]], deps))
  am$header.tags  = c(am$header.tags, meta[is.head])

}
