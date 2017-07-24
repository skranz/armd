
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

makeDependencyRelativeToPackage = function(dep) {
  if (!is.null(dep$package)) return(dep)
  libPaths = .libPaths()
  restore.point("makeDependencyRelativeToPackage")

  src = dep$src[["file"]]

  src = gsub("\\\\","/",src,fixed=TRUE)

  matched.paths =  which(substring(src, 1, nchar(libPaths)) == libPaths)
  # no dependency in libPath
  if (length(matched.paths)==0) return(dep)

  libPath = libPaths[matched.paths[1]]

  package = str.right.of(src,paste0(libPath,"/"))
  package = str.left.of(package,"/")

  dep$package = package
  rel.source = str.right.of(src,paste0(libPath,"/",package,"/"))
  dep$src[["file"]] = rel.source

  dep

}
