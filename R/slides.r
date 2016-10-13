find.bdf.auto.slide.type = function(df) {
  if ("frame" %in% bdf$types) return("frame")
  if ("exercise" %in% bdf$types) return("exercise")
  if ("section" %in% bdf$types) return("section")
  return("armd")
}
