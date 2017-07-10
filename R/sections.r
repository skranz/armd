# This file contains functions for sectioning number

armd.has.chapter = function(am) {
  any(am$bdf$type == "chapter") | !is.null(am$opts$auto.number.previous$chapter)
}

make.section.numbering = function(am, bdf = am$bdf) {
  restore.point("make.section.numbering")

  has.chapter = armd.has.chapter(am)
  parts = c(if (has.chapter) "chapter","section","subsection","subsubsection")
  bdf = am$bdf

  bdf$part.number = NA
  bdf$part.prefix = ""
  i = 1
  #li = vector("list", length(parts))
  for (i in seq_along(parts)) {
    part = parts[i]
    prev.number = first.non.null(am$opts$auto.number.previous[[part]],0)
    rows = bdf$type == part
    if (sum(rows)==0) next
    if (i == 1) {
      bdf$part.number[rows] = cumsum(rows)[rows] + prev.number
      bdf$part.prefix[rows] = paste0(am$opts$auto.number.prefix,bdf$part.number[rows])
    } else {
      parent.col = paste0("parent_", parts[i-1])
      bdf$parent.col = bdf[[parent.col]]
      # TO DO: need to compute default prefix for deeper levels
      # works so far correctly only for outer level
      default.prefix = am$opts$auto.number.previous[[parts[i-1]]]
      bdf =  bdf %>%
        #filter(type==part) %>%
        group_by_(parent.col) %>%
        mutate(
          part.number = ifelse(type==part,cumsum(type==part)+prev.number,part.number)
        ) %>%
        mutate(
          part.prefix = ifelse(type==part,paste0(
            ifelse(parent.col==0,default.prefix,bdf$part.prefix[parent.col]),".", part.number),part.prefix)
        ) %>%
        ungroup() %>%
        select(-parent.col)
    }
    #li[[i]] = bdf
  }

  am$bdf = bdf
  return(invisible(bdf))
}