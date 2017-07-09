# This file contains functions for sectioning number

armd.has.chapter = function(am) {
  any(am$bdf$type == "chapter") | !is.null(am$opts$previous.chapter)
}

make.section.numbering = function(am) {
  restore.point("make.section.numbering")

  has.chapter = armd.has.chapter(am)
  parts = c(if (has.chapter) "chapter","section","subsection","subsubsection")
  bdf = am$bdf

  bdf$part.number = NA
  bdf$part.prefix = ""
  for (i in seq_along(parts)) {
    part = parts[i]
    prev.number = first.non.null(am$opts$auto.number.previous[[part]],0)
    rows = bdf$type == part
    if (sum(rows)==0) break
    if (i == 1) {
      bdf$part.number[rows] = cumsum(rows)[rows] + prev.number
      bdf$part.prefix[rows] = paste0(am$opts$auto.number.prefix,bdf$part.number[rows])
    } else {
      parent.col = paste0("parent_", parts[i-1])
      bdf = bdf %>%
        #filter(type==part) %>%
        group_by_(parent.col) %>%
        mutate_if(type==part,
          part.number=as.numeric(seq_along(type))
        ) %>%
        s_mutate_if(paste0("type=='",part,"', part.prefix=paste0(bdf$part.prefix[",parent.col,"],'.', part.number)")) %>%
        ungroup()
    }
  }

  am$bdf = bdf
  return(invisible())
}