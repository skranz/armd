bdf.part.filter = function(line=NULL,ranked.types=c("frame","subsection","section"),...) {
  bdf.type.filter(line=line,ranked.types=ranked.types,first.if.null=TRUE,...)
}

get.auto.filter.type = function(am, bdf=am$bdf) {
  ranked.types=c("frame","subsection","section")
  type = NULL
  if (!is.null(ranked.types)) {
    for (type in ranked.types) {
      if (sum(bdf$type == type)>0) return(type)
    }
  }

}

bdf.auto.filter.type = bdf.auto.slide.type = function(bdf,ranked.types=c("frame","subsubsection","subsection","section")) {
  type = NULL
  if (!is.null(ranked.types)) {
    for (type in ranked.types) {
      if (sum(bdf$type == type)>0) return(type)
    }
  }
}

bdf.type.filter = function(line=NULL,type.ind=NULL,bdf.ind=NULL,type=NULL, ranked.types = NULL, types.to.keep = c("precompute","armd","settings","css","head", "pane","panequiz","layout","define"), first.if.null=TRUE) {
  function(bdf, te=NULL) {
    restore.point("in.bdf.type.filer")

    # if we have multiple types, pick the first type that exists
    if (!is.null(ranked.types)) {
      for (type in ranked.types) {
        if (sum(bdf$type == type)>0) break
      }
    }

    parent.types.to.keep = types.to.keep[paste0("parent_",types.to.keep) %in% colnames(bdf)]

    bdf.ind = get.bdf.ind(line=line,type.ind=type.ind,bdf.ind=bdf.ind,bdf=bdf,te=te,type=type)
    if (length(bdf.ind)==0) {
      bdf.ind = min(which(bdf$type==type))
      if (is.na(bdf.ind) | !first.if.null)
        return(bdf)
    }

    child.ind = which(bdf[,paste0("parent_",type)] == bdf.ind)
    keep = bdf$type %in% types.to.keep & bdf$index <= bdf.ind
    for (ktype in parent.types.to.keep) {
      keep = keep | (bdf[,paste0("parent_",ktype)] >0 & bdf$index <= bdf.ind)
    }
    keep.ind = which(keep)

    rows = sort(unique(c(keep.ind,bdf.ind,child.ind)))

    # mark blocks that are only computed for side effects
    # as don't show
      # the true parent blocks must be shown
    cols = names(bdf)[str.starts.with(names(bdf),"parent_")]
    parent.inds = unique(c(1,as.numeric(bdf[bdf.ind,cols])))
    dont.show = unique(setdiff(keep.ind,c(bdf.ind, child.ind, parent.inds)))
    bdf$dont.show = FALSE
    bdf$dont.show[dont.show] = TRUE

    bdf[rows,,drop=FALSE]
  }

}
