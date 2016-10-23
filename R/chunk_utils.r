
remove.verbatim.end.chunks = function(chunk.start, chunk.end) {
  restore.point("remove.verbatim.end.chunks")
  df = data.frame(ind =c(0, seq_along(chunk.start), seq_along(chunk.end)),
                  row=c(0, chunk.start,chunk.end),
                  type=c("f",
                         rep("s",length(chunk.start)),
                         rep("e",length(chunk.end))
                       )
                  )
  df = arrange(df,row)
  df$del =  df$type == "e" & !is.true(lag(df$type) == "s")

  keep.ind = df$ind[df$type=="e" & !df$del]
  chunk.end[keep.ind]
}


chunk.opt.string.to.list = function(str, keep.name=FALSE) {
  restore.point("chunk.opt.string.to.list")
  #str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"

  tokens = str.split(str,",")
  str = str.between(str,"{r","}")
  code = paste0("list(",str,")")
  li = eval(base::parse(text=code,srcfile=NULL))

  if (keep.name) return(li)
  if (length(li)==0) return(li)

  #if ("replace.sol" %in% names(li))
  #  stop("nbfbfurb")
  # remove chunk.name
  if (is.null(names(li))) {
    return(li[-1])
  } else if (nchar(names(li)[1]) == 0) {
    return(li[-1])
  }
  li
}


fast.name.rmd.chunks = function(txt,prefix = "chunk_", chunk.lines = NULL, method="number") {
  restore.point("name.rmd.chunks.by.number")

  if (is.null(chunk.lines)) {
    chunk.lines = which(str.starts.with(txt,"```{r"))
  }
  if (length(chunk.lines)==0) return(txt)
  str = str.right.of(txt[chunk.lines],'```{r')

  comma.pos = str.locate.first(str,",")[,1]
  eq.pos = str.locate.first(str,"=")[,1]
  brace.pos = str.locate.first(str,"}")[,1]

  has.eq = !is.na(eq.pos) & !is.true(comma.pos < eq.pos)
  has.comma = !is.na(comma.pos) & !has.eq

  if (method=="number") {
    name = paste0('"chunk_',seq_along(str),'"')
  } else {
    name = random.string(length(str),nchar = 14)
  }
  right = str
  right[has.eq] = paste0(", ",str[has.eq])
  right[has.comma] = substring(str[has.comma],comma.pos[has.comma])
  right[!has.comma & !has.eq] = "}"

  new =  paste0("```{r ",name,right)
  txt[chunk.lines] = new
  txt
}

