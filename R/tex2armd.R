example = function() {
  setwd("D:/lehre/vwl_einf")

  setwd("D:/libraries/armd/examples")

  pre.fixed = c(
    "\\newpage","\n#. frame\n"
  )
  post.fixed = c("\\#.","#.")
  convert.tex.to.armd("test.tex", pre.fixed=pre.fixed, post.fixed=post.fixed)

}

convert.tex.to.armd = function(input.file, pre.fixed=NULL, post.fixed=NULL) {
  restore.point("convert.tex.to.armd")

  input = input.file
  base = tools::file_path_sans_ext(input.file)
  temp.input = paste0(base,".temp.tex")
  temp.output = paste0(base,".temp.md")
  armd.output = paste0(base,".armd.Rmd")

  txt = readLines(input, warn=FALSE)
  #txt = preconvert.tex.to.armd(txt)
  txt = replace.by.rules(txt, pre.fixed)
  writeLines(txt,temp.input)

  run.pandoc(temp.input, temp.output)

  txt = readLines(output, warn=FALSE)
  txt = md2armd(txt)
  txt = replace.by.rules(txt, post.fixed)
  writeLines(txt, armd.output)

}

preconvert.tex.to.armd = function(txt) {
  restore.point("preconvert.tex.to.armd")

  txt = merge.lines(txt)

  parts = c("section","subsection")

  tex = paste0("\\",parts)
  armd = paste0("\n#. ", parts)

  txt = convert.curl.tex(txt=txt, tex=tex,armd=armd)
  sep.lines(txt)
}



convert.curl.tex = function(txt,tex, armd, curls=TRUE) {
  restore.point("convert.curl.tex")
  curl.pos = str.blocks.pos(txt,"{","}")

  pos.li = lapply(seq_along(tex), function(i) {
    pos = str.find(txt,paste0(tex[[i]],"{"))
    if (NROW(pos)==0) return(NULL)
    cbind(pos, index=i)
  })
  pos = do.call("rbind", pos.li)
  curl.rows = match(pos[,2], curl.pos$outer[,1])

  inner = curl.pos$inner[curl.rows,,drop=FALSE]
  new = paste0(armd[pos[,"index"]], " ", str.at.pos(txt,inner),"\n")
  rep.pos = cbind(pos[,1], curl.pos$outer[curl.rows,2])

  res = str.replace.at.pos(txt, rep.pos,new)
}

md2armd = function(txt) {
  restore.point("md2armd")

  txt = sep.lines(txt)
  subs = c(frame="### ",subsection="## ",section="# ")
  i = 1
  for (i in seq_along(subs)) {
    md = subs[i]
    armd = paste0("#. ",names(subs)[i]," ")
    txt = gsub(md,armd,txt, fixed=TRUE)
  }
  txt
}

replace.by.rules = function(txt,rules, fixed=TRUE, newline=FALSE) {
  N = length(rules)
  fi = seq(1,N,by=2)
  ri = seq(2,N,by=2)

  find = rules[fi]
  repl = rules[ri]

  n = length(repl)
  for (i in seq_along(repl)) {
    txt = gsub(find[i], repl[i],txt, fixed=fixed)
  }
  txt
}

run.pandoc = function(input,output,options="--no-wrap --atx-headers") {
  restore.point("run.pandoc")

  com = paste0("pandoc ", options," -o ", output," ",input)
  system(com)
}
