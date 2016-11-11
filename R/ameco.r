

amecoApp = function(data = ameco, show.cntry ="DEU", show.since=1991) {
  restore.point("amecoApp")


  library(shinyEventsUI)
  library(ameco)
  library(shinyAce)
  library(memoise)

  ameco = data
  colnames(ameco)

  source = ameco.tree.source(ameco)
  tree = fancytree(id="tree",source=source)

  restore.point("ameco.inner")

  app = eventsApp()
  app$code = ""
  app$ui = jqueryLayoutPage(
    west=div(tree),
    center=div(
      selectInput("unitInput",label="Unit",choices=NULL),
      textInput("varInput",label="Variable",value=""),
      actionButton("goBtn", "Go"),
      aceEditor("codeAce",mode = "r",value=app$code, height="8em"),
      plotOutput("plotUI", height="200px"),
      uiOutput("tabUI")
    )
  )
  clickHandler("tree", function(data=NULL, app=getApp(),...) {
    restore.point("click.tree")
    data = data$values
    if (length(data)==0) return()
    d = ameco[ameco$sub.chapter==data$sub.chapter & ameco$title==data$title,,drop=FALSE]
    units = unique(d$unit)

    count = table(d$unit)
    count = count[units]
    ord = order(-count)
    units = units[ord]
    count = count[ord]


    uli = as.character(units)
    names(uli) = paste0(units," : ", count)

    uli = c(uli,"NULL"="")
    app$data = data
    app$unit = units[[1]]
    updateSelectInput(app$session, "unitInput", choices=uli)

    dp = filter(d, cntry==show.cntry, year>=show.since, unit == app$unit)

    html = html.table(dp)
    setUI("tabUI",HTML(html))
    setPlot("plotUI",ggplot2::qplot(x=dp$year, y=dp$value,xlab="Year",ylab="Value"))
  })
  buttonHandler("goBtn",function(..., app=getApp()) {

    var = getInputValue("varInput")
    unit = getInputValue("unitInput")
    title = app$data$title
    app$code = getInputValue("codeAce")
    restore.point("goBtnClick")

    code = paste0('dat = ameco_add(dat, var="',var,'", unit="',unit,'", title = "',title,'")')
    app$code = paste0(app$code,"\n",code)
    updateAceEditor(app$session, "codeAce", value = app$code)
  })
  restore.point.options(display.restore.point = TRUE)

  app
  #viewApp(app)
}


ameco.tree.source.inner = function(ameco) {
  scs = sort(unique(ameco$sub.chapter))
  scs = unique(ameco$sub.chapter)

  source = lapply(seq_along(scs), function(i.sc) {
    sc = scs[[i.sc]]
    d = ameco[ameco$sub.chapter==sc,]
    titles = unique(d$title)
    children = lapply(seq_along(titles), function(i.title) {
      title = titles[i.title]
      dc = d[d$title==title,]
      list(key=paste0("title_",i.title), title=title, expanded=FALSE, folder=FALSE,values=list(sub.chapter=sc, title=title))
    })
    #children=NULL
    list(key=paste0("sc_",i.sc), title=sc, children=children, expanded=FALSE, folder=TRUE)
  })
}


ameco.tree.source = memoise(ameco.tree.source.inner)


ameco_add = function(dat=NULL,var, title, unit=NULL) {
  restore.point("ameco_add")

  nd = ameco[ameco$title==title,,drop=FALSE]
  if (nchar(unit)>0) {
    nd = nd[nd$unit==unit,,drop=FALSE]
  }

  wd = select(nd, cntry,year, value)
  names(wd)[3] = var

  if (is.null(dat)) {
    ul = character()
    ul[[var]] = unit
    tl = character()
    tl[[var]] = title

    attr(wd,"units") = ul
    attr(wd,"titles") = tl
    return(wd)
  }

  units = attr(dat,"units")
  titles = attr(dat,"titles")
  units[[var]] = unit
  titles[[var]] =title
  d = left_join(dat,wd, by=c("cntry","year"))

  attr(d,"units") = units
  attr(d,"titles") = titles
  return(d)
}

ameco_example = function() {
  restore.point.options(display.restore.point = FALSE)

  data = filter(ameco, cntry %in% c("DEU","D"))
  app = amecoApp(data)
  viewApp(app)

  library(ameco)
  dat = NULL
  dat = ameco_add(dat, var="pop", unit="1000 persons", title = "Total population")
  dat = ameco_add(dat, var="pop_above65", unit="1000 persons", title = "Population: 65 years and over")
  dat = ameco_add(dat, var="gdp", unit="Mrd ECU/EUR", title = "Gross domestic product at current prices")

  dat = ameco_add(dat, var="gdp_per_capita", unit="(1000 EUR)", title = "Gross domestic product at current prices per head of population")
  dat = ameco_add(dat, var="rgdp_per_capita", unit="1000 EURO-DEM", title = "Gross domestic product at 2010 reference levels per head of population")


  # unemployment and employment
  dat = ameco_add(dat, var="u", unit="", title = "Unemployment rate: total :- Member States: definition EUROSTAT")
  dat = ameco_add(dat, var="empl", unit="1000 persons", title = "Employment, persons: total economy (National accounts)")
  dat = ameco_add(dat, var="empl_manufact", unit="1000 persons", title = "Employment, persons: manufacturing industry (National accounts)")
  dat = ameco_add(dat, var="empl_constr", unit="1000 persons", title = "Employment, persons: building and construction (National accounts)")
  dat = ameco_add(dat, var="empl_services", unit="1000 persons", title = "Employment, persons: services (National accounts)")

  dat = ameco_add(dat, var="empl_self", unit="1000 persons", title = "Number of self-employed: total economy (National accounts)")

  # nominal gross value added
  dat = ameco_add(dat, var="gva", unit="Mrd ECU/EUR", title = "Gross Value Added at current prices: total of branches")
  dat = ameco_add(dat, var="gva_industry", unit="Mrd ECU/EUR", title = "Gross value added at current prices: industry excluding building and construction")
  dat = ameco_add(dat, var="gva_manuf", unit="Mrd ECU/EUR", title = "Gross value added at current prices: manufacturing industry")
  dat = ameco_add(dat, var="gva_constr", unit="Mrd ECU/EUR", title = "Gross value added at current prices: building and construction")
  dat = ameco_add(dat, var="gva_service", unit="Mrd ECU/EUR", title = "Gross value added at current prices: services")

  # real gross value added
  dat = ameco_add(dat, var="rgva", unit="Mrd EURO-DEM", title = "Gross value added at 2010 prices: total of branches")
  dat = ameco_add(dat, var="rgva_agriculture", unit="Mrd EURO-DEM", title = "Gross value added at 2010 prices: agriculture, forestry and fishery products")
  dat = ameco_add(dat, var="rgva_industry", unit="Mrd EURO-DEM", title = "Gross value added at 2010 prices: industry excluding building and construction")
  dat = ameco_add(dat, var="rgva_constr", unit="Mrd EURO-DEM", title = "Gross value added at 2010 prices: building and construction")
  dat = ameco_add(dat, var="rgva_services", unit="Mrd EURO-DEM", title = "Gross value added at 2010 prices: services")
  dat = ameco_add(dat, var="rgva_manuf", unit="Mrd EURO-DEM", title = "Gross value added at 2010 prices: manufacturing industry")

  dat = ameco_add(dat, var="uwc_industry", unit="(National currency: 2010 = 100)", title = "Nominal unit wage costs: industry excluding building and construction")
  dat = ameco_add(dat, var="uwc_constr", unit="(National currency: 2010 = 100)", title = "Nominal unit wage costs: building and construction")
  dat = ameco_add(dat, var="uwc_manuf", unit="(National currency: 2010 = 100)", title = "Nominal unit wage costs: manufacturing industry")
  dat = ameco_add(dat, var="uwc_services", unit="(National currency: 2010 = 100)", title = "Nominal unit wage costs: services")

  dat = ameco_add(dat, var="i_short", unit="(%)", title = "Nominal short-term interest rates")
  dat = ameco_add(dat, var="i_long", unit="(%)", title = "Nominal long-term interest rates")

  dat = ameco_add(dat, var="income_empl", unit="Mrd EURO-DEM", title = "Compensation of employees: households and NPISH")
  dat = ameco_add(dat, var="income_transfers", unit="Mrd EURO-DEM", title = "Current transfers received: households and NPISH")
  dat = ameco_add(dat, var="net_income_capital", unit="Mrd EURO-DEM", title = "Net property income: households and NPISH")
  dat = ameco_add(dat, var="s_net", unit="", title = "Saving rate, net: households and NPISH (Net saving as percentage of net disposable income)")
  dat = ameco_add(dat, var="s_gross", unit="", title = "Saving rate, gross: households and NPISH (Gross saving as percentage of gross disposable income)")
  dat = ameco_add(dat, var="T", unit="Mrd ECU/EUR", title = "Current tax burden: total economy :- ESA 2010")
  dat = ameco_add(dat, var="T_perc", unit="(Percentage of GDP at current prices (excessive deficit procedure))", title = "Current tax burden: total economy :- ESA 2010")
  dat = ameco_add(dat, var="wage_share", unit="", title = "Adjusted wage share: total economy: as percentage of GDP at current prices (Compensation per employee as percentage of GDP at market prices per person employed.)")

  # extract German data
  d = rbind(
    filter(dat, cntry=="D", year<1991),
    filter(dat, cntry=="DEU", year >=1991)
  )
  d = copy.ameco.attributes(dat,d)

  setwd("D:/libraries/armd")
  saveRDS(d, "ameco_de.Rds")
}

copy.ameco.attributes = function(source, dest) {
  attr(dest,"titles") = attr(source,"titles")
  attr(dest,"units") = attr(source,"units")
  dest
}