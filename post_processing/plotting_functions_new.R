# Created on Thu Apr 18 08:46:48 2019
# @author: bsergi
sjt.df(iris,
       altr.row.col = T, # this colors the rows
       title = "Descriptive statistics", #always give
       #your tables
       #titles
       file = "sjt_des_2.doc")
install.packages("mvtnorm")
install.packages("emmeans")
install.packages("sjstats")
install.packages("sjPlot")
install.packages("cowplot")

library(cowplot)
library(ggplot2)
library(openxlsx)
library(plyr)
library(reshape2)
library(skimr)
library(dplyr)
#library(sjPlot)
library(table1)
library(xtable)


## Working directory and inputs ####
#baseWD <- "/Users/Cartographer/GAMS/dispatch_RA-master"
resultsFolder <- "model_runs" #this is now defunct, edited by Luke 8.13.2019

baseWD <- "C:/Users/Luke/Desktop/dispatch_RA_model-dev"
setwd(paste(baseWD, "post_processing", sep="/"))

#Figure labeller
#https://logfc.wordpress.com/2017/03/15/adding-figure-labels-a-b-c-in-the-top-left-corner-of-the-plotting-region/
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
  
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
  
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
    
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
  
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
  
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
  
  x1 <- switch(pos,
               topleft     =x[1] + sw, 
               left        =x[1] + sw,
               bottomleft  =x[1] + sw,
               top         =(x[1] + x[2])/2,
               center      =(x[1] + x[2])/2,
               bottom      =(x[1] + x[2])/2,
               topright    =x[2] - sw,
               right       =x[2] - sw,
               bottomright =x[2] - sw)
  
  y1 <- switch(pos,
               topleft     =y[2] - sh,
               top         =y[2] - sh,
               topright    =y[2] - sh,
               left        =(y[1] + y[2])/2,
               center      =(y[1] + y[2])/2,
               right       =(y[1] + y[2])/2,
               bottomleft  =y[1] + sh,
               bottom      =y[1] + sh,
               bottomright =y[1] + sh)
  
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
  
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}

# loads da/day-of/RT loads for plotting
readLoads <- function(filename,WD){
  setwd(paste(WD, "raw_data", sep="/"))
  df <- read.csv(filename)
  #convert datetime to R format
  df$datetime <- as.POSIXct(df[,"Local.Datetime..Hour.Ending."], format="%m/%d/%Y %H:%M")
  #reportedLMPs$datetime <- reportedLMPs$datetime + 1*3600 #get the times in same
  df$date <-format(df$datetime, "%m-%d-%y")
  #sum loads across region
  subsetcols <- c("datetime","Day.Of.Load.Forecast.MW..Average",
                  "Day.Ahead.Load.Forecast.MW..Average","Actual.Load.MW..Average")
  #zonalLoad[,c("date", "timepoint", "zone", "gross_load")]
  datadf <- df[,subsetcols]
  sumdf <- datadf %>% 
    group_by(datetime) %>% 
    summarise_all(funs(sum))
  sumdf$date <- format(sumdf$datetime,"%m-%d-%y")
  return(sumdf)
}

readLoads("da_rt_loads.csv",baseWD)

#function to boxplot load forecasts at different time intervals
compareLoads <- function(filename, WD, AllDates,DatesSubset1,DatesSubset2){
  df <- readLoads(filename,WD)
  
  #reportedLMPsub <- reportedLMPs[(reportedLMPs$date %in% format(dates, "%m-%d-%y")) & (df$Load.Region=="PJM EAST"),]
  suball <- df[(df$date %in% format(AllDates, "%m-%d-%y")),]# & (df$Load.Region=="PJM EAST"),]
  sub1 <- df[(df$date %in% format(DatesSubset1, "%m-%d-%y")),]# & (df$Load.Region=="PJM EAST"),]
  sub2 <- df[(df$date %in% format(DatesSubset2, "%m-%d-%y")),]# & (df$Load.Region=="PJM EAST"),]

  
  #add labels
  suball$label <- "Hourly 2014-2019"
  sub1$label <- "January 2014 Week"
  sub2$label <- "October 2017 Week"
  
  #tidy data for creating boxplot
  #print(sub1)
  #print(sub2)
  datadf <- rbind(suball,sub1,sub2)
  subsetcols <- c("datetime","label","Day.Ahead.Load.Forecast.MW..Average",
                  "Day.Of.Load.Forecast.MW..Average","Actual.Load.MW..Average")
  #zonalLoad[,c("date", "timepoint", "zone", "gross_load")]
  datadf <- datadf[,subsetcols]
  colnames(datadf) <- c("datetime","label","Day Ahead Load Forecast", "Day Of Load Forecast",
                        "Actual Load")
  #melt
  tidydf <- melt(datadf, id.vars=c("datetime", "label"),
       measure.vars=c("Day Of Load Forecast","Day Ahead Load Forecast","Actual Load"))
  #tidydf$GW <- as.numeric(tidydf$variable)/1000
  #print(tidydf)
  #colnames(tidydf) <- c("datetime","label")
  #scale_colour_manual(values=dispatchcolors)
  #ylim(c(25000,50000))+
  
  tidydf$variable <- factor(tidydf$variable, levels=c("Day Ahead Load Forecast",
                                                      "Day Of Load Forecast",
                                                      "Actual Load"))
  mycolors <- c("#FDAA83","#7498C7","#FEE8AC")
  
  #add columns to get deltas
  arrangeddf <- tidydf %>% arrange(desc(variable), label,datetime)
  #subset, repeat, add as new column
  arrangeddf$ActualLoad <- rep(arrangeddf$value[arrangeddf$variable=="Actual Load"],3)
  arrangeddf$Delta <- arrangeddf$value - arrangeddf$ActualLoad
  #print(arrangeddf)
  #finally, do plotting
  
  ggplot(tidydf, aes(x=label, y=value, fill=variable)) + 
    geom_boxplot()+ xlab("") + ylab("Megawatts (MW)") +
    ylim(c(60000,120000))+
    theme_classic() +
    scale_fill_manual(values=mycolors)+
    guides(fill=guide_legend(title="", nrow=2, byrow=TRUE))+
    theme(legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position="top",
          axis.title.y = element_text(size=22),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste("Load Forecast Compare", ".png"),dpi=500,units="in", width=10, height=6)
  
  ggplot(arrangeddf, aes(x=label, y=Delta, fill=variable)) + 
    geom_boxplot()+ xlab("") + ylab("Delta v. Actual (MW)") +
    theme_classic() +
    ylim(c(-4000,4000))+
    scale_fill_manual(values=mycolors)+
    guides(fill=guide_legend(title="", nrow=2, byrow=TRUE))+
    theme(legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position="top",
          axis.title.y = element_text(size=22),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste("Delta Load Forecast Compare", ".png"),dpi=500,units="in", width=10, height=6)
  
  
  #return(tidydf)
}
AllDates <- seq(as.POSIXct("1/1/2014", format = "%m/%d/%Y"), by="day", length.out=2191)
resultdf <- compareLoads("da_rt_loads.csv",baseWD,AllDates,dates1,dates2)
#sort to get in proper order


## Load model results ####

# helper function to read all files in date range of a specific output
readFiles <- function(filename, dates, dateWD, subFolder="results"){

  for(i in 1:length(dates)){
    date <- dates[i]
    dateString <- paste(as.numeric(format(date, "%m")), as.numeric(format(date, "%d")), as.numeric(format(date, "%Y")), sep=".")
    setwd(paste(dateWD, dateString, subFolder, sep="/"))
    
    # load file
    df <- read.csv(filename)
    df$date <- date
    
    if(i == 1){
      fullDF <- df
    } else{
      fullDF <- rbind(fullDF, df)
    }
  }
  return(fullDF)
}

loadResults <- function(dates, case){
        
  dateMonth <- unique(format(dates, "%b"))  # extract month of dates for directory
  dateYear <- unique(format(dates, "%Y"))  # extract year of dates for directory
  
  if (length(dateMonth) == 1){
    dayStart <- as.numeric(format(dates[1], "%d"))
    dayEnd <- max(as.numeric(format(dates[length(dates)], "%d")),dayStart+6)
    print(dayEnd)
    #dayEnd <- dayEnd
    # case options: NoReserves, SimpleORDC, ConstantReserves, withORDC
    # there will be new case options but this should be ok
    directory <- paste(dateMonth, dayStart, dayEnd, dateYear, case, sep="_")
    dateResultsWD <- paste(baseWD, directory, sep="/")
    
  } else{
    print("Error. Multiple months in date range.")   # multiple months yields error for now, can update based on directory format
    return(NULL)
  }
  
  modelLMP <- readFiles("zonal_prices.csv", dates, dateResultsWD)
  txFlows <- readFiles("tx_flows.csv", dates, dateResultsWD)
  reserves <- readFiles("reserve_segment_commit.csv", dates, dateResultsWD)
  dispatch <- readFiles("generator_dispatch.csv", dates, dateResultsWD)
  VRE <- readFiles("renewable_generation.csv", dates, dateResultsWD)
  Objective <- readFiles("comparison_objective_function_value.csv",dates,dateResultsWD)
  genreserves <- readFiles("generator_commits_reserves.csv",dates,dateResultsWD)
  
  ordc <- readFiles("full_ordc.csv", dates, dateResultsWD, subFolder="inputs")
  gens <- readFiles("PJM_generators_full.csv", dates, dateResultsWD, subFolder="inputs")
  zonalLoad <- readFiles("timepoints_zonal.csv", dates, dateResultsWD, subFolder="inputs")
  genCapacity <- gens
  genScheduledAvail <- readFiles("PJM_generators_scheduled_outage.csv", dates, dateResultsWD, subFolder="inputs")
  rampdata <- readFiles("PJM_generators_zone.csv",dates,dateResultsWD,subFolder="inputs")
  
  #print(case)
  if (case=="DynamicORDC"){
    capacity_outage <- readFiles("daycopt.csv",dates,dateResultsWD,subFolder="inputs")
    #print(capacity_outage)
  }
  
  
  #format ramp data
  rampdata <- rampdata[c("Gen_Index","Ramp_Rate")]#retain only ramp rate
  rampdata <- rampdata[!duplicated(rampdata),]#drop duplicates to have only a single zone
  
  # some formatting
  gens <- gens[!duplicated(gens),] # remove duplicate generations
  gens <- gens[,c("Name", "Zone", "Category")]  # subset generator columns
  
  ## will eventually want an if: clause here so only new cases do this
  #reformat full_ordc so it matches previous format
  ordc <- ordc[,c("timepoint","segments","SynchMW","Price","date")]
  colnames(ordc) <- c("timepoint","segments","MW","Price","date")
  
  #overwrite, but defunct
  #modelLMP$newLMP <- 0
  #modelLMP <- transform(modelLMP, newLMP = ifelse(PrimarySynchPenaltyFactor == 300 & LMP>300, LMP-PrimarySynchPenaltyFactor, LMP))
  #print(modelLMP)
  
  #reformat zonal prices so it matches previous format
  modelLMP <- modelLMP[,c("X","hour","LMP","date")]
  
  #reformat reserves
  reserves <- reserves[,c("X","MW.on.primary.synch.reserve.segment","date")]
  colnames(reserves) <- c("X","MW.on.reserve.segment","date")
  
  # return results
  if (case=="DynamicORDC"){
    results <- list(modelLMP, zonalLoad, reserves, ordc, dispatch, VRE, gens, txFlows,Objective, genreserves,genCapacity,genScheduledAvail,rampdata,capacity_outage)
    names(results) <- c("modelLMP", "zonalLoad", "reserves", "ordc", "dispatch", "VRE", "gens", "txFlows","Objective","GenReserves","GenCapacity","ScheduledAvail","rampdata","CapacityOutage")
    return(results)
  }
  else{
    results <- list(modelLMP, zonalLoad, reserves, ordc, dispatch, VRE, gens, txFlows,Objective, genreserves,genCapacity,genScheduledAvail,rampdata)
    names(results) <- c("modelLMP", "zonalLoad", "reserves", "ordc", "dispatch", "VRE", "gens", "txFlows","Objective","GenReserves","GenCapacity","ScheduledAvail","rampdata")
    return(results)
  }
}

# helper function to load data from all four cases
loadAllCases <- function(dates, cases){
  results <- list()
  for(case in cases){
    results[[case]] <- loadResults(dates, case=case)
  }
  return(results)
}

aggregateCaseData <- function(results, targetData){
  newDF <- data.frame()
  # format results by case
  for(case in names(results)){
    dfTemp <- results[[case]][[targetData]] 
    dfTemp$case <- case
    newDF <- rbind(newDF, dfTemp)
  }
  return(newDF)
}

## LMPs ####

readPJM_LMPs <- function(dates,pricetype="PJM-DAH LMP"){
  setwd(paste(baseWD, "post_processing", sep="/"))
  reportedLMPs <- read.csv("lmp_historical_wRT.csv")

  reportedLMPs$datetime <- as.POSIXct(reportedLMPs[,"Local.Datetime..Hour.Ending."], format="%m/%d/%Y %H:%M")
  #reportedLMPs$datetime <- reportedLMPs$datetime + 1*3600 #get the times in same
  reportedLMPs$date <-format(reportedLMPs$datetime, "%m-%d-%y")
  reportedLMPs$hour <- as.numeric(format(reportedLMPs$datetime, "%H"))
  
  reportedLMPsub <- reportedLMPs[(reportedLMPs$date %in% format(dates, "%m-%d-%y")) & (reportedLMPs$Price.Type.Full.Description==pricetype),]
  reportedLMPsub$date <- as.POSIXct(reportedLMPsub$date, format="%m-%d-%y")
  
  #subset out whether DA or RT
  
  
  return(reportedLMPsub)
}

plotLMPs <- function(results, dates, plotTitle, truncate_value=NA, PJM_only=FALSE){
  
  modelLMP <- aggregateCaseData(results, "modelLMP")
  
  # zonalLoad is the same across cases (model input)
  zonalLoad <- results[[1]][["zonalLoad"]]
    
  #formatting of model LMP
  colnames(modelLMP)[1] <- "Node"
  
  # add in gross load
  modelLMP <- merge(modelLMP, zonalLoad[,c("date", "timepoint", "zone", "gross_load")], 
                    by.x=c("date", "hour", "Node"), by.y=c("date", "timepoint", "zone"), all=T, sort=F)

  # calculated generation-weighted average across zones for PJM-wide LMP
  PJM_LMP <- ddply(modelLMP, ~ case + date + hour, summarise, 
                   LMP = sum(gross_load * LMP / sum(gross_load)), 
                   gross_load = sum(gross_load))
  PJM_LMP$Node <- "PJM"
  PJM_LMP <- PJM_LMP[,c("date", "hour", "Node", "LMP", "case", "gross_load")]
  modelLMP <- rbind(modelLMP, PJM_LMP)
  modelLMP <- subset(modelLMP, select=-gross_load)
  
  # harmonize zone names and formats for merge
  modelLMP$Node <- mapvalues(modelLMP$Node, from=c("DC_BGE_PEP", "PA_METED_PPL"), to=c("BGE", "PPL"))
  
  # shift hour window and drop any timepoints outside of 24 hour window
  modelLMP$hour <- modelLMP$hour - 1
  modelLMP <- modelLMP[modelLMP$hour < 24,]
  
  # load and format PJM LMPs
  reportedLMPsub <- readPJM_LMPs(dates)
  reportedLMPsub$case <- "reported"
  reportedLMPsub <- reportedLMPsub[, c("date", "hour", "Price.Node.Name", "Price...MWh", "case")]
  colnames(reportedLMPsub) <- c("date", "hour", "Node", "LMP", "case")
  reportedLMPsub$Node <- mapvalues(reportedLMPsub$Node, 
                                   from=c("PJM-RTO ZONE", "DOMINION HUB", "EASTERN HUB", "WESTERN HUB"), 
                                   to=c("PJM", "VA_DOM", "EAST", "WEST"))
  
  # merge reported and modeled data
  fullLMP <- rbind(modelLMP, reportedLMPsub)
  
  
  fullLMP$datetime <- with(fullLMP, paste(date, hour))
  fullLMP$datetime <- as.POSIXct(fullLMP$datetime, format = "%Y-%m-%d %H")
  
  #plotting reformatting and attempts, by luke
  case1 <- unique(fullLMP$case)[1]
  tablevecs <- fullLMP$LMP[fullLMP$case==case1 & fullLMP$Node=="PJM"]
  #deltavec <- fullLMP$LMP[fullLMP$case==case1]-fullLMP$LMP[fullLMP$case=="reported"]
  for (c in unique(fullLMP$case)[2:length(unique(fullLMP$case))]){
    addvec <- fullLMP$LMP[fullLMP$case==c & fullLMP$Node=="PJM"]
    tablevecs <- cbind(tablevecs,addvec)
  }
  colnames(tablevecs) <- unique(fullLMP$case)
  #print(summary(tablevecs))
  
  plotfullLMP <- fullLMP[fullLMP$Node=="PJM",c("LMP","case","datetime")]
  subtractvec <- plotfullLMP$LMP[plotfullLMP$case=="reported"]
  fullsubtractvec <- rep(subtractvec, length(unique(plotfullLMP$case)))
  plotfullLMP$deltaLMP <- plotfullLMP$LMP - fullsubtractvec
  plotfullLMP$deltaLMP <- sqrt((plotfullLMP$deltaLMP)**2)
  #print(group_by(plotfullLMP, case) %>% skim())
  
  #print(plotfullLMP)
  
  #table1::label(plotfullLMP$LMP) <- "LMP ($/MWh)"
  #table1::label(plotfullLMP$deltaLMP) <- "delta LMP v. Reported ($/MWh)"
  #print(table1::table1(~LMP+deltaLMP | case, data = plotfullLMP,topclass="Rtable1-zebra"))
  
  
  # plot
  #print(names(results))
  cases <- names(results)
  cases <- c(cases,"reported")
  #print(cases)
  #cases <- c("changes","changes_full","reported")
  #cases <- c("PJMHistorical",
  #           "PJMHistoricalChangeWestGasHub","reported")
  
  palette <- c('#d73027','#fc8d59','#e0f3f8','#91bfdb','#4575b4','#fee090')
  
  brian_colors <- c()
  linetypes <- c()
  for (i in 1:length(cases)){
    #print(i)
    if (cases[i]=="reported"){
      brian_colors <- c(brian_colors,'#000000')
      linetypes <- c(linetypes,1)
    }
    else{
      #print(i)
      #print(brian_colors)
      brian_colors <- c(brian_colors,palette[i])
      linetypes <- c(linetypes,i%%2+2)
    }
  }
  
  print(linetypes)
  print(brian_colors)
  #brian_colors <- c('#d73027','#fc8d59','#000000')
  #linetypes <- c(2,2,1)
  #brian_colors <- c('#4daf4a','#377eb8','#000000')
                    #,'#984ea3','#ff7f00')
  my_color_palette <- c("green","red","black")
  #scale_linetype_manual(breaks = cases, values=c(3,2,1,4,5)) +
  #scale_colour_manual(breaks = cases, values=brian_colors) +
  
  if (PJM_only==TRUE){
    fullLMP <- fullLMP[fullLMP$Node=="PJM",]
  }
  print(fullLMP)
  fullLMP$Node[fullLMP$Node=="PPL"] <- rep("PPL-METED",length(fullLMP$Node[fullLMP$Node=="PPL"]))
  fullLMP$Node[fullLMP$Node=="VA_DOM"] <- rep("DOM",length(fullLMP$Node[fullLMP$Node=="VA_DOM"]))
  fullLMP$Node[fullLMP$Node=="BGE"] <- rep("BGE-PEP",length(fullLMP$Node[fullLMP$Node=="BGE"]))
  
  #print(fullLMP)
  fullLMP$Node <- factor(fullLMP$Nod, levels=c("BGE-PEP",
                                               "EAST",
                                               "PPL-METED",
                                               "DOM",
                                               "WEST",
                                               "PJM"))
  #ylim(c(-400,400)) +
  fullLMP$deltaLMP <- fullLMP$LMP - rep(fullLMP$LMP[fullLMP$case=="reported"],5)
  fullLMP <- fullLMP[fullLMP$case!="reported",]
  #guides(linetype = guide_legend(override.aes = list(size = 2)))
  LMPplot <- ggplot(data=fullLMP, aes(x=datetime, y=deltaLMP, colour=case, linetype=case)) + 
    facet_wrap(~Node) + geom_line(size=2) + theme_classic() + 
    xlab("") + ylab("??LMP vs. DA Reported ($ per MWh)") +
    guides(colour=guide_legend(title="Scenario: ", nrow=2),linetype=guide_legend(title="Scenario: ", override.aes=list(size=10),nrow=2)) + 
    scale_linetype_manual(breaks = cases, values=linetypes) +
    scale_colour_manual(breaks = cases, values=brian_colors) +
    theme(legend.text = element_text(size=32),
          legend.title=element_text(size=32),
          legend.position = "bottom",
          axis.title.y = element_text(size=20),
          axis.text.x= element_text(size=10),
          axis.text.y= element_text(size=16),
          strip.text.x = element_text(size = 18),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks=element_line(size=2),
          plot.margin=unit(c(.1,.5,0,.1),"cm"))
  
  if(!is.na(truncate_value) & max(fullLMP$LMP) > truncate_value){
    print("passed.")
    LMPplot <- LMPplot + coord_cartesian(ylim = c(0,truncate_value))
  } 
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("LMPs ", plotTitle, ".png"), dpi=500,units="in", width=12, height=6)
  return(LMPplot)
}
#plotLMPs(resultsOct, alldates, plotTitle="Oct 2017")
p1 <- plotLMPs(results1,dates1,plotTitle="Jtest")
p2 <- plotLMPs(results2,dates2,plotTitle="Otest")
g1<-plot_grid(p1+theme(legend.position="none"),
              p2+theme(legend.position="none"),
              align = 'vh',
              labels = c("A", "B"),
              label_size=20,
              hjust=-1,
              rel_widths=c(1,1))
legend <- get_legend(p1+ theme(legend.box.margin = margin(0, 0, 2, 0)))
mygrid <- plot_grid(g1, legend, rel_heights = c(1, .15),nrow=2)
setwd(paste(baseWD, "post_processing", "figures", sep="/"))
ggsave(paste("LMPAll", ".png", sep=""),dpi=500,units="in", width=12, height=6)


plotLMPsZonelabel <- function(results, dates, plotTitle, truncate_value=NA){
  
  modelLMP <- aggregateCaseData(results, "modelLMP")
  
  # zonalLoad is the same across cases (model input)
  zonalLoad <- results[[1]][["zonalLoad"]]
  
  #formatting of model LMP
  colnames(modelLMP)[1] <- "Node"
  
  # add in gross load
  modelLMP <- merge(modelLMP, zonalLoad[,c("date", "timepoint", "zone", "gross_load")], 
                    by.x=c("date", "hour", "Node"), by.y=c("date", "timepoint", "zone"), all=T, sort=F)
  
  # calculated generation-weighted average across zones for PJM-wide LMP
  PJM_LMP <- ddply(modelLMP, ~ case + date + hour, summarise, 
                   LMP = sum(gross_load * LMP / sum(gross_load)), 
                   gross_load = sum(gross_load))
  PJM_LMP$Node <- "PJM"
  PJM_LMP <- PJM_LMP[,c("date", "hour", "Node", "LMP", "case", "gross_load")]
  modelLMP <- rbind(modelLMP, PJM_LMP)
  modelLMP <- subset(modelLMP, select=-gross_load)
  
  # harmonize zone names and formats for merge
  modelLMP$Node <- mapvalues(modelLMP$Node, from=c("DC_BGE_PEP", "PA_METED_PPL"), to=c("BGE", "PPL"))
  
  # shift hour window and drop any timepoints outside of 24 hour window
  modelLMP$hour <- modelLMP$hour - 1
  modelLMP <- modelLMP[modelLMP$hour < 24,]
  
  # load and format PJM LMPs
  reportedLMPsub <- readPJM_LMPs(dates)
  reportedLMPsub$case <- "reported"
  reportedLMPsub <- reportedLMPsub[, c("date", "hour", "Price.Node.Name", "Price...MWh", "case")]
  colnames(reportedLMPsub) <- c("date", "hour", "Node", "LMP", "case")
  reportedLMPsub$Node <- mapvalues(reportedLMPsub$Node, 
                                   from=c("PJM-RTO ZONE", "DOMINION HUB", "EASTERN HUB", "WESTERN HUB"), 
                                   to=c("PJM", "VA_DOM", "EAST", "WEST"))
  
  # merge reported and modeled data
  fullLMP <- rbind(modelLMP, reportedLMPsub)
  
  
  fullLMP$datetime <- with(fullLMP, paste(date, hour))
  fullLMP$datetime <- as.POSIXct(fullLMP$datetime, format = "%Y-%m-%d %H")
  
  # plot
  cases <- c("DynamicORDC","reported")
  
  fullLMP$casenode <- paste(fullLMP$case, fullLMP$node) 
  fullLMP$Node[fullLMP$Node=="PPL"] <- rep("PPL-METED",length(fullLMP$Node[fullLMP$Node=="PPL"]))
  fullLMP$Node[fullLMP$Node=="VA_DOM"] <- rep("DOM",length(fullLMP$Node[fullLMP$Node=="VA_DOM"]))
  fullLMP$Node[fullLMP$Node=="BGE"] <- rep("BGE-PEP",length(fullLMP$Node[fullLMP$Node=="BGE"]))
  
  #print(fullLMP)
  fullLMP$Node <- factor(fullLMP$Nod, levels=c("BGE-PEP",
                                               "EAST",
                                               "PPL-METED",
                                               "DOM",
                                               "WEST",
                                               "PJM"))
  
  LMPplot <- ggplot(data=fullLMP, aes(x=datetime, y=LMP, colour=Node, linetype=case)) + 
    facet_wrap(~Node) + geom_line(size=1.2) + theme_classic() + 
    xlab("") + ylab("LMP ($ per MWh)") +
    scale_linetype_manual(breaks = cases, values=c("dotted","solid"), 
                          labels=c("Model", "Reported")) +
    coord_cartesian(ylim=c(0, max(fullLMP$LMP)*1.05)) +
    guides(colour=guide_legend(title="Zone: ", nrow=2,override.aes = list(size = 5)),
           linetype=guide_legend(title="", nrow=2))+
    theme(legend.text = element_text(size=24),
          legend.title=element_text(size=20),
          legend.position = "bottom",
          axis.title.y = element_text(size=24),
          axis.text.x= element_text(size=14,angle=45,vjust=.7),
          axis.text.y= element_text(size=16),
          strip.text.x = element_text(size = 18),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks=element_line(size=2),
          plot.margin=unit(c(.1,.5,0,.1),"cm"))
  
  if(!is.na(truncate_value) & max(fullLMP$LMP) > truncate_value){
    print("passed.")
    LMPplot <- LMPplot + coord_cartesian(ylim = c(0,truncate_value))
  } 
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("LMPs ", plotTitle, ".png"), width=12, height=6)
  return(LMPplot)
}
p1<-plotLMPsZonelabel(resultszones1,dates1, plotTitle="zones Jan 4-10 2014")
p2<-plotLMPsZonelabel(resultszones2,dates2, plotTitle="zones Oct 19-25 2017")

g1<-plot_grid(p1 + theme(legend.position="none"),
              p2+theme(legend.position="none"),
              align = 'vh',
              labels = c("A", "B"),
              label_size=20,
              hjust=-1,
              rel_widths=c(1,1))
legend <- get_legend(p1+ theme(legend.box.margin = margin(0, 0, 2, 0)))
mygrid <- plot_grid(g1, legend, rel_heights = c(1, .13),nrow=2)
setwd(paste(baseWD, "post_processing", "figures", sep="/"))
ggsave(paste("LMPZonecombo3", ".png", sep=""),dpi=500,units="in", width=12, height=6)


## Reserve pricing ####
summarizeReserves <- function(results, dates, all=F){
  
  reserves <- aggregateCaseData(results, "reserves")
  ordc <- aggregateCaseData(results, "ordc")
  
  # formatting for reserve results
  reserves$segments <- as.numeric(gsub(",.+", "", reserves$X))
  reserves$hour <- as.numeric(gsub(".+,", "", reserves$X))
  reserves$X <- NULL
  
  colnames(ordc)[colnames(ordc) == "timepoint"] <- "hour"
  
  reserves <- merge(reserves, ordc, all=T, by=c("case", "date", "hour","segments")) 
  reserves <- reserves[with(reserves, order(case, date, hour, segments)),]
  
  # subset to 24 hours
  reserves$hour <- reserves$hour - 1
  reserves <- reserves[reserves$hour < 24,]
  
  # find clearing price
  reserves <- ddply(reserves, ~ case + date + hour, transform, 
                    cumulativeReserve = cumsum(MW),
                    cumulativeProcured = cumsum(MW.on.reserve.segment),
                    procured = sum(MW.on.reserve.segment))
  
  reserves$priceFlag <- ifelse(reserves$procured > reserves$cumulativeReserve | reserves$MW.on.reserve.segment == 0, F, T)
  
  reserves$datetime <- with(reserves, paste(date, hour))
  reserves$datetime <- as.POSIXct(reserves$datetime, format = "%Y-%m-%d %H")
  
  procured <- reserves[reserves$priceFlag,]
  procured <- subset(procured, select=-c(cumulativeProcured, cumulativeReserve, priceFlag))
  colnames(procured)[colnames(procured) == "segments"] <- "lastSegment" 
  colnames(procured)[colnames(procured) == "MW"] <- "segmentLengthMW"
  colnames(procured)[colnames(procured) == "procured"] <- "totalReservesProcured"
  
  #luke added 8.14.2019
  procured$Price[procured$Price==850] <- 0
  
  # if specified, return all segments otherwise just return last segment on ORDC for each hour
  if(all){
    return(reserves)
  } else {
    return(procured)
  }
  
}


## Total reserve and congestion costs ####

calcCongestionCosts <- function(results, dates){
  
  zonalLoad <- aggregateCaseData(results, "zonalLoad")
  txFlows <- aggregateCaseData(results, "txFlows")

  # drop lines with zero flows (not actually an active line)
  txFlows <- txFlows[txFlows$flow..MW. != 0,]
  
  # separate transmission info
  txFlows$from <- gsub("_to.+", "", txFlows$X)
  txFlows$to <- gsub(".+to_", "", txFlows$X)
  txFlows$to <- gsub("-.+", "", txFlows$to)
  txFlows$hour <- as.numeric(gsub(".+-", "", txFlows$X))
  
  txFlows$X <- NULL
  
  # drop extra hours
  txFlows <- txFlows[txFlows$hour <= 24,]
  
  # identify zone that is receiving transmission
  txFlows$zone <- as.factor(ifelse(txFlows$flow..MW. < 0, txFlows$from, txFlows$to))
  
  # match load for receiving zone by hour
  zonalLoadSub <- zonalLoad[,c("case", "date", "timepoint", "zone", "gross_load")]
  colnames(zonalLoadSub)[colnames(zonalLoadSub) == "timepoint"] <- "hour"
  
  txFlows <- merge(txFlows, zonalLoadSub, by=c("case", "date", "hour", "zone"), all.x=T)
  txFlows$hour <- txFlows$hour - 1
  txFlows$datetime <- as.POSIXct(with(txFlows, paste(date, hour)), format = "%Y-%m-%d %H")
  
  # cost of congestion: price x load in receiving zone (using absolute value of congestion price)
  txFlows$congestionCost <- abs(txFlows$congestion.price....MW.) * txFlows$gross_load
  congestion <- ddply(txFlows, ~ case + datetime, summarise, costs = sum(congestionCost))
  
  return(congestion) 
}

plotWeeklyReserveCosts <- function(results, dates, plotTitle){
  
  procuredReserves <- summarizeReserves(results, dates)
  
  # calclate reserve costs
  # mutiply by 6 because synch reserves are 10 minutes and should be reported hourly for comparison
  procuredReserves$costs <- 6*procuredReserves$Price * procuredReserves$totalReservesProcured
  procuredReserves <- procuredReserves[c("case", "datetime", "costs")]
  procuredReserves$costs[procuredReserves$case=="PJMHistorical"] <- 0 #reset base case reserve costs to 0

  # read in congestion costs
  congestionCosts <- calcCongestionCosts(results, dates)
  #read in LMPs
  modelLMP <- aggregateCaseData(results, "modelLMP")
  
  # zonalLoad is the same across cases (model input)
  zonalLoad <- results[[1]][["zonalLoad"]]
  
  #formatting of model LMP
  colnames(modelLMP)[1] <- "Node"
  
  # add in gross load
  modelLMP <- merge(modelLMP, zonalLoad[,c("date", "timepoint", "zone", "gross_load")], 
                    by.x=c("date", "hour", "Node"), by.y=c("date", "timepoint", "zone"), all=T, sort=F)
  
  # calculated generation-weighted average across zones for PJM-wide LMP
  PJM_LMP <- ddply(modelLMP, ~ case + date + hour, summarise, 
                   LMP = sum(gross_load * LMP / sum(gross_load)), 
                   gross_load = sum(gross_load))
  PJM_LMP$Node <- "PJM"
  PJM_LMP <- PJM_LMP[,c("date", "hour", "Node", "LMP", "case", "gross_load")]
  PJM_LMP$costs <- PJM_LMP$LMP*PJM_LMP$gross_load
  PJM_LMP_graph <- PJM_LMP[,c("case","date","costs")]
  colnames(PJM_LMP_graph) <- c("case","datetime","costs")
  
  procuredReserves$costType <- "Primary Synchronized Reserves"
  congestionCosts$costType <- "Congestion"
  PJM_LMP_graph$costType <- "Energy"
  
  totalCosts <- rbind(procuredReserves, congestionCosts, PJM_LMP_graph)
  
  totalCostsSum <- ddply(totalCosts, ~ case + costType, summarise, costs=sum(costs))
  
  # set certain case reserves to 0 
  newRows <- data.frame(case=c("NoReserves","PJMHistorical"), costType="Primary Synchronized Reserves", costs=0)
  
  totalCostsSum <- rbind(totalCostsSum, newRows)
  
  used_colors = c("lightgrey","firebrick","gold")
  
  #mutate(name = factor(name, levels=c("north", "north-east", "east",
  totalCostsSum$case <- factor(totalCostsSum$case, levels=c("DynamicORDC",
                                                                    "SimpleORDC",
                                                                    "PJMHistorical",
                                                                    "NoReserves"))
  g<-ggplot(totalCostsSum, aes(x=case, y=costs/1E6, fill=costType)) + 
    geom_bar(stat="identity", position=position_dodge(), colour="black", alpha=0.75) + coord_flip()+
    scale_fill_manual(values=used_colors) +
    theme_classic() + xlab("") + ylab("Million $") + guides(fill=guide_legend(title="",override.aes = list(size=14))) + 
    theme(legend.text = element_text(size=20),
          legend.title=element_text(size=24),
          legend.position="top",
          axis.title.y = element_text(size=32),
          axis.title.x = element_text(size=32),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20),
          strip.text.x = element_text(size = 20),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2))
  g1<-plot_grid(g + theme(legend.position="none"))
  legend <- get_legend(g + theme(legend.box.margin = margin(0, 0, 0, 12)))
  mygrid <- plot_grid(legend, g1, rel_heights = c(.1, 1),nrow=2)
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste("Reserve costs ", plotTitle, ".png", sep=""),dpi=500,units="in", width=12, height=6)
  return(g)
}

#test
p1<-plotWeeklyReserveCosts(results2, dates2, plotTitle="Oct 19-25 2017")


## Reserves vs. price over time period ####
plotReserves <- function(results, dates, plotTitle){
  
  reserveSegments <- summarizeReserves(results, dates, all=T)
  procured <- reserveSegments[reserveSegments$priceFlag!=0,]
  
  #zero out procured in PJM Historical case when sufficient procurement is done
  #print(procured[procured$case=="PJMHistorical",])
  procured$Price[(procured$MW.on.reserve.segment>189) & (procured$MW.on.reserve.segment<191) & (procured$case=="PJMHistorical")] <- 0
  #print(procured[procured$case=="PJMHistorical",])
  #procured$Price[procured$Price==850] <- 0
  #procured$Price[procured$Price==300] <- 0
  
  # scale for secondary price axis
  scale <- max(procured$Price) / max(reserveSegments$cumulativeProcured) 
  
  procured$case <- factor(procured$case, levels=c("NoReserves",
                                                            "PJMHistorical",
                                                            "SimpleORDC",
                                                            "DynamicORDC"))
  reserveSegments$case <- factor(reserveSegments$case, levels=c("NoReserves",
                                                  "PJMHistorical",
                                                  "SimpleORDC",
                                                  "DynamicORDC"))

  g1<-ggplot(reserveSegments, aes(x=datetime, y=MW.on.reserve.segment, fill=segments)) + geom_bar(stat='identity', size=0) +
    geom_line(data=procured, aes(x=datetime, y=Price/scale, linetype="Penalty Factor\n(Secondary Axis)"), colour='red',size=1.2) +
    facet_wrap(~case) +
    scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Primary Synchronized Reserve\nPenalty Factor ($ per MW)")) + 
    coord_cartesian(ylim=c(0, max(reserveSegments$cumulativeProcured)*1.05)) +
    xlab("") + ylab("Primary Synchronized\nReserves procured (MW)") + 
    guides(fill=guide_legend(title="ORDC\nsegment"),
           shape=guide_legend(), linetype=guide_legend(title="")) +
    scale_fill_gradient(breaks=rev(c(1:10)),low = "black", high = "yellow") +
    theme_classic() +
    theme(legend.text = element_text(size=20),
          legend.title=element_text(size=24),
          legend.position="bottom",
          axis.title.y = element_text(size=20),
          axis.text.x= element_text(size=16),
          axis.text.y= element_text(size=18),
          strip.text.x = element_text(size=24),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("Reserves ", plotTitle, ".png"),dpi=500,units="in", width=12, height=6)
  
  #reserve procurement distribution (including slack reserves)
  mycolors <- c("#FDAA83","#7498C7","#FEE8AC")
  
  g2<-ggplot(procured, aes(x=case, y=procured, fill=case)) + 
    geom_boxplot()+ xlab("") + ylab("Megawatts (MW)") +
    scale_fill_manual(values=mycolors)+
    theme_classic() +
    ggtitle("Procured Primary Synchronized Reserve Distribution")+
    guides(fill=guide_legend(title="", nrow=1, byrow=TRUE))+
    theme(legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position="top",
          plot.title = element_text(size=16,face="bold",hjust = 0.5),
          axis.title.y = element_text(size=22),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=20),
          axis.ticks.x = element_blank(),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("ReservesDistribution ", plotTitle, ".png"),dpi=500,units="in", width=12, height=6)
  
  g3<-ggplot(procured, aes(x=case, y=Price, fill=case)) + 
    geom_boxplot()+ xlab("") + ylab("$/MWh") +
    theme_classic() +
    scale_fill_manual(values=mycolors)+
    ggtitle("Reserve Penalty Factor Distribution") +
    ylim(c(0,200))+
    guides(fill=guide_legend(title="", nrow=1, byrow=TRUE))+
    theme(legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position="top",
          plot.title = element_text(size=16,face="bold",hjust = 0.5),
          axis.title.y = element_text(size=22),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=20),
          axis.ticks.x = element_blank(),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("PenaltyFactorDistribution ", plotTitle, ".png"),dpi=500,units="in", width=12, height=6)
  return(list(g1,g2,g3))
}
JanReserveFigs <- plotReserves(results1,dates2, plotTitle="Jan 4-10 2014")
OctReserveFigs <- plotReserves(results2, dates2, plotTitle="Oct 19-25 2017")
JanReserveFigs <- OctReserveFigs

plots <- align_plots(JanReserveFigs[[1]], JanReserveFigs[[2]], align = 'v', axis = 'l')
bottom_row <- plot_grid(JanReserveFigs[[2]]+theme(legend.position="none"),
                        JanReserveFigs[[3]]+theme(legend.position="none"),
                        labels = c('B', 'C'), label_size = 20,
                        rel_widths=c(1,.8))
g1<-plot_grid(plots[[1]],
              bottom_row,
              labels = c("A", ""),
              label_size=20,
              hjust=-1,
              rel_heights=c(1,.6),
              nrow=2)
g1
legend <- get_legend(JanReserveFigs[[2]]+ theme(legend.box.margin = margin(0, 0, 0, 12)))
mygrid <- plot_grid(g1,legend, rel_heights = c(1, .05),nrow=2)
setwd(paste(baseWD, "post_processing", "figures", sep="/"))
ggsave(paste("OctReservesFigylim", ".png", sep=""),dpi=500,units="in", width=12, height=12)

## Generation dispatch ####

plotDispatch <- function(results, dates, case, plotTitle, hours=24){

  dispatch <- results[[case]][["dispatch"]]
  gens <- results[[case]][["gens"]]
  VRE <- results[[case]][["VRE"]]
  
  # subset dispatch output to single day (include columns for date and case as well)
  dispatch <- dispatch[, c(1:(hours+1), dim(dispatch)[2])]
  
  colnames(dispatch) <- c("id", 0:(hours-1), "date")
  dispatch$zone <- gsub("-[[:print:]]*", "", dispatch[,1])
  dispatch$plant <- gsub("[[:print:]]*-", "", dispatch[,1])
  dispatch[,"id"] <- NULL
  
  dispatch <- melt(dispatch, id.vars=c("date", "zone", "plant"))
  
  colnames(dispatch) <- c("date", "zone", "plant", "hour", "MW")
  
  # drop rows with zero generation
  #dispatch <- dispatch[dispatch$MW != 0,]
  # match with fuel type
  dispatch <- merge(dispatch, gens[,c("Name", "Category")], by.x="plant", by.y="Name", all.x=T)
  
  #drop duplicated entries in output
  dispatch <- dispatch[!duplicated(dispatch), ]

  # summarize by fuel type
  fuelDispatch <- ddply(dispatch, ~ date + hour + zone + Category, summarise, MW = sum(MW))
  fuelDispatch$zone <- factor(fuelDispatch$zone)
  
  # add in renewable gen. and curtailment
  VRE <- melt(VRE, id.vars = c("date", "timepoint", "zone"))
  colnames(VRE) <- c("date", "hour", "zone", "Category", "MW")
  VRE$hour <- factor(VRE$hour - 1)
  
  fuelDispatch <- rbind(fuelDispatch, VRE)
  fuelDispatch$datetime <- as.POSIXct(with(fuelDispatch, paste(date, hour)), format = "%Y-%m-%d %H")
  
  
  fuelDispatch$Category <- factor(fuelDispatch$Category, levels = c("curtailment", "DR", "wind", "solar", "DS", 
                                                                    "CT", "CC", "HD", "ST1", "ST2", "NU", NA))
  #create color panel for later use
  dispatchcolors <- c("red","gray90","cyan","yellow","green",
                      "brown","orange","blue","gray50","black","purple")
  
  #drop NA's
  fuelDispatch <- fuelDispatch[!is.na(fuelDispatch$Category),]
  
  #fuelDispatch$Category <- mapvalues()
  # calculate PJM wide
  PJM_dispatch <- ddply(fuelDispatch, ~ datetime + Category, summarise, MW = sum(MW))
  PJM_dispatch$zone <- "PJM"
  
  fuelDispatch <- fuelDispatch[,c("datetime", "Category", "MW", "zone")]
  fuelDispatch <- rbind(fuelDispatch, PJM_dispatch)
  
  
  curtailedPower <- fuelDispatch[fuelDispatch$Category == "curtailment" & !is.na(fuelDispatch$Category) & fuelDispatch$MW > 0,]
  
  #fuelDispatch 
  fuelDispatch$Category <- droplevels(fuelDispatch$Category)
  
  #Brian's plotting code (defunct)
  #ggplot(data=fuelDispatch, aes(x=datetime, y=MW/1E3, fill=Category)) + geom_area() + facet_wrap(~zone, nrow=3, scales = "free") + 
  #  theme_classic() + ylab("GW") + guides(fill=guide_legend(title="")) + xlab("") +
  #  scale_x_datetime() +
  #  ggtitle(paste("Generation by fuel for", plotTitle))
  
  #print(fuelDispatch)
  #print(fuelDispatch$zone[fuelDispatch$zone=="PA_METED_PPL"])
  #print(fuelDispatch$zone[fuelDispatch$zone=="VA_DOM"])
  #fuelDispatch$zone[fuelDispatch$zone=="PA_METED_PPL"] <- "PPL-METED"#rep("PPL-METED",length(fuelDispatch$zone[fuelDispatch$zone=="PA_METED_PPL"]))
  #fuelDispatch$zone[fuelDispatch$zone=="VA_DOM"] <- "DOM"#rep("DOM",length(fuelDispatch$zone[fuelDispatch$zone=="VA_DOM"]))
  #fuelDispatch$zone[fuelDispatch$zone=="DC_BGE_PEP"] <- "BGE-PEP"#rep("BGE-PEP",length(fuelDispatch$zone[fuelDispatch$zone=="DC_BGE_PEP"]))
  
  fuelDispatch$zone <- as.character(fuelDispatch$zone)
  fuelDispatch$zone[fuelDispatch$zone=="PA_METED_PPL"]<-"PPL-METED"
  fuelDispatch$zone[fuelDispatch$zone=="VA_DOM"]<-"DOM"
  fuelDispatch$zone[fuelDispatch$zone=="DC_BGE_PEP"]<-"BGE-PEP"
  
  #capitalize solar, wind, curtailment
  print(fuelDispatch$Category)
  capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
  }
  fuelDispatch$Category <- capFirst(fuelDispatch$Category)
  print(fuelDispatch$Category)

  #print(fuelDispatch$zone[fuelDispatch$zone=="DOM"])
  #print(fullLMP)
  #print(fuelDispatch)
  fuelDispatch$zone <- factor(fuelDispatch$zone, levels=c("BGE-PEP",
                                               "EAST",
                                               "PPL-METED",
                                               "DOM",
                                               "WEST",
                                               "PJM"))
  
  fuelDispatch$Category <- factor(fuelDispatch$Category,levels=c("Curtailment","DR","Wind","Solar",
                                                                 "DS","CT","CC","HD","ST1","ST2","NU"))
  
  #Luke's plotting code (active)
  DispatchPlot <- ggplot(data=fuelDispatch, aes(x=datetime, y=MW/1E3, fill=Category)) + geom_area() + facet_wrap(~zone, nrow=3, scales = "free") + 
    theme_classic() + ylab("Gigawatts (GW)") + guides(fill=guide_legend(title="", nrow=2, byrow=TRUE)) + xlab("") +
    scale_x_datetime() + scale_fill_manual(values=dispatchcolors) +
    theme(legend.text = element_text(size=20),
          legend.position = "bottom",
          axis.title.y = element_text(size=24),
          axis.text.x= element_text(size=12),
          axis.text.y= element_text(size=16),
          strip.text.x = element_text(size = 22),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))
  #plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
  #+ggtitle(paste("Generation by fuel for", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("dispatch ", plotTitle, "-", case, ".png"),dpi=500,units="in", width=12, height=6)
  return(DispatchPlot)
}
p1 <- plotDispatch(results1, dates1, case="NoReserves", plotTitle="No Reserves Jan 4-10 2014")
p2 <- plotDispatch(results2, dates2, case="DynamicORDC", plotTitle="Oct 19-25 2017")

g1<-plot_grid(p1 + theme(legend.position="none"),
              p2+theme(legend.position="none"),
              align = 'vh',
              labels = c("A", "B"),
              label_size=20,
              hjust=-1,
              rel_widths=c(1,1))
legend <- get_legend(p1+ theme(legend.box.margin = margin(0, 0, 0, 12)))
mygrid <- plot_grid(legend, g1, rel_heights = c(.15, 1),nrow=2)
setwd(paste(baseWD, "post_processing", "figures", sep="/"))
ggsave(paste("Dispatchcombo3_update", ".png", sep=""),dpi=500,units="in", width=12, height=8)


## Price deltas ####

# function to calculate zonal differences 
calcZonalDifference <- function(results, dates, case, zoneMapping){
  
  # load prices 
  actualPrices <- readPJM_LMPs(dates) 
  modelPrices <- results[[case]][["modelLMP"]]

  # actual prices 
  actualPrices <- actualPrices[,c("Price.Node.Name" ,"datetime", "Price...MWh")]
  colnames(actualPrices) <- c("node", "datetime", "price")
  
  actualPrices$node <- mapvalues(actualPrices$node, 
                                 from=c("DOMINION HUB", "EASTERN HUB", "WESTERN HUB", "BGE", "PPL"), 
                                 to=c("VA_DOM", "EAST", "WEST", "DC_BGE_PEP", "PA_METED_PPL"))
  
  # drop PJM wide price
  actualPrices <- actualPrices[actualPrices$node != "PJM-RTO ZONE",]
  
  mapResults <- merge(zoneMapping, actualPrices, by.x="from", by.y="node", all.x=T)
  mapResults <- merge(mapResults, actualPrices, by.x=c("to", "datetime"), by.y=c("node", "datetime"), all.x=T)
  
  # modeled prices 
  modelPrices$hour <- modelPrices$hour - 1   # hours given in 1-24 format, so need to offset to match PJM reported data
  modelPrices <- modelPrices[modelPrices$hour < 24,] # remove extra hours in each (inlcuded to smooth transition between days)
  
  modelPrices$datetime <- as.POSIXct(paste(modelPrices$date, modelPrices$hour), format = "%Y-%m-%d %H")
  colnames(modelPrices)[1] <- "node"
  modelPrices <- modelPrices[,c("node", "datetime", "LMP")]
  
  mapResults <- merge(mapResults, modelPrices, by.x=c("from", "datetime"), by.y=c("node", "datetime"), all.x=T)
  mapResults <- merge(mapResults, modelPrices, by.x=c("to", "datetime"), by.y=c("node", "datetime"), all.x=T)
  
  mapResults <- mapResults[with(mapResults, order(datetime, from, to)),]
  mapResults <- mapResults[,c("datetime", "from", "to", "price.x", "price.y", "LMP.x", "LMP.y")]
  colnames(mapResults) <- c("datetime", "from", "to", "price_actual_from", "price_actual_to", "price_model_from", "price_model_to")
  
  # calculate deltas (from - to)
  mapResults$delta_actual <- mapResults$price_actual_from - mapResults$price_actual_to
  mapResults$delta_model <- mapResults$price_model_from - mapResults$price_model_to
  
  return(mapResults)
}


# function to plot price deltas
plotDeltas <- function(results, dates, txMapping, case, plotTitle){
  
  priceDeltas <- calcZonalDifference(results, dates, case, txMapping)
  
  priceDeltas <- priceDeltas[,c("datetime", "from", "to", "delta_actual", "delta_model")]
  
  priceDeltas <- melt(priceDeltas, id.vars = c("datetime", "from", "to"))
  priceDeltas$interface <- with(priceDeltas, paste(from, "to", to))
  
  ggplot(priceDeltas, aes(x=datetime, y=value, linetype=variable, color=variable)) + geom_line() + 
    facet_wrap(~interface) + xlab("") + ylab("Price differntial ($ per MWh)") +
    guides(linetype=guide_legend(title=""), color=guide_legend(title="")) + #theme_classic() +
    scale_color_manual(breaks = c("delta_actual", "delta_model"), values=c("blue", "red"), labels = c("actual", "model")) +
    scale_linetype_manual(breaks = c("delta_actual", "delta_model"), values=c(1,2), labels = c("actual", "model")) +
    theme(legend.position = "top")
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste("Price deltas", plotTitle, "-", case, ".png"), width=8, height=6)
}

CompareObjectives <- function(results, dates, plotTitle, refcase="DynamicORDC"){
  ObjectiveDF <- aggregateCaseData(results,"Objective")
  CompareDF <- ObjectiveDF[(ObjectiveDF$X=="ComparisonObjectiveValue") &(ObjectiveDF$case!="NoReserves"),]
  CompareDF$Delta = rep(CompareDF$X0[CompareDF$case==refcase],length(unique(CompareDF$case)))-CompareDF$X0
  #print(CompareDF)
  sumCompareDF <- CompareDF %>% 
    group_by(case) %>% 
    summarise(Delta = sum(Delta))
  print(sumCompareDF)
  dispatchcolors <-c("#FDAA83","#7498C7","#FEE8AC")
  sumCompareDF$case <- factor(sumCompareDF$case, levels=c("PJMHistorical",
                                                          "SimpleORDC",
                                                          "DynamicORDC"))
  
  g <- ggplot(sumCompareDF, aes(x=case, y=Delta/1E6, fill=case)) + 
    geom_bar(stat="identity", position=position_dodge(), colour="black", alpha=1) +
    scale_fill_manual(values=dispatchcolors)+
    theme_classic() + xlab("") + ylab("Change in Social Welfare ($M)") + guides(fill=guide_legend(title="",override.aes = list(size=14))) + 
    theme(legend.text = element_text(size=24),
          legend.title=element_text(size=24),
          legend.position="top",
          axis.title.y = element_text(size=22),
          axis.title.x = element_blank(),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=20),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2),
          axis.ticks.x=element_blank())
  return(g)
}


LOLPTimeSeries <- function(results,dates, plotTitle, refcase="DynamicORDC"){
  reserves <- aggregateCaseData(results, "reserves")
  ordc <- aggregateCaseData(results, "ordc")
  
  # formatting for reserve results
  reserves$segments <- as.numeric(gsub(",.+", "", reserves$X))
  reserves$hour <- as.numeric(gsub(".+,", "", reserves$X))
  reserves$X <- NULL
  
  colnames(ordc)[colnames(ordc) == "timepoint"] <- "hour"
  
  reserves <- merge(reserves, ordc, all=T, by=c("case", "date", "hour","segments")) 
  reserves <- reserves[with(reserves, order(case, date, hour, segments)),]
  
  # subset to 24 hours
  reserves$hour <- reserves$hour - 1
  reserves <- reserves[reserves$hour < 24,]
  
  # find clearing price
  reserves <- ddply(reserves, ~ case + date + hour, transform, 
                    cumulativeReserve = cumsum(MW),
                    cumulativeProcured = cumsum(MW.on.reserve.segment),
                    procured = sum(MW.on.reserve.segment))
  
  reserves$priceFlag <- ifelse(reserves$procured > reserves$cumulativeReserve | reserves$MW.on.reserve.segment == 0, F, T)
  
  #add repetition of DynamicORDC seg for comparison
  reserves$CompareCumulativeReserve <- rep(reserves$cumulativeReserve[reserves$case==refcase],length(unique(reserves$case)))
  reserves$ComparePrice<- rep(reserves$Price[reserves$case==refcase],length(unique(reserves$case)))
  reserves$comparepriceFlag <- ifelse(reserves$procured>reserves$CompareCumulativeReserve, F, T)
  reserves$compareLOLP <- reserves$ComparePrice*reserves$comparepriceFlag/2000
  
  reserves$datetime <- with(reserves, paste(date, hour))
  reserves$datetime <- as.POSIXct(reserves$datetime, format = "%Y-%m-%d %H")
  LOLP <- reserves %>% group_by(case,datetime) %>% summarise(LOLP = max(compareLOLP))
  LOLPdropNR <- LOLP[LOLP$case!="NoReserves",]
  print(LOLPdropNR)
  
  #dispatchcolors <-c("darkorange","dodgerblue","yellow")
  dispatchcolors <-c("#FDAA83","#7498C7","#FEE8AC")
  LOLPdropNR$case <- factor(LOLPdropNR$case, levels=c("PJMHistorical",
                                                          "SimpleORDC",
                                                          "DynamicORDC"))
  
  g <- ggplot(LOLPdropNR, aes(x=datetime, y=LOLP, colour=case)) + geom_line(size=2) +
    xlab("") + ylab("Reserve Shortage Probability") + ylim(c(0,1))+
    theme_classic() + scale_colour_manual(values=dispatchcolors)+
    theme(legend.text = element_text(size=24),
          legend.title=element_text(size=24),
          legend.position="top",
          axis.title.y = element_text(size=22),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20),
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks =element_line(size=2))
  #print(LOLP)
  #write.csv(LOLP,"checklolp.csv")

  
  #print(procured)
  return(g)
}

p1 <- CompareObjectives(results1, dates1, "JanObjectives")
p2 <- LOLPTimeSeries(results1, dates1, "JanLOLP")
p3 <- CompareObjectives(results2, dates2, "OctObjectives")
p4 <- LOLPTimeSeries(results2, dates2, "OctLOLP")

g1<-plot_grid(p2 + theme(legend.position="none"),
              p1+theme(legend.position="none"),
              p4 + theme(legend.position="none"),
              p3+theme(legend.position="none"),
              align = 'vh',
              labels = c("A","B","C","D"),
              label_size=20,
              hjust=-1,
              rel_widths=c(1,.3),
              rel_heights=c(1,1),
              nrow=2,
              ncol=2)
legend <- get_legend(p1+ theme(legend.box.margin = margin(0, 0, 0, 12)))
mygrid <- plot_grid(legend, g1, rel_heights = c(.1, 1),nrow=2)
setwd(paste(baseWD, "post_processing", "figures", sep="/"))
ggsave(paste("LOLP", ".png", sep=""),dpi=500,units="in", width=12, height=12)

#combine above plot types into a single plot


## Main ####
dates1 <- seq(as.POSIXct("1/4/2014", format = "%m/%d/%Y"), by="day", length.out=7)
dates2 <- seq(as.POSIXct("10/19/2017", format = "%m/%d/%Y"), by="day", length.out=7)
alldates <- seq(as.POSIXct("10/1/2017", format = "%m/%d/%Y"), by="day", length.out=31)
shortdates1 <- seq(as.POSIXct("1/4/2014", format = "%m/%d/%Y"), by="day", length.out=1)
shortdates2 <- seq(as.POSIXct("1/4/2014", format = "%m/%d/%Y"), by="day", length.out=6)

# cases under consideration
cases <- c("DynamicORDC","PJMHistorical",
           "NoReserves","SimpleORDC")

cases2 <- c("DynamicORDC","PJMHistorical",
            "NoReserves","SimpleORDC")
smallcases <- c("PJMHistorical",'ftrialing_st1switch')
caseszones1 <- c("DynamicORDC")
caseszones2 <- c("DynamicORDC")

casehist <- c("PJMHistorical")

casestest <- c("base","base_30pcttx","base_dieselinterrupt_wmax","base_extradieselinterrupt_wmax") #"base",
casestest <- c("base")
#,"PJMHistoricalUpdateReserves",
#cases2 <- c("PJMHistorical","PJMHist_nohurdle","fullORDC")
# load data
results1 <- loadAllCases(dates1, cases=cases)
results2 <- loadAllCases(dates2, cases=cases2)
results3 <- loadAllCases(dates2,cases=cases)
resultsOct <- loadAllCases(alldates,casehist)
#results3 <- loadAllCases(dates1, cases=cases)
#results4 <- loadAllCases(shortdates2, cases=casestest)
results2$DynamicORDC$Objective

resultszones1 <- loadAllCases(dates1, cases=caseszones1)
resultszones2 <- loadAllCases(dates2, cases=caseszones2)

plotLMPs(results1, shortdates2, plotTitle="Jan 4-10 2014 single zone")
plotLMPs(results2, dates2, plotTitle="Oct 19-25 2017 RT",truncate_value = NA,PJM_only=TRUE)


plotLMPs(results3, dates2, plotTitle='Oct DynORDC compare')

plotLMPs(results1, shortdates2, plotTitle='Jan 4 2014 comparison3',truncate_value=500)

plotLMPsZonelabel(resultszones1,dates1, plotTitle="zones Jan 4-10 2014")
plotLMPsZonelabel(resultszones2,dates2, plotTitle="zones Oct 19-25 2017")

plotWeeklyReserveCosts(results1, dates1, plotTitle="Jan 4-10 2014")
plotWeeklyReserveCosts(results2, dates2, plotTitle="Oct 19-25 2017")
#plotWeeklyReserveCosts(results2, dates2, plotTitle="Jan 4-10 2014")

plotReserves(results1, dates1, plotTitle="Jan 4-10 2014")
plotReserves(results2, dates2, plotTitle="Oct 19-25 2017")

plotDispatch(results1, dates1, case="PJMHistorical", plotTitle="Jan 4-10 2014")
plotDispatch(results2, dates2, case="PJMHistorical", plotTitle="Oct 19-25 2017")

###added by luke - can use to make table of multiple cases
janplotlmp <- plotLMPs(results1, dates1, plotTitle="Jan 4-10 2014")
octplotlmp <- plotLMPs(results2, dates2, plotTitle="Oct 19-25 2017")
janplotlmp <- plotLMPs(resultsOct, alldates, plotTitle='Oct2017')
colnames(octplotlmp) <- c("octLMP","octcase","octdatetime","octdeltaLMP")
plotfullLMP <- cbind(janplotlmp,octplotlmp)
plotfullLMP <- janplotlmp
filter(plotfullLMP,"2014-07-01" %in% datetime)
table1::label(plotfullLMP$LMP) <- "January 2014 LMP ($/MWh)"
table1::label(plotfullLMP$deltaLMP) <- "January 2014 RMSE v. Reported ($/MWh)"
table1::label(plotfullLMP$octLMP) <- "October 2017 LMP ($/MWh)"
table1::label(plotfullLMP$octdeltaLMP) <- "October 2017 RMSE v. Reported ($/MWh)"
table1::table1(~LMP+deltaLMP | case, 
               data = plotfullLMP,topclass="Rtable1-zebra", output="latex")



df1 <- results1$fullORDC
df1tx <- df1$txFlows

# call price delta functions

# Transmission linkages
# EAST_to_PA_METED_PPL
# WEST_to_DC_BGE_PEP
# WEST_to_VA_DOM
# WEST_to_PA_METED_PPL
# DC_BGE_PEP_to_VA_DOM
# DC_BGE_PEP_to_PA_METED_PPL

txMapping <- data.frame(from=c("EAST", "WEST", "WEST", "WEST", "DC_BGE_PEP", "DC_BGE_PEP"),
                        to=c("PA_METED_PPL", "DC_BGE_PEP", "VA_DOM", "PA_METED_PPL", "VA_DOM", "PA_METED_PPL"))  

plotDeltas(results1, dates1, txMapping, case="withORDC", "Jan 4-10 2014")
plotDeltas(results2, dates2, txMapping, case="PJMHistoricalwCoalSplit", "Oct 19-25 2017")
plotDeltas(results2, dates1, txMapping, case="withORDC","Jan 4-10 2014")

## Save ####
setwd(paste(baseWD, "post_processing", sep="/"))
save.image("Results.RData")


#some testing

getDispatch <- function(results, dates, case, hours=24){
  
  dispatch <- results[[case]][["dispatch"]]
  #gens <- results[[case]][["gens"]]
  #VRE <- results[[case]][["VRE"]]
  
  # subset dispatch output to single day (include columns for date and case as well)
  dispatch <- dispatch[, c(1:(hours+1), dim(dispatch)[2])]
  
  colnames(dispatch) <- c("id", 0:(hours-1), "date")
  dispatch$zone <- gsub("-[[:print:]]*", "", dispatch[,1])
  dispatch$plant <- gsub("[[:print:]]*-", "", dispatch[,1])
  dispatch[,"id"] <- NULL
  
  dispatch <- melt(dispatch, id.vars=c("date", "zone", "plant"))
  
  colnames(dispatch) <- c("date", "zone", "plant", "hour", "MW")
  
  # drop rows with zero generation
  #dispatch <- dispatch[dispatch$MW != 0,]
  # match with fuel type
  #dispatch <- merge(dispatch, gens[,c("Name", "Category")], by.x="plant", by.y="Name", all.x=T)
  
  #drop duplicated entries in output
  dispatch <- dispatch[!duplicated(dispatch), ]
  return(dispatch)
}
testdispatch <- getDispatch(results1,dates1,"DynamicORDC")
#sum across zones
dipatchdf <- testdispatch %>% group_by(plant,date,hour) %>% summarise(MW = sum(MW))
#reorder if desired
dipatchdf

#include available capacity by gen
#first grab capacity
cap_and_ramp <- merge(results1[["DynamicORDC"]][["rampdata"]],results1[["DynamicORDC"]][["GenCapacity"]][,c("Name","Capacity","Can_Spin","Fuel_Cost")],
                     by.x=c("Gen_Index"),by.y=c("Name"),all=T)
cap_and_ramp <- cap_and_ramp[!duplicated(cap_and_ramp), ]
#results1[["DynamicORDC"]][["GenCapacity"]]
capacitydf <- merge(dipatchdf,cap_and_ramp,by.x=c("plant"),by.y=c("Gen_Index"),all=T)
capacitydf <- capacitydf[!duplicated(capacitydf), ]
#then grab sch avail
schavail <- results1[["DynamicORDC"]][["ScheduledAvail"]]
schavail$timepoint <- schavail$timepoint-1
schavaildf <- merge(capacitydf,schavail,by.x=c("hour","plant","date"),by.y=c("timepoint","Gen_Index","date"),all=T)
schavaildf <- schavaildf[!duplicated(schavaildf), ]

#reserves
genreserves <- results1[["DynamicORDC"]][["GenReserves"]][,c("Gen_Index","timepoint","Committed","Total.Held.as.Primary.Synch.Reserves..MW.","date")]
genreserves$timepoint <- genreserves$timepoint-1
genreservesdf <- merge(schavaildf,genreserves,by.x=c("hour","plant","date"),by.y=c("timepoint","Gen_Index","date"),all=T)
genreservesdf <- genreservesdf[!duplicated(genreservesdf), ]
genreservesdf_commits <- genreservesdf[genreservesdf$Committed==1,]
#create fuel flag, then drop marginal costs
genreservesdf_commits
genreservesdf_commits$MCflag <- ifelse(genreservesdf_commits$Fuel_Cost > 0, T, F)
genreservesdf_commits<-genreservesdf_commits[ , !(names(genreservesdf_commits) %in% c("Fuel_Cost"))]
#then do a final drop duplicates
genreservesdf_commits <- genreservesdf_commits[!duplicated(genreservesdf_commits),]
#THEN write
write.csv(genreservesdf_commits,"gentest.csv")
#calc headroom
#

#create empirical distribution from PJM values
ggplot(data = data.frame(x = c(-1000, 2000)), aes(x)) +
  stat_function(fun = lnorm, n = 101, args = list(mean = 269, sd = 379.2)) + ylab("") +
  scale_y_continuous(breaks = NULL) + theme_bw()
stat_function(fun = dnorm, n = 101, args = list(mean = 269, sd = 379.2))

set.seed(0)
m <- 269.0
s <- 379.2
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))
print(paste("location:", location))
print(paste("shape:", shape))
draws3 <- rlnorm(n = 100000, location, shape)
#mean(draws3)
#sd(draws3)
#plot(density(draws3))
drawdf <- as.data.frame(draws3)
#colnames(drawdf) <- c("PJM Winter Block 3 (Lognormal fitted)")
drawdf$casename <- rep("PJM Winter Block 3",length(drawdf$draws3))

#load in and create empirical distribution from Sinnott's data
filename <- "PJM.UCM.SFU1U2U3D1D2D3.072018.csv"
setwd(paste(baseWD, "raw_data", sep="/"))
forcedoutagerawdf <- read.csv(filename)

PJM.UCM.SFU1U2U3D1D2D3.FINAL <- forcedoutagerawdf #create a copy

for (i in 1:ncol(PJM.UCM.SFU1U2U3D1D2D3.FINAL)){
  PJM.UCM.SFU1U2U3D1D2D3.FINAL[,i] <- c(NA, diff(PJM.UCM.SFU1U2U3D1D2D3.FINAL[,i], lag = 1, differences = 1))
} #outages as hourly deltas
PJM.UCM.SFU1U2U3D1D2D3.FINAL[PJM.UCM.SFU1U2U3D1D2D3.FINAL < 0] <- 0 # ignore recoveries
PJM.UCM <- as.data.frame(rowSums(PJM.UCM.SFU1U2U3D1D2D3.FINAL)) #sum all gens
PJM.UCM$dates <- seq(as.POSIXct("1995-1-1 0:00"), as.POSIXct("2018-3-31 23:00"), by = "hour")
PJM.UCM$Year <- as.numeric(format(PJM.UCM$dates,'%Y'))
PJM.UCM$Month <- as.numeric(format(PJM.UCM$dates,'%m'))
PJM.UCM$Hour <- as.numeric(format(PJM.UCM$dates,'%H'))
PJM.UCM$Day <- as.numeric(format(PJM.UCM$dates,'%d'))
#subset PJM.UCM
year_list <- c(2011,2012,2013)
month_list <- c(12,1,2)
hour_list <- c(7,8,9,10)
subset.PJM.UCM <- PJM.UCM[(PJM.UCM$Year %in% year_list) & (PJM.UCM$Month %in% month_list) & (PJM.UCM$Hour %in% hour_list),]
#mean(subset.PJM.UCM[,1])
#sd(subset.PJM.UCM[,1])
colnames(subset.PJM.UCM)[1] <- "ForcedOutage"
subset.PJM.UCM$casename <- rep("PJM Winter Block 5 Empirical",length(subset.PJM.UCM$ForcedOutage))
subset.PJM.UCM$ForcedOutage<- as.numeric(subset.PJM.UCM$ForcedOutage)

#also subset the actuals!!!
year_list_actual <- c(2014)
month_list_actual <- c(1)
day_list_actual <- c(4,5,6,7,8,9,10)
hour_list_actual <- c(7,8,9,10)
subset.PJM.UCM_actual <- PJM.UCM[(PJM.UCM$Year %in% year_list_actual) & (PJM.UCM$Month %in% month_list_actual) & (PJM.UCM$Hour %in% hour_list_actual) & (PJM.UCM$Day %in% day_list_actual),]
colnames(subset.PJM.UCM_actual)[1] <- "ForcedOutage"
subset.PJM.UCM_actual$ForcedOutage<- as.numeric(subset.PJM.UCM_actual$ForcedOutage)

outagedf <- results1[["DynamicORDC"]][["CapacityOutage"]]

library(RColorBrewer)
c(brewer.pal(n = 7, name = "Set3"),"black")

outagedf
as.factor(subset.PJM.UCM_actual$Day)
geom_rug(data=vac, aes(x = visit), inherit.aes = F)
g <- ggplot(subset.PJM.UCM, aes(x=ForcedOutage,colour=casename)) +
  geom_density(size=1,show.legend=FALSE) +
  geom_line(data=outagedf,aes(x=Margin,y=Prob,colour=as.factor(date)),size=1.5)+
  scale_colour_manual(values=c(brewer.pal(n = 7, name = "Set3"),"black",brewer.pal(n = 7, name = "Set3")))+
  stat_density(aes(x=ForcedOutage, colour=casename),
               geom="line",position="identity",size=3)+
  ylab("Probability") + xlab("MW of Forced Outages")+xlim(c(0,4000))+ylim(c(-.00021,.004))+
  theme_classic() +
  theme(legend.text = element_text(size=16),
        legend.title=element_blank(),
        legend.position="top",
        axis.title.y = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.text.x= element_text(size=20),
        axis.text.y= element_text(size=20),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks =element_line(size=2))
g

#code from Brian to help w/ custom rug
x <- abs(rnorm(25))
y <- x+rnorm(25, sd=0.2)
a <- sample(c('sinnott','luke'), 25, replace=TRUE)

df <- data.frame(x=x, y=y, a=a)

# original plot
ggplot(df, aes(x=x, y=y, colour=a)) + geom_point(shape=1)

# collect data for custom rugs
# use group number to draw lines between correct points

# y-axis 
rug_y <- data.frame(x1=-0.1, x2=0, y1=df$y, y2=df$y, a=df$a)
rug_x <- data.frame(y1=-0.1, y2=0, x1=df$x, x2=df$x, a=df$a)

# plot
ggplot(df, aes(x=x, y=y, colour=a)) + geom_point(shape=1) +
  geom_segment(mapping=aes(x=x1, xend=x2, y=y1, yend=y2), data=rug_x, size=2.5) +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 2.5)) # use this to limit window

subset.PJM.UCM_actual
subset.PJM.UCM_actual$y1 <- -.0002
subset.PJM.UCM_actual$y2 <- -0.00004
rug_x
#then add the custom rug
g
#basic
g + geom_segment(data=subset.PJM.UCM_actual,mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=2, inherit.aes=F)

width <- 2
#segmented
g + geom_segment(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==4,],mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=width, inherit.aes=F, show.legend=F, colour=brewer.pal(n=7,name="Set3")[1]) +
  geom_segment(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==5,],mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=width, inherit.aes=F, show.legend=F, colour=brewer.pal(n=7,name="Set3")[2]) +
  geom_segment(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==6,],mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=width, inherit.aes=F, show.legend=F, colour=brewer.pal(n=7,name="Set3")[3]) +
  geom_segment(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==7,],mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=width, inherit.aes=F, show.legend=F, colour=brewer.pal(n=7,name="Set3")[4]) +
  geom_segment(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==8,],mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=width, inherit.aes=F, show.legend=F, colour=brewer.pal(n=7,name="Set3")[5]) +
  geom_segment(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==9,],mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=width, inherit.aes=F, show.legend=F, colour=brewer.pal(n=7,name="Set3")[6]) +
  geom_segment(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==10,],mapping=aes(x=ForcedOutage,xend=ForcedOutage,y=y1,yend=y2),size=width, inherit.aes=F, show.legend=F, colour=brewer.pal(n=7,name="Set3")[7])
setwd(paste(baseWD, "post_processing", "figures", sep="/"))
ggsave(paste("DensityEmpirical_Updated_rerug", ".png"),dpi=500,units="in", width=10, height=6)

#old code
g + geom_rug(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==4,],aes(x=ForcedOutage),inherit.aes=F,show.legend=FALSE,colour=brewer.pal(n = 7, name = "Set3")[1],size=4, length = unit(0.06, "npc")) +
  geom_rug(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==5,],aes(x=ForcedOutage),inherit.aes=F,show.legend=FALSE,colour=brewer.pal(n = 7, name = "Set3")[2],size=4, length = unit(0.06, "npc")) +
  geom_rug(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==6,],aes(x=ForcedOutage),inherit.aes=F,show.legend=FALSE,colour=brewer.pal(n = 7, name = "Set3")[3],size=4, length = unit(0.06, "npc"), lineend=square) +
  geom_rug(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==7,],aes(x=ForcedOutage),inherit.aes=F,show.legend=FALSE,colour=brewer.pal(n = 7, name = "Set3")[4],size=1, length = unit(0.06, "npc")) +
  geom_rug(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==8,],aes(x=ForcedOutage),inherit.aes=F,show.legend=FALSE,colour=brewer.pal(n = 7, name = "Set3")[5],size=4, length = unit(0.06, "npc")) +
  geom_rug(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==9,],aes(x=ForcedOutage),inherit.aes=F,show.legend=FALSE,colour=brewer.pal(n = 7, name = "Set3")[6],size=4, length = unit(0.06, "npc")) +
  geom_rug(data=subset.PJM.UCM_actual[subset.PJM.UCM_actual$Day==10,],aes(x=ForcedOutage),inherit.aes=F,show.legend=FALSE,colour=brewer.pal(n = 7, name = "Set3")[7],size=4, length = unit(0.06, "npc"))
  
write.csv(results1$NoReserves$modelLMP, "checkprices.csv")
setwd(paste(baseWD, "post_processing", "figures", sep="/"))
ggsave(paste("DensityEmpirical_Updated", ".png"),dpi=500,units="in", width=10, height=6)

