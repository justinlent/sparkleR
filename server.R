
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#


# sc <- sparkR.init(master="local[8]", appName="SparkTest", sparkHome="/Users/jlent/spark-0.9-devprev")
# orig_input_data <- map( textFile(sc, "/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_data.csv") ,
#                         FUN=function(x){x<-strsplit(x=x,split=",")}
#                )

library(ggplot2)
library(RColorBrewer)
library(shiny)
library(shinyAce)
# library(shinyIncubator)
library(rJava)
library(SparkR)

# --------------------------------------
# Chart plotting functiona
plotHistogram<-function( values, barColor="Blues", barColorDarkness=3, chartTitle=NULL, chartTitleSize=16, 
                         showMeanLine=FALSE, showStDevLines=FALSE, 
                         xAxisLabel=NULL, yAxisLabel="Count", 
                         showHorizontalBars=FALSE,
                         useDefaultName=FALSE) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-as.data.frame(values)
  colnames(DF)<-"x"
  localEnv <- environment()
  xAxisLabelStr <- NULL
  yAxisLabelStr <- yAxisLabel
  
  if( useDefaultName ){
    xAxisLabelStr <-  names(values)
  } else {
    if( !is.null(xAxisLabel) ) {
      xAxisLabelStr <- xAxisLabel
    }  
  }
  
  
  ggPlotFormula<-ggplot(data=DF, aes(x=x)) + 
    scale_color_brewer(type="seq") +
    geom_histogram(fill=paste(mypalette[barColorDarkness]),colour="grey30") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  if( showMeanLine ){
    tempMean=mean(values)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.5)
    if( showHorizontalBars ){
      ggPlotFormula<-ggPlotFormula + 
        annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=1.05, size=3.5, color="grey40" )        
    } else {
      ggPlotFormula<-ggPlotFormula + 
        annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=-0.1, size=3.5, color="grey40" )        
    } 
  }
  if( showStDevLines ){
    tempMean<-mean(values)
    if( class(values)[1] == "xts" || class(values)[1] == "zoo" ){
      tempStdDev<-apply(values,2,sd)  
    } else {
      tempStdDev<-sd(values)
    }
    if( showHorizontalBars ){
      ggPlotFormula<-ggPlotFormula + 
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" )
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" )
    }
  }
  return(ggPlotFormula)
}

plotBarChart<-function( values, categoryNames, barColor="Blues", barColorDarkness=4, 
                        chartTitle=NULL, chartTitleSize=18, xAxisLabel=NULL, yAxisLabel="Values", 
                        showMeanLine=FALSE, showStDevLines=FALSE, showHorizontalBars=FALSE, sortAsc=FALSE, sortDesc=FALSE ) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-data.frame(xVal=values, categoryNames=categoryNames)
  localEnv <- environment()
  yAxisLabelStr<-yAxisLabel
  if( !sortAsc && !sortDesc ){
    ggPlotFormula<-ggplot(data=DF, aes(x=categoryNames, y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
  } else {
    if( sortAsc ){
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) 
    } else {
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
    } 
  }
  #  ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) + 
  ggPlotFormula<-ggPlotFormula +
    geom_bar(fill=paste(mypalette[barColorDarkness]),colour="grey30",stat="identity") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  
  if( showMeanLine ){
    tempMean<-mean(values)
    minXcoord<-min(values)
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = tempMean, colour=paste(mypalette[barColorDarkness+2]), linetype = "dashed", size=1.0, alpha=1.0) +
      annotate(geom="text", x=minXcoord, y=tempMean, label="avg", vjust=-0.5, hjust=-0.1, size=5, color="grey20" )  
  }
  
  if( showStDevLines) {
    
    tempMean<-mean(values)
    minXcoord<-min(values)
    if( class(values)[1] == "xts" || class(values)[1] == "zoo" ){
      tempStdDev=apply(values,2,sd)  
    } else {
      tempStdDev=sd(values)
    }
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
      annotate(geom="text", x=minXcoord, y=tempMean-tempStdDev, label="-1sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean+tempStdDev, label="+1sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean-2*tempStdDev, label="-2sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean+2*tempStdDev, label="+2sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" )
  }
  
  return(ggPlotFormula)
} 

plotScatterplot<-function( xValues, yValues, pointLabelNames=NULL, showPointLabels=FALSE, 
                           barColor="Blues", barColorDarkness=5, pointSize=4, 
                           chartTitle=NULL, chartTitleSize=16,  
                           xAxisLabel=NULL, yAxisLabel=NULL, 
                           showRegressionLine=FALSE, showConfidenceInterval=FALSE, 
                           showRegressionAdjRSQ=FALSE, labelPositionRSQ="upper-left", labelRSQfontSize=6, 
                           showXmeanLine=FALSE, showYmeanLine=FALSE, 
                           showXstDevLines=FALSE, showYstDevLines=FALSE, 
                           labelPositionRSQoverrideX=0, labelPositionRSQoverrideY=0,
                           pointColorValues=NULL,
                           lowShadingColor=NULL,
                           highShadingColor=NULL,
                           legendLabel=""
) {
  # parameter "barColor" can be 'Blues', 'Greens', 'Reds'
  
  # inputs "xValues", "yValues", and "pointLabelNames" should be vectors all of the same length
  if( is.null(pointLabelNames) && showPointLabels==TRUE ){
    print("You specified showPointLabels=TRUE but did not pass in a vector of point label names to the pointLabelNames parameter.")
    return()
  }
  
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(8, barColor)
  
  #DF<-as.data.frame(values)
  if( showPointLabels ) {
    DF<-data.frame(x=xValues, y=yValues, pointLabels=pointLabelNames)
    colnames(DF)<- c("x","y","pointLabels")
  } else {
    DF<-data.frame(x=xValues, y=yValues)
    colnames(DF)<- c("x","y")
  }
  
  if( ! is.null(pointColorValues) ){
    DF<-cbind( DF, pointColors=pointColorValues )
  } else {
    DF<-cbind( DF, pointColors=barColorDarkness )
  }
  
  localEnv <- environment()
  
  yAxisLabelStr<-yAxisLabel
  ggPlotFormula <- NULL
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y ) )
  } else {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y, color=pointColors))
  }
  ggPlotFormula <- ggPlotFormula + 
    # scale_color_brewer(type="seq") +
    # geom_point(show_guide=FALSE, shape=19, size=pointSize) + 
    #geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize) + 
    # geom_point(color=pointColorValues, show_guide=FALSE, shape=19, size=pointSize) + 
    # scale_color_gradient(colours=rainbow(7))
    ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) + 
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    #    geom_text(data=DF, aes(label=pointLabels), vjust=-0.8) +
    theme(plot.title=element_text(size=chartTitleSize)) 
  
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggPlotFormula +
      geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize)
  } else {
    ggPlotFormula <- ggPlotFormula +
      geom_point(show_guide=FALSE, shape=19, size=pointSize) +
      labs(colour=legendLabel) + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="bottom")
    
    if( !is.null(lowShadingColor) & !is.null(highShadingColor) ) {
      ggPlotFormula <- ggPlotFormula + 
        scale_color_gradient(low=lowShadingColor, high=highShadingColor)
    }
  }
  
  if( showPointLabels ){
    ggPlotFormula<-ggPlotFormula +
      geom_text(data=DF, aes(label=pointLabels), vjust=-0.8)
  }
  if( showRegressionLine ){
    if( showConfidenceInterval ){
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", fill="pink", alpha=0.25, method=lm, show_guide=FALSE)
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", method=lm, se=FALSE, show_guide=FALSE)
    }
  }
  if( showRegressionAdjRSQ ) {
    tempLM<-lm(DF[,2] ~ DF[,1])
    tempAdjRSQ<-summary(tempLM)$adj.r.squared
    tempAdjRSQ<-round(tempAdjRSQ,2)
    
    if( labelPositionRSQ=="lower-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =", tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=0.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="lower-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=-0.5+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    
  }
  
  if( showXmeanLine ){
    tempMean=mean(xValues)
    minYcoord<-min(DF[,2])
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=tempMean, y=minYcoord, label="avg(X)", angle=90, vjust=-1.0, hjust=0.2, size=4, color="grey40" )  
  }
  if( showYmeanLine ){
    tempMean=mean(yValues)
    minXcoord<-min(DF[,1])
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=minXcoord, y=tempMean, label="avg(Y)", vjust=-1.0, hjust=0.1, size=4, color="grey40" )  
  }
  
  if( showXstDevLines ){
    tempMean=mean(xValues)
    tempStdDev=sd(xValues)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  if( showYstDevLines ){
    #     tempMean=mean(yValues)
    tempStdDev=sd(yValues)
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  return(ggPlotFormula)
}

# --------------------------------------

# count:  rk1_count <- reduceByKey( map(sparkDataTest_map, function(x){ c(x[1],1) }), function(x,y){ as.numeric(x) + as.numeric(y) }  , 2L )
# lapply(take(rk1_count,5),unlist)
# unlist(lapply(lapply(take(rk1_count,5),unlist), "[", 1) )  # get all the "group" names/categories ie: for chart labels
# avg <- function(...){ return(base::mean(c(as.numeric(...)))) }
# med <- function(...){ return(stats::median(c(...))) }
# stdev <- function(...){ return(stats::sd(c(...))) }
# cnt <- function(...){ return(base::length(c(...))) }

getKeys_ListOfList <- function(listOfLists) {
  return( unlist(lapply( listOfLists, "[[", 1)) )
}

getValues_ListOfList <- function(listOfLists) {
  return( unlist(lapply( listOfLists, "[[", 2)) )
}

depthList <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depthList)), 0L)

filter_RDD <- function( RDD, FUN_trueCondition ) {
  return( lapplyPartition( RDD, function(part){ Filter(f=FUN_trueCondition, x=part) } ) )
}

listToDF_old <- function( inputList, convertToNumericIfPossible=FALSE, numericColumns=c(2,3,4,5,6) ){
  returnDF <- data.frame(check.names=TRUE,stringsAsFactors=FALSE)
  
  for( i in 1:length(inputList) ){
    if( convertToNumericIfPossible ){
      # returnDF <- rbind( returnDF, as.integer(inputList[[i]][[1]]) )   
      # inputList[[i]][[1]][,numericColumns] <- as.numeric(inputList[[i]][[1]][,numericColumns])
      # returnDF <- rbind( returnDF, as.numeric(inputList[[i]][[1]]) )  
      names(inputList[[i]][[1]]) <- seq(1:length(inputList[[i]][[1]]))
      returnDF <- rbind( returnDF, as.numeric(inputList[[i]][[1]]) )  
      names(returnDF) <- names(inputList[[i]][[1]])
    } else {
      names(inputList[[i]][[1]]) <- seq(1:length(inputList[[i]][[1]]))
      returnDF <- rbind( returnDF, dQuote(inputList[[i]][[1]]) )       
      names(returnDF) <- names(inputList[[i]][[1]])
    }
    
  }
  
  return( returnDF )
}

listToDF <- function( inputList, convertToNumericIfPossible=FALSE, columnHeaders=NULL ){
  
  # returnDF <- matrix(nrow=length(inputList),ncol=length(inputList[[1]][[1]]) )
  returnDF <- matrix(nrow=0,ncol=length(inputList[[1]][[1]]) )
  
  for( i in 1:length(inputList) ){
    if( convertToNumericIfPossible ){
      # returnDF <- rbind( returnDF, as.numeric(inputList[[i]][[1]]) )  
      # names(inputList[[i]][[1]]) <- seq(1:length(inputList[[i]][[1]]))
      returnDF <- rbind( returnDF, as.numeric(inputList[[i]][[1]]) )  
      # names(returnDF) <- names(inputList[[i]][[1]])
    } else {
      if( is.null(columnHeaders) ){
        names(inputList[[i]][[1]]) <- seq(1:length(inputList[[i]][[1]]))  
      } else {
        names(inputList[[i]][[1]]) <- paste( seq(1:length(inputList[[i]][[1]])), columnHeaders, sep="_" )   
      }
      
      returnDF <- rbind( returnDF, inputList[[i]][[1]] )       
      names(returnDF) <- names(inputList[[i]][[1]])
    }
  }
  return( returnDF )
}


orig_input_data <<- NULL
#dataHeaders <<- NULL
sc <<- NULL

operationsStack <<- list()
savedRDD <<- list()
previous_RDD <<- NULL
takeList <<- list()
takeListDF <<- data.frame()
ig <<- list()
igDF <<- data.frame()
prev_revertOrigData_AB <<- 0
prev_runSpark_AB <<- 0
currStackVal <<- 0

connectToSpark <- function(masterURL="local[4]", appNameStr="SparkTest", sparkHomeStr) {
  scSet <- sparkR.init(master=masterURL, appName=appNameStr, sparkHome=sparkHomeStr)
  return( scSet )
}
getSparkData <<- function(sparkContext, pathStr) {  
  #  sparkData <- map( textFile(sparkContext, "/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_data.csv") ,
  #                   FUN=function(lineStr){splitVec<-strsplit(x=lineStr,split=",")}
  #                )
  orig_input_data <- map( textFile(sc=sparkContext, path=pathStr) ,
                    FUN=function(lineStr){splitVec<-strsplit(x=lineStr,split=",")}
  )
  return( orig_input_data )
}

getDataColumnHeaders <- function(pathStr) {
  # dataHeaders <- read.csv(file="/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_headers.csv",header=FALSE)
  dataHeaders <- read.csv(file=pathStr,header=FALSE)
  dataHeaders <- c(t(dataHeaders))
  return( dataHeaders )  
}

# dataHeaders <- read.csv(file="/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_headers.csv",header=FALSE)
# dataHeaders <- c(t(tournResRDD_headers))
dataHeaders <<- NULL
prevSave <<- 0

shinyServer(function(input, output, session) {
  
  resetSparkData <- reactive( {  
    if( input$revertOrigData_AB > 0 ) {
      if( input$revertOrigData_AB > prev_revertOrigData_AB ){
        sc <<- connectToSpark(sparkHomeStr=input$sparkHomeDir_TI)
        previous_RDD <<- getSparkData(sc, pathStr=input$pathToData_TI)
        operationsStack <<- list()
        savedRDD <<- list()
        prev_revertOrigData_AB <<- input$revertOrigData_AB 
        currStackVal <<- 0
        #  takeList <<- SparkR::take(previous_RDD, 5)
        #  return( print(takeList) ) 
      }  
    }
  } )
  
  # Generates the RDD sample data shown in the "Current RDD Sample" tab 
  output$tab_RDD <- renderPrint( {
    
    if( input$runSparkOper_AB > 0 ) {
      
      isolate( {
        rddToUseStr <<- "previous_RDD"
        
        if( input$revertOrigData_CB ) {
          previous_RDD <<- orig_input_data
          operationsStack <<- list()
          savedRDD <<- list()
          savedRDD[[ "orig_input_data" ]] <<- previous_RDD
          prev_revertOrigData_AB <<- input$revertOrigData_AB 
          currStackVal <<- 0
          updateCheckboxInput(session, "revertOrigData_CB", value=FALSE)
        } else {
          if( !(input$rddToUse_SI == "previous_RDD") ) {
            previous_RDD <<- savedRDD[[ input$rddToUse_SI ]]
            rddToUseStr <<- paste("savedRDD[[ '", input$rddToUse_SI, "' ]]",sep="")
          }
        }
        
        # expressionStr <<- NULL
        
        if( input$sparkOper_SI == "groupByKey" ) {
          # savedRDD[[ input$runSparkOper_AB ]] <<- eval( expr=parse( text=paste( input$sparkOper_SI,"(",as.character(quote(previous_RDD)), ", numPartitions=2L)" , sep="") ) )
          # previous_RDD <- eval( expr=parse( text=paste( input$sparkOper_SI,"(",as.character(quote(previous_RDD)), ", numPartitions=2L)" , sep="") ) )
          expressionStr <- paste("previous_RDD <<- SparkR::groupByKey(", rddToUseStr, ", numPartitions=2L)" , sep="")
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::groupByKey(previous_RDD , numPartitions=2L)" , sep="") ) )
          eval( expr=parse( text=paste("previous_RDD <<- SparkR::groupByKey(", rddToUseStr, ", numPartitions=2L)" , sep="") ) )
          # WORKS -- previous_RDD <<- SparkR::groupByKey(previous_RDD , numPartitions=2L)
        }
        #    if( input$sparkOper_SI == "take" ) {
        #      eval( expr=parse( text=paste("previous_RDD <<- SparkR::take( previous_RDD," , input$sampleSize_NI, ")" , sep="") ) )
        #    }
        if( input$sparkOper_SI == "sample" ) {
          expressionStr <- paste("previous_RDD <<- SparkR::sampleRDD(", rddToUseStr,", FALSE,", input$sampleSize_NI, ", seed=9L)", sep="" )
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::sampleRDD( previous_RDD, FALSE," , input$sampleSize_NI, ", seed=9L)" , sep="") ) )
          eval( expr=parse( text=paste("previous_RDD <<- SparkR::sample(", rddToUseStr, ",FALSE," , input$sampleSize_NI, ", seed=9L)" , sep="") ) )
        }
        if( input$sparkOper_SI == "parallelize" ) {
          expressionStr <- paste("previous_RDD <<- SparkR::parallelize( sc, ", input$parCollection_TI, " numSlices=", input$parSlices_NI, ")" , sep="")
          eval( expr=parse( text=paste("functionToRun <-", input$parCollection_TI) ) )
          tempCollection <- functionToRun()
          eval( expr=parse( text=paste("previous_RDD <<- SparkR::parallelize( sc, tempCollection, numSlices=" , input$parSlices_NI , ")" , sep="") ) )
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::parallelize( sc, " , input$parCollection_TI, ", numSlices=" , input$parSlices_NI , ")" , sep="") ) )
        }
        if( input$sparkOper_SI == "CUSTOM..." ) {
          expressionStr <- paste("previous_RDD <<- ", input$customSparkOper_TI)
          eval( expr=parse( text=paste("functionToRun <-", input$customSparkOper_TI) ) )
          eval( expr=parse( text=paste("previous_RDD <<- functionToRun()" , sep="") ) )
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::parallelize( sc, " , input$parCollection_TI, ", numSlices=" , input$parSlices_NI , ")" , sep="") ) )
        }
        if( input$sparkOper_SI == "map" ) {
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::map( previous_RDD, function(x){ c( x[[1]][1], x[[1]][2] ) } )" , sep="") ) )
          expressionStr <- paste("previous_RDD <<- SparkR::map(", rddToUseStr, ", ", input$sparkFunParamMap_TI, ")", sep="" )
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::map(previous_RDD,", input$sparkFunParamMap_TI, ")" , sep="") ) )
          eval( expr=parse( text=paste("previous_RDD <<- SparkR::map(", rddToUseStr, "," , input$sparkFunParamMap_TI, ")" , sep="") ) )
          # previous_RDD <<- SparkR::map( previous_RDD, function(x){ c( x[[1]][1], x[[1]][2] ) } )
        }
        if( input$sparkOper_SI == "filter" ) {
          expressionStr <- paste("previous_RDD <<- filter_RDD(", rddToUseStr, ",", input$sparkFunParamFilter_TI, ")", sep="" )
          # eval( expr=parse( text=paste("previous_RDD <<- filter_RDD( previous_RDD,", input$sparkFunParamFilter_TI, ")" , sep="") ) )
          eval( expr=parse( text=paste("previous_RDD <<- SparkR::filter(", rddToUseStr, "," , input$sparkFunParamFilter_TI, ")" , sep="") ) )
        }
        if( input$sparkOper_SI == "reduce" ) {
          expressionStr <- paste("previous_RDD <<- SparkR::reduce(", rddToUseStr, ",", input$sparkFunParamReduce_TI, ")", sep="" )
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::reduce( previous_RDD,", input$sparkFunParamReduce_TI, ")" , sep="") ) )
          eval( expr=parse( text=paste("previous_RDD <<- SparkR::reduce(", rddToUseStr, "," , input$sparkFunParamReduce_TI, ")" , sep="") ) )
        }
        if( input$sparkOper_SI == "reduceByKey" ) {
          expressionStr <- paste("previous_RDD <<- SparkR::reduceByKey(", rddToUseStr, ",", input$sparkFunParamReduce_TI, ", 2L)", sep="" )
          # eval( expr=parse( text=paste("previous_RDD <<- SparkR::reduceByKey( previous_RDD,", input$sparkFunParamReduce_TI, ", 2L)" , sep="") ) )
          eval( expr=parse( text=paste("previous_RDD <<- SparkR::reduceByKey(", rddToUseStr, "," , input$sparkFunParamReduce_TI, ", 2L)" , sep="") ) )
        }
        if( input$sparkOper_SI == "count" ) {
          eval( expr=parse( text=paste("countNum <<- SparkR::count(", rddToUseStr, ")" ) ) )
          return( print(countNum) )
        }
        if( input$sparkOper_SI == "countByKey" ) {
          # expressionStr <- paste("previous_RDD <<- ", input$countByKey_TI)
          # expressionStr <- paste("previous_RDD <<- function() { return(", input$countByKey_TI, ") }")
          # eval( expr=parse( text=paste("functionToRun <- function() { return(", input$countByKey_TI, ") }") ) )
          expressionStr <- paste("previous_RDD <<- function() { return( reduceByKey( map( ", rddToUseStr, ",", input$countByKey_TI, " )" , ", function(x,y) { as.numeric(x) + as.numeric(y) }, numPartitions=2L) ) }" )
          eval( expr=parse( text=paste("previous_RDD <<- reduceByKey( map( ", rddToUseStr, ",", input$countByKey_TI, " )," , "function(x,y) { as.numeric(x) + as.numeric(y) }, numPartitions=2L)" ) ) )
          # eval( expr=parse( text=paste("previous_RDD <<- functionToRun()" , sep="") ) )
        }
        
        currStackVal <<- currStackVal + 1
        # operationsStack[[ currStackVal ]] <<- paste("operation #", currStackVal, " (", input$sparkOper_SI, ") R expr: ", expressionStr, sep="")
        operationsStack[[ currStackVal ]] <<- paste("operation #", currStackVal, " (", input$sparkOper_SI, "): ", input$newStepRDD_TI, " <<-", strsplit(expressionStr,"<<-")[[1]][2], sep="")
        
        if( !is.null(input$newStepRDD_TI) ) {
          # need to save off both the actual RDD object as well as the "string" which is the key to the list. The string is necessary for generating the expression for an eval() call
          savedRDD[[ input$newStepRDD_TI ]] <<- previous_RDD            
        } 
        
        takeList <<- SparkR::take(previous_RDD,5)
        takeListDF <<- listToDF( SparkR::take(previous_RDD,100) )
        names(takeListDF) <- seq(1:dim(takeListDF)[2])
        
        updateSelectInput( session=session,inputId="rddToUse_SI",label="Select an RDD on which to apply the operation (disregard for 'CUSTOM...'):", choices=c("previous_RDD", names(savedRDD)) )
        updateTextInput(session=session,inputId="newStepRDD_TI",label="Save the Results to a new RDD variable?",value="previous_RDD")
        
        return( print(takeList) )
      } )
      
    }
    
    if( input$loadDataIntoSpark_AB == 1 ) {
      if( is.null(orig_input_data) ) {
        sc <<- connectToSpark(sparkHomeStr=input$sparkHomeDir_TI)
        orig_input_data <<- getSparkData(sc, pathStr=input$pathToData_TI)
        previous_RDD <<- orig_input_data
        savedRDD[[ "orig_input_data" ]] <<- previous_RDD
        dataHeaders <<- getDataColumnHeaders(pathStr=input$pathToDataColHeaders_TI)
        takeList <<- SparkR::take(previous_RDD,5)
        takeListOrig <<- takeList
        takeListDF <<- listToDF( SparkR::take(previous_RDD,1000), columnHeaders=dataHeaders )
        names(takeListDF) <- seq(1:dim(takeListDF)[2])
        
        takeListOrigDF <<- takeListDF
        # names(takeListOrigDF) <<- paste( seq(1:dim(takeListOrigDF)[2]), dataHeaders, sep="_" )  
        names(takeListOrigDF) <<- dataHeaders  
        
        return( print(takeList) ) 
        # return( print(takeListDF) ) 
      }       
    }
    
  } )
  
  output$tab_RDD_dt <- renderDataTable( {
    if( input$loadDataIntoSpark_AB == 1 ) {
      return( takeListOrigDF )  
    } else {
      if( input$runSparkOper_AB >= 1 ) {
        return( takeListDF )  
      } else { 
        return( data.frame() )
      }
    }
  } , options = list(iDisplayLength=5, aLengthMenu=c(5,10,20,50,100)) 
  )
  
  observe( {
    if( input$collectAndSave_AB > prevSave ){
      collectedRDD <- collect(previous_RDD)
      save(collectedRDD,operationsStack, file=input$fileSavePath_TI)
      # updateCheckboxInput(session, "collectAndSave_CB", value=FALSE)
      prevSave <<- prevSave + 1
    }
  })
  
  output$tab_viz <- renderPlot( {
    if( input$createViz_AB > 0 ) {
      # updateTabsetPanel( session, inputId=input$outputTabs, selected=input$selectVizTab )
      isolate( {
        
        
        if( input$vizOrigOrCurrent_RB == "useOrigData_RB") {
          tempRDD_DF <- listToDF( collect(SparkR::sampleRDD(rdd=orig_input_data, withReplacement=FALSE, fraction=input$origSampleSize_TI, seed=9L)), columnHeaders=dataHeaders )
          if( input$vizType_RB == "vizBar_RB") {
            return( print( plotBarChart( values=as.numeric(tempRDD_DF[,input$origY_TI]), categoryNames=tempRDD_DF[,input$origX_TI], chartTitle=input$chartTitle_TI, xAxisLabel=input$chartXlabel_TI, yAxisLabel=input$chartYlabel_TI ) ) )      
          }
          if( input$vizType_RB == "vizScatter_RB") {
            return( print( plotScatterplot(yValues=as.numeric(tempRDD_DF[,input$origY_TI]),xValues=tempRDD_DF[,input$origX_TI], chartTitle=input$chartTitle_TI, xAxisLabel=input$chartXlabel_TI, yAxisLabel=input$chartYlabel_TI ) ) )    
          }
        } else {
          if( input$vizType_RB == "vizBar_RB") {
            return( print( plotBarChart( values=as.numeric(getValues_ListOfList(collect(previous_RDD))), categoryNames=getKeys_ListOfList(collect(previous_RDD)), chartTitle=input$chartTitle_TI, xAxisLabel=input$chartXlabel_TI, yAxisLabel=input$chartYlabel_TI ) ) )    
          }
          if( input$vizType_RB == "vizScatter_RB") {
            return( print( plotScatterplot(yValues=as.numeric(getValues_ListOfList(collect(previous_RDD))),xValues=as.numeric(getKeys_ListOfList(collect(previous_RDD))), chartTitle=input$chartTitle_TI, xAxisLabel=input$chartXlabel_TI, yAxisLabel=input$chartYlabel_TI ) ) )    
          }  
        }
      } )
    }
  } )
  
  output$tab_OrigData <- renderPrint( {
    #  takeList <- take(orig_input_data,5)
    #  takeListDF <- listToDF( takeList )
    #  names(takeListDF) <- seq(1:dim(takeListDF)[2])
    if( input$runSparkOper_AB > 0 ){
      return( print(takeListOrig) )
    }
  } )
  
  output$tab_operStack <- renderPrint( {
    if( input$runSparkOper_AB > 0 ){
      return( print(operationsStack) )
    }
  } )
  
  # Generate the summary at bottom of webpage of the code used to create the current RDD data
  output$summaryStatus <- renderPrint({
    if( input$runSparkOper_AB > 0 ){
      return( print(takeListOrig) ) 
      #    isolate( {
      #     # operationsStack[[ input$runSparkOper_AB ]] <<- paste("operation #:", input$runSparkOper_AB, input$sparkOper_SI)
      #     currStackVal <<- currStackVal + 1
      #     operationsStack[[ currStackVal ]] <<- paste("operation #:", currStackVal, input$sparkOper_SI)
      #     prev_runSpark_AB <<- prev_runSpark_AB + 1
      #     print( operationsStack )
      #    } )
      
    } else {
      outputText <- "Load Data into Spark -- The area above will show a sample from the current RDD from filter operations, and the area below will show the original data for convenience."
      print(outputText)  
    }
    
  })
  
  
})
