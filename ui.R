
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyAce)
# library(shinyIncubator)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Spark Data Viewer"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    radioButtons( inputId="ConnectFilterOrChart_RB",label=" ",
                  choices=c("Connect To Spark Server" = "connectSpark_RB" , 
                            "Execute Spark Operation" = "exeSpark_RB" ,
                            "Create Visualization" = "createViz_RB" ,
                            "'Collect RDD' & Save in R format" = "collectAndSave_RB"
                  ) 
    ) ,
    
    conditionalPanel(
      condition = "input.ConnectFilterOrChart_RB == 'createViz_RB'",
      wellPanel( 
        helpText("Create a visualization based off of either the originally imported data, or the results of the current RDD.") ,
        radioButtons( inputId="vizOrigOrCurrent_RB",label=" ",
                      choices=c("Use Current RDD Results" = "useCurrentRDD_RB" ,
                                "Plot Fields from the Original Data" = "useOrigData_RB"
                      ) 
        ) 
      ) ,
      
      conditionalPanel(
        condition = "input.vizOrigOrCurrent_RB == 'useOrigData_RB'",
        # condition = "input.ConnectFilterOrChart_RB == 'createViz_RB'",
        wellPanel( 
          helpText("Enter a sample size (in %, between 0 and 1.0), and column numbers from the original dataset to use in your plot") ,
          numericInput(inputId="origSampleSize_TI", label="Sample size %, between 0 and 1.0:", value=0.10,min=0.0,max=1.0) ,
          numericInput(inputId="origX_TI", label="Column for X-axis values:", value=1) ,
          numericInput(inputId="origY_TI", label="Column for Y-axis values:", value=2) 
        )
      ) ,
      
      
      wellPanel(
        helpText("Choose a chart type.") ,
        radioButtons( inputId="vizType_RB",label=" ",
                      choices=c("Bar Chart" = "vizBar_RB" , 
                                "Scatterplot" = "vizScatter_RB" 
                                #   "Line Graph" = "vizScatter_RB"
                      ) 
        ) ,
        textInput(inputId="chartTitle_TI", label="Chart Title", value="") ,
        textInput(inputId="chartXlabel_TI", label="X-axis Label", value="") ,
        textInput(inputId="chartYlabel_TI", label="Y-Axis Label", value="")
      ) ,
      
      actionButton(inputId="createViz_AB",label="Create Chart") ,
      helpText("Click 'VISUALIZATION' tab on right to view chart")
      
    ),
    
    conditionalPanel(
      condition = "input.ConnectFilterOrChart_RB == 'collectAndSave_RB'",
      wellPanel( 
        helpText("Enter Path & Filename of where to save your RDD data (will be saved as an R list-of-lists)") ,
        tags$textarea(id="fileSavePath_TI", rows=1, cols=150, "~/myRDD_Rformat") ,
        # textInput(inputId="fileSavePath_TI", label="Path & filename to save", value="~/myRDD_Rformat") ,
        actionButton(inputId="collectAndSave_AB",label="Save") ,
        helpText("When loading the file later: RDD data will be restored into variable 'collectedRDD' and the operations performed on the original data to get these results are restored into varialbe 'operationsStack' ") 
      )
    ),
    
    conditionalPanel(
      condition = "input.ConnectFilterOrChart_RB == 'connectSpark_RB'",
      wellPanel( 
        # tags$textarea(id="fileSavePath_TI", rows=1, cols=80, "~/myRDD_Rformat") ,
        helpText("Spark Server (Master)"),
        tags$textarea(id="sparkServerURL_TI", rows=1, cols=150, "local[4]") ,
        # textInput(inputId="sparkServerURL_TI", label="Spark Server (Master)", value="local[4]") ,
        helpText("Spark Home Directory"),
        tags$textarea(id="sparkHomeDir_TI", rows=1, cols=150, "~/spark-0.9-devprev") ,
        # textInput(inputId="sparkHomeDir_TI", label="Spark Home Directory", value="~/spark-0.9-devprev") ,
        #textInput(inputId="pathToData_TI", label="Path to text file to load into Spark", value="/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_data.csv") ,
        # textInput(inputId="pathToDataColHeaders_TI", label="Path to column headers for text file", value="/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_headers.csv") ,
        helpText("Path to text file to load into Spark ") ,
        helpText("(Either local directory, or HDFS. Full path required; can't use '~')" ) ,
        tags$textarea(id="pathToData_TI", rows=1, cols=150, "/Users/jlent/tempData/tourney_results_data.csv") ,
        #textInput(inputId="pathToData_TI", label="Path to text file to load into Spark (full path required, can't use '~')", value="/Users/jlent/tempData/tourney_results_data.csv") ,
        helpText("Path to column headers for text file"),
        tags$textarea(id="pathToDataColHeaders_TI", rows=1, cols=150, "~/tempData/tourney_results_headers.csv") ,  
        #textInput(inputId="pathToDataColHeaders_TI", label="Path to column headers for text file", value="~/tempData/tourney_results_headers.csv") ,
        
        textInput(inputId="fieldSepChar_TI", label="Field Separator:", value=",") ,
        actionButton(inputId="loadDataIntoSpark_AB",label="Load Textfile Into Spark") 
      )
    ),
    
    #    radioButtons( inputId="filterOrChart_RB", " ",
    #                 c( "Execute Spark Operation" = "exeSpark_RB",
    #                    "Create Visualization" = "createViz_RB" ) 
    #                 ) ,
    
    conditionalPanel(
      condition = "input.ConnectFilterOrChart_RB == 'exeSpark_RB'" ,
      wellPanel(
        selectInput(inputId="sparkOper_SI", label="Select an operation:", 
                    choices = c("map", "filter", "reduce", "groupByKey", "reduceByKey", "sample", "take", "parallelize",
                                "join", "_____________", "count", "countByKey", "_____________", "CUSTOM..." 
                                )
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI != 'CUSTOM...'" ,
          selectInput(inputId="rddToUse_SI", label="Select an RDD on which to apply the operation:", 
                      choices = c("orig_input_data")
                      ) ,
          checkboxInput(inputId="revertOrigData_CB",label="Delete existing RDD's & Apply operation to original input data", value=FALSE)
        ) ,
        # checkboxInput(inputId="revertOrigData_CB",label="Delete existing RDD's & Apply operation to original input data", value=FALSE) ,
        # checkboxInput(inputId="useAce_CB", label="Use Ace Editor?", value=FALSE) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'count'"
          # selectInput(inputId="count_SI",label="Select an RDD to get its count",choices=c("orig_input_data"),selected="orig_input_data")
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'countByKey'" ,
          helpText("Modify the R 'map' function below to specify the column that contains the 'key'") ,
        #  tags$textarea(id="countByKey_TI", rows=12, cols=80, "reduceByKey( map( orig_input_data, 
        #                function(x){ c( x[[1]][1] ,1 ) } ), 
        #                function(x,y) { as.numeric(x) + 
        #                                as.numeric(y) }, 
        #                numPartitions=2L 
        #                )" 
        #  )
          tags$textarea(id="countByKey_TI", rows=8, cols=80, "function(x) { 
                                                                c( x[[1]][1] , 1 ) 
                                                              }" )
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'CUSTOM...'" ,
          helpText("Enter an R function that returns an RDD. All previously saved RDD's are available as variables, as well as 'orig_input_data' and 'previous_RDD'.") ,
          tags$textarea(id="customSparkOper_TI", rows=12, cols=80, "function() { return( 
                                                                        map( orig_input_data, 
                                                                        function(x) { 
                                                                          c( x[[1]][1] , x[[1]][2] ) 
                                                                        } ) 
                                                                        ) }")  
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'sample'" ,
          numericInput(inputId="sampleSize_NI", label="Sample size:", value=0.3)
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'take'" ,
          numericInput(inputId="takeSize_NI", label="Take Sample size:", value=10)
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'parallelize'" ,
          helpText("Enter R function that returns a collection (list or vector) to parallelize:") ,
          tags$textarea(id="parCollection_TI", rows=6, cols=120, "function() { return( 1:10 ) }") , 
          # aceEditor("parCollection_TI", mode="r", theme="idle_fingers", height=150, value="function() { return( 1:10 ) }" ) ,
          numericInput(inputId="parSlices_NI", label="Number of partitions to create in the RDD:", value=2)
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'reduce' | input.sparkOper_SI == 'reduceByKey'" ,
          helpText("Enter R function code to reduce by:") ,
          tags$textarea(id="sparkFunParamReduce_TI", rows=6, cols=120, "function(x,y) { as.numeric(x) + as.numeric(y) }") ,
          # textInput(inputId="sparkFunParamReduce_TI", label="Enter R function code to reduce by:", value="function(x,y){as.numeric(x)+as.numeric(y)}") ,
          # aceEditor("sparkFunParamReduce_TI", mode="r", theme="idle_fingers", height=100, value="function(x,y) { as.numeric(x) + as.numeric(y) }" ) , 
          helpText("Reduce Examples:") ,
          helpText("# 'sum' ; or 'count' if all your values in (K,V) were previously mapped to 1") ,
          helpText("function(x,y) { as.numeric(x) + as.numeric(y) } ")
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'filter'" ,
          helpText("Enter R function code to filter by that returns TRUE/FALSE:") ,
          tags$textarea(id="sparkFunParamFilter_TI", rows=6, cols=120, "function(x) { return( x > 0 ) }") ,
          # aceEditor("sparkFunParamFilter_TI", mode="r", theme="idle_fingers", height=100, value="function(x) { return( x > 0 ) }" ) , 
          helpText("Filter Examples:") ,
          helpText("# Get all rows where column 1 == 'B' ") ,
          helpText("function(x) { return( x[[1]][1] == 'B' ) }") ,
          helpText("# Get all rows where column 2 > 150 " ) ,
          helpText("function(x) { return( x[[1]][2] > 150 ) } ")
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'map'" ,
          helpText("Enter R function code to map by:") ,
          tags$textarea(id="sparkFunParamMap_TI", rows=6, cols=120, "function(x) { 
                                                                          c( x[[1]][1] , x[[1]][2] ) 
                                                                     }") ,
          # aceEditor("sparkFunParamMap_TI", mode="r", theme="idle_fingers", height=100, value="function(x) { c( x[[1]][1] , x[[1]][2] ) }" ) ,  
          helpText("Map Examples:") ,
          helpText("# Grab column 1 and 2 from the current RDD") ,
          helpText("function(x) { c(x[[1]][1] , x[[1]][2]) }") ,
          helpText("# Grab column 3, and map each value to a 1; for preparation of doing a 'count' via a reduce/reduceByKey") ,
          helpText("function(x) { c(x[[1]][3] , 1) } ") ,
          helpText("# Grab columns 1,2,3, and add columns 2 + 3") ,
          helpText("function(x) { c( x[[1]][1] , x[[1]][2] ,  x[[1]][3] , as.numeric(x[[1]][2]) +  as.numeric(x[[1]][3]) ) }")
        ) ,
        
        conditionalPanel(
          condition = "input.sparkOper_SI == 'join'" ,
          helpText("Name of 1st RDD to Join:") ,
          tags$textarea(id="join1_TI", rows=1, cols=120, "orig_input_data") ,
          helpText("Name of 1st RDD to Join:") ,
          tags$textarea(id="join2_TI", rows=1, cols=120, "previous_RDD") 
        ) ,
        conditionalPanel(
          condition = "input.sparkOper_SI == 'averageByKey'" ,
          helpText("Name of RDD with 'count':") ,
          tags$textarea(id="averageCounts_TI", rows=1, cols=120, "") ,
          helpText("Name of RDD with 'sum':") ,
          tags$textarea(id="join2_TI", rows=1, cols=120, "previous_RDD") 
        ) ,
        
        actionButton(inputId="runSparkOper_AB",label="Run Operation") ,
        textInput(inputId="newStepRDD_TI", label="Save the Results to a new RDD variable?", value="previous_RDD"),
        helpText("(Otherwise result will just overwrite 'previous_RDD')")
      )  
    ) 
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    wellPanel(
      h4("Original Imported Data (take 1000 sample)") ,
      dataTableOutput(outputId="tab_RDD_dt") 
    ) ,
    
    
    wellPanel( 
      # h4("Current Data After the Applied Operations, Etc.") ,
      h4("Visualization of Results After Running Spark Operations") ,
      tabsetPanel(id="outputTabs",
                  # tabPanel(title="Current RDD Table", dataTableOutput(outputId="tab_RDD_dt"), value=5) ,
                  tabPanel(title="Current RDD (take 5 sample)", verbatimTextOutput(outputId="tab_RDD"), value=1) ,
                  tabPanel(title="Previous Operations Applied", verbatimTextOutput(outputId = "tab_operStack"), value=2) ,
                  tabPanel(title="Original Data RDD Format (take 5 sample)", verbatimTextOutput(outputId = "tab_OrigData"), value=3) ,
                  # tabPanel(title="Current RDD Sample", plotOutput(outputId="tab_RDD", width="100%", height="700px"),value=1) , 
                  tabPanel(title="VISUALIZATION", plotOutput(outputId="tab_viz",width="100%",height="500px"), value=4, inputId="selectVizTab") 
                  #tabPanel(title="Export", tableOutput("table_data"),value=3) ,
                  #tabPanel(title="Underlying Data", verbatimTextOutput(outputId="table_data"),value=4)
      ) 
    )  
    
    # verbatimTextOutput(outputId="summaryStatus") 
    
    #  tabsetPanel(id="outputTabs2",
    #                tabPanel(title="Bar Chart", plotOutput(outputId="tab_Bar",width="100%",height="700px"), value=5) ,
    #                tabPanel(title="Histogram", plotOutput(outputId = "tab_Hist",width="100%",height="700px"), value=6) ,
    #                tabPanel(title="Scatterplot", plotOutput(outputId = "tab_Scatter",width="100%",height="700px"), value=7) 
    #              )
    
  )
) ) 
