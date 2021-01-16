library(shinydashboard)
library(shinyalert)
library(vroom)
library(xtable)
library(data.table)
library(shinyFiles)
library(rjson)

datarootdirUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    box(title = "helpx", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp"))),
  # fluidRow(
  #     column(3,
  #            shinyDirButton(
  #                           ns("dir"),label = "select root data dir",
  #                           title = "Please select the root data folder where the data dirs are locacated"
  #                           )
  #           ),
  #     column(9,
  #            verbatimTextOutput(ns("dir"), placeholder = TRUE)
  #            )
  #   ),
  fluidRow(
    column(12,
           textInput(ns("dirtextinput"),label = "the final path that will be used")
    )
  ),

  # fluidRow(
  #     column(12,
  #       verbatimTextOutput(ns("mydir"))
  #     )
  #   ),
  )
}


datarootdirServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns
      # nur fuer Windows
      #disks <- system("wmic logicaldisk get caption", inter = TRUE)
      #d <- read.fwf(textConnection(disks[2:(length(disks)-1)]),
      #                                 widths=c(4), strip.white=TRUE, stringsAsFactors=FALSE)
      #mydisks = d$V1
      #names(mydisks) <- mydisks



      filename = "lastSession.json"
      if (file.exists(filename)){
        #cat(file = stderr(),"file exists\n")
        P <<- fromJSON(file=filename)
        #cat(file = stderr(),paste0("P=",P,"\n"))
        #cat(file = stderr(),paste0("names(P)=",colnames(P),"\n"))
        if ("datarootpath" %in% names(P)){
         # cat(file=stderr(),"datarootpath found\n")
          updateTextInput(session, "dirtextinput", value = P$datarootpath)
        }
      }else{
        #cat(file=stderr(),"create p new\n")

        P<-list()
      }

      # list()
      # for (i in 1:length(d)){
      #   mydisks <- c(mydisks, d[i]=d[i])
      # }
      #home = c('my home'= getwd(),"C:"= "C:\\")
      #home = mydisks
      # observeEvent(input$drive,{
      #   home = c('my home'= input$drive)
      # })
      # shinyDirChoose(
      #   input,
      #   'dir',
      #   roots = home , #c(home = file.path("G:","Programming","dataVisold")),
      #   filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
      # )
      #
      # datapath <- reactive({
      #   parseDirPath(home, input$dir)
      # })
      #
      # observeEvent(datapath(), {
      #   #name <- paste(input$data1, "analysis 1")
      #   updateTextInput(session, "dirtextinput", value = datapath())
      # })
      #
      #
      # output$dir <- renderPrint({
      #   datapath()
      # })

      returnval <- reactive({input$dirtextinput})

      observe({
        #cat(file = stderr(), "a change\n")
        input$dirtextinput
        mypath <- isolate(input$dirtextinput)
        #cat(file = stderr(), paste0("mypat=",mypath,"\n"))
        #cat(file = stderr(), paste0("length(mypat)=",nchar(mypath),"\n"))

        if ("datarootpath" %in% names(P)){
          if ((nchar(mypath)>1) && (mypath != P$datarootpath)){
            P$datarootpath <- mypath
            jsonData <- toJSON(P)
            write(jsonData, filename)
            #cat(file = stderr(), "saved\n")
         }
        }else{
            if (nchar(mypath)>1) {
              P$datarootpath <- mypath
              jsonData <- toJSON(P)
              write(jsonData, filename)
              #cat(file = stderr(), "saved2\n")
            }
        }
        # mypath <- isolate(input$dirtextinput)
        # cat(file = stderr(), paste0("mypath = ", mypath,"\n"))
        # cat(file = stderr(), paste0("class(mypath) = ", class(mypath),"\n"))
        # mypath <- as.character(mypath)
        # cat(file = stderr(), paste0("mypath = ", mypath,"\n"))
        # cat(file = stderr(), paste0("class(mypath) = ", class(mypath),"\n"))
        # P$datarootpath <- mypath
        # jsonData <- toJSON(P)

        # writing into JSON file


      })
      # #savein the path into our options file
      # P <- list()
      # cat(file = stderr(), class(returnval))
      # P$datapath <-  returnval


    return(returnval)
    }

  )
}



