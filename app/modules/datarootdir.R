library(shinydashboard)
library(shinyalert)
library(vroom)
library(xtable)
library(data.table)
library(shinyFiles)

datarootdirUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    box(title = "helpx", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp"))),
  fluidRow(
      column(3,
             shinyDirButton(
                            ns("dir"),label = "select root data dir",
                            title = "Please select the root data folder where the data dirs are locacated"
                            )
            ),
      column(9,
             verbatimTextOutput(ns("dir"), placeholder = TRUE)
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
      disks <- system("wmic logicaldisk get caption", inter = TRUE)
      d <- read.fwf(textConnection(disks[2:(length(disks)-1)]),
                                       widths=c(4), strip.white=TRUE, stringsAsFactors=FALSE)
      mydisks = d$V1
      names(mydisks) <- mydisks
      # list()
      # for (i in 1:length(d)){
      #   mydisks <- c(mydisks, d[i]=d[i])
      # }
      #home = c('my home'= getwd(),"C:"= "C:\\")
      home = mydisks
      # observeEvent(input$drive,{
      #   home = c('my home'= input$drive)
      # })
      shinyDirChoose(
        input,
        'dir',
        roots = home , #c(home = file.path("G:","Programming","dataVisold")),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
      )

      datapath <- reactive({
        parseDirPath(home, input$dir)
      })

      output$dir <- renderPrint({
        datapath()
      })

    return(datapath)
    }

  )
}



