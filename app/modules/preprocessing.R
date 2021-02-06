library(shinydashboard)
library(shinyalert)
library(vroom)
library(xtable)
library(data.table)

preprocessingUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    box(title = "helpx", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp"))),

    # fluidRow(
    #   column(4, actionButton(ns("showhelp"),"showhelp"),
    #          actionButton(ns("hidehelp"),"hidehelp"), offset = 8),
    # ),
    #htmlOutput(ns("htmlhelp")),
    fluidRow(
      column(8, fileInput(ns("file_beha"), label = "behavioral file", accept = c(".csv", ".tsv"))),
      column(2, textInput(ns("delimiter_beha"),"csv sep", value = ";")),
      #column(2, div(style = "background-color:yellow; text-align:center;", "Bottomright", actionButton(ns("showbeha"),"show beha"),
      column(2, div(style = "text-align:center; margin-top: 25px;", actionButton(ns("showbeha"),"show Tab"),
                                  actionButton(ns("hidebeha"),"hide Tab"))),
    ),
    fluidRow(
      #HTML("<div class='col-sm-4' style='min-width: 350px !important;'>"),
      column(12, box(tableOutput(ns("head_beha")))),
    ),
    #box(title = "table head", collapsible = TRUE, collapsed = TRUE, tableOutput(ns("head_beha"))),

    fluidRow(
      column(12, fileInput(ns("file_data"), label = "data json file", accept = c(".csv", ".tsv", ".json"))),
      #column(2, textInput(ns("delimiter_data"),"csv sep", value = ";")),

#      column(2, div(style = "text-align:center; margin-top: 25px;", actionButton(ns("showdata"),"show Tab"),
#                    actionButton(ns("hidedata"),"hide Tab"))),
    ),
    #box(title = "table head", collapsible = TRUE, collapsed = TRUE, tableOutput(ns("head_beha"))),


    # textInput(ns("delimiter_data"),"enter delimiter", value = ";"),
    # fileInput(ns("file_data"), label = "data file", accept = c(".csv", ".tsv")),
    # actionButton(ns("showdata"),"show data"),
    # actionButton(ns("hidedata"),"hide data"),
    tableOutput(ns("head_data")),


    fluidRow(
      # column(4,
      #        selectInput(ns("dataMethod"), h4("Select Method"),
      #           choices = c("Coherence","Transferentropy","Frequency", "Granger"))
      #        ),
      column(4,
             textInput(ns("outputname"),"enter output name", value = "01"),
             ),
    ),
fluidRow(
      column(4, actionButton(ns("estimate"),"perform preprocessing")
             ),
    ),
    useShinyalert(),

    #uiOutput(ns("uipreprocessing")),
    #verbatimTextOutput(ns("preprocessingText")),
    # selectInput(ns("var"), "Variable", choices = c("one", "two"), selected = "two"),
    # actionButton(ns("do1"),"Click me")
  )
}


preprocessingServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns

      # showhtml <- reactiveVal(value = FALSE)
      showbeha <- reactiveVal(value = FALSE)
      showdata <- reactiveVal(value = FALSE)


      data_beha <- reactive({
        req(input$file_beha)

        #read.csv("G:/Programming/R_VIS/preprocessing/behavioral_data_20201120.csv", header = TRUE, sep = ";")
        #read.csv(input$file_beha$datapath, header = TRUE, sep = input$delimiter_beha, check.names = FALSE)
        fread(input$file_beha$datapath, header = TRUE, sep = input$delimiter_beha, check.names = FALSE)

        #ext <- tools::file_ext(input$file$name)
        # switch(ext,
        #        csv = vroom::vroom(input$file$datapath, delim = ";"),
        #        tsv = vroom::vroom(input$file$datapath, delim = "\t"),
        #        validate("Invalid file; Please upload a .csv or .tsv file")
        # )
      })

      data_data <- reactive({
        req(input$file_data)
        #fread(input$file_data$datapath, header = TRUE, sep = input$delimiter_data, check.names = FALSE)
        #read.csv(input$file_data$datapath, header = TRUE, sep = input$delimiter_data, check.names = FALSE)
      })


      observeEvent(input$estimate, {
        cat(file=stderr(), paste0("dir in which data dir will be created = ",g_datarootpath(),"\n" ))
        req(input$outputname)

        # diese Funktion fuert das preprocessing durch und speichert alle Dateien
        cat(file = stderr(), "perform_preprocessing2 now... please wait ...\n")
        perform_preprocessing2(g_datarootpath(),
                               df_BD = data_beha(),
                               datafilename = input$file_data$datapath,
                               postfix = input$outputname,
                               inshiny = TRUE)

       # perform_preprocessing(data_beha(), data_data(), myDirName, method = input$dataMethod)
      })









      # output$uipreprocessing <- renderUI({
      #   fluidRow(
      #     column(6,
      #            selectInput(ns("mod_group1"), h4("Select Group1"),
      #                        choices = c("Stats1"), selected = 1)
      #     ),
      #     column(6,
      #            selectInput(ns("mod_group2"), h4("Select Group 2"),
      #                        choices = c("Stats2"), selected = 1)
      #     )
      #   )
      # })


      # observeEvent(input$showhelp, {showhtml(TRUE)    })
      # observeEvent(input$hidehelp, {showhtml(FALSE)   })
      observeEvent(input$showbeha, {showbeha(TRUE)    })
      observeEvent(input$hidebeha, {showbeha(FALSE)   })
      observeEvent(input$showdata, {showdata(TRUE)    })
      observeEvent(input$hidedata, {showdata(FALSE)   })

      output$htmlhelp <- renderUI({
        # if (showhtml()){
          includeMarkdown("./documentation/preprocessing_markdown.md")
        # }
      })

      output$head_beha <- renderTable({
        if (showbeha()) {
          xtable(head(data_beha(), 5))
        }
      })

      output$head_data <- renderTable({
        if (showdata())  data_data()
      })
      # output$head_data <- renderTable(width = "400px", striped =  TRUE, {
      #   if (showdata())  head(data_data(), 5)
      # })
      #


      output$preprocessingText <- renderPrint({
        req(input$mod_group1)
        req(input$mod_group2)
        mylocalfunc()
        cat("my output")
      })
    }
  )
}

mylocalfunc<-function(){
  cat(file = stderr(), "my locale function\n")
}

