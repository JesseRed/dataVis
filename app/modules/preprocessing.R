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
    ),

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

 # box(title = "merge Data", width = 12, collapsible = TRUE, collapsed = TRUE, mergedata(ns("mergedata")))

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
        read.csv(input$file_beha$datapath, header = TRUE, sep = input$delimiter_beha, check.names = FALSE)

        # changed 20211111 because of a problem of Johanna

        #fread(input$file_beha$datapath, header = TRUE, sep = input$delimiter_beha, check.names = FALSE)

        #cat(file = stderr(), paste0("reading by read_csv2(",input$file_beha$datapath,")"))

        # das funktionierte nicht bei kurzen Tabellen
        # read_csv2(input$file_beha$datapath)

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


      output$mergedata <- renderUI({
        # if (showhtml()){
        includeMarkdown("./documentation/preprocessing_markdown.md")
        # }
      })



    }
  )
}


