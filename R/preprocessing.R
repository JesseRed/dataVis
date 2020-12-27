library(shiny)
library(vroom)


preprocessingUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    actionButton(ns("showhelp"),"showhelp"),
    actionButton(ns("hidehelp"),"hidehelp"),
    htmlOutput(ns("htmlhelp")),

    textInput(ns("delimiter_beha"),"enter delimiter", value = ";"),
    fileInput(ns("file_beha"), label = "behavioral file", accept = c(".csv", ".tsv")),
    actionButton(ns("showbeha"),"show beha"),
    actionButton(ns("hidebeha"),"hide beha"),
    tableOutput(ns("head_beha")),

    textInput(ns("delimiter_data"),"enter delimiter", value = ";"),
    fileInput(ns("file_data"), label = "data file", accept = c(".csv", ".tsv")),
    actionButton(ns("showdata"),"show data"),
    actionButton(ns("hidedata"),"hide data"),
    tableOutput(ns("head_data")),


    textInput(ns("outputname"),"enter output name", value = "Granger01"),
    actionButton(ns("estimate"),"perform preprocessing"),

    useShinyalert(),

    uiOutput(ns("uipreprocessing")),
    verbatimTextOutput(ns("preprocessingText")),
    selectInput(ns("var"), "Variable", choices = c("one", "two"), selected = "two"),
    actionButton(ns("do1"),"Click me")
  )
}


preprocessingServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns

      showhtml <- reactiveVal(value = FALSE)
      showbeha <- reactiveVal(value = FALSE)
      showdata <- reactiveVal(value = FALSE)


      data_beha <- reactive({
        req(input$file_beha)

        #read.csv("G:/Programming/R_VIS/preprocessing/behavioral_data_20201120.csv", header = TRUE, sep = ";")
        read.csv(input$file_beha$datapath, header = TRUE, sep = input$delimiter_beha, check.names = FALSE)

        #ext <- tools::file_ext(input$file$name)
        # switch(ext,
        #        csv = vroom::vroom(input$file$datapath, delim = ";"),
        #        tsv = vroom::vroom(input$file$datapath, delim = "\t"),
        #        validate("Invalid file; Please upload a .csv or .tsv file")
        # )
      })

      data_data <- reactive({
        req(input$file_data)
        read.csv(input$file_data$datapath, header = TRUE, sep = input$delimiter_data, check.names = FALSE)
      })


      observeEvent(input$estimate, {
        mainDir = "./data"
        req(input$outputname)
        if (dir.exists(file.path(mainDir, input$outputname))){
          showNotification("A directory with this name already exist\n please choose a differnt or delete by hand", type= "error")
          shinyalert(title = "Warning",
                     text = "Directory already exist\n if you procede, all content in this directory will be deleted",
                     type = "warning",
                     showCancelButton = TRUE,
                     cancelButtonText = "Cancel",
                     showConfirmButton = TRUE,
                     confirmButtonText = "delete",
                     callbackR = function(x) {
                       cat(file = stderr(), paste0("\nx=",x,"\n"))

                       if(x) {
                         cat(file = stderr(), paste0("delet ... ",file.path(mainDir, input$outputname),"\n"))
                         cat(file = stderr(), paste0("delete all\n"))
                         unlink(file.path(mainDir, input$outputname,"*"))
                       }
                     }
          )

        }else{
          dir.create(file.path(mainDir, input$outputname))
        }
        # diese Funktion fuert das preprocessing durch und speichert alle Dateien
        cat(file = stderr(), "perform_preprocessing now\n")
        perform_preprocessing(data_beha(), data_data(), input$outputname)
      })









      output$uipreprocessing <- renderUI({
        fluidRow(
          column(6,
                 selectInput(ns("mod_group1"), h4("Select Group1"),
                             choices = c("Stats1"), selected = 1)
          ),
          column(6,
                 selectInput(ns("mod_group2"), h4("Select Group 2"),
                             choices = c("Stats2"), selected = 1)
          )
        )
      })


      observeEvent(input$showhelp, {showhtml(TRUE)    })
      observeEvent(input$hidehelp, {showhtml(FALSE)   })
      observeEvent(input$showbeha, {showbeha(TRUE)    })
      observeEvent(input$hidebeha, {showbeha(FALSE)   })
      observeEvent(input$showdata, {showdata(TRUE)    })
      observeEvent(input$hidedata, {showdata(FALSE)   })

      output$htmlhelp <- renderUI({
        if (showhtml()){
          includeMarkdown("./documentation/preprocessing_markdown.md")
        }
      })

      output$head_beha <- renderTable({
        if (showbeha()) {
          head(data_beha(), 5)
        }
      })

      output$head_data <- renderTable({
        if (showdata())  head(data_data(), 5)
      })



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

