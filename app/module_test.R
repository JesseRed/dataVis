library(shinydashboard)



testUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.

  fluidRow(
    shinydashboard::box(
      title = "Select Cols",
      selectInput("select", "Select columns", names(mtcars), multiple = TRUE)
    )
    ,
    shinydashboard::box(
      title = "Data Viewer",
      width = 10,
      DT::dataTableOutput(ns('data_table'))
    )
  )
    # tagList(
  #
  #   uiOutput(ns("uipreprocessing")),
  #   verbatimTextOutput(ns("preprocessingText")),
  #   selectInput(ns("var"), "Variable", choices = c("one", "two"), selected = "two"),
  #   actionButton(ns("do1"),"Click me")

}


testServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns

      showhtml <- reactiveVal(value = FALSE)


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




      output$preprocessingText <- renderPrint({
       print("my text")
        cat("my output")
      })
    }
  )
}

mylocalfunc<-function(){
  cat(file = stderr(), "my locale function\n")
}

