library(shinydashboard)
library(shinyalert)
library(vroom)
library(xtable)
library(data.table)

mergedataUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    box(title = "help merge dataa", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp"))),



    fluidRow(
      column(12, fileInput(ns("file_data1"), label = "choose D.Rda file ", accept = c(".Rda"))),
    ),
    fluidRow(
      column(12, fileInput(ns("file_data2"), label = "choose D.Rda file", accept = c(".Rda"))),
    ),

    tableOutput(ns("head_data")),


    fluidRow(
      column(4,
             textInput(ns("outputname"),"enter output name", value = "01"),
             ),
    ),
    fluidRow(
      column(4, actionButton(ns("estimate"),"perform the merge")
             ),
    ),
    useShinyalert(),
   )

}


mergedataServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns<-session$ns

      observeEvent(input$estimate, {
        cat(file = stderr(), "observeEvent(input$estimate in mergedataServer\n")
        cat(file=stderr(), paste0("in mergedataServerdirx in which data dir will be created = ",g_datarootpath(),"\n" ))
        req(input$outputname)

        # diese Funktion fuert das preprocessing durch und speichert alle Dateien
        cat(file = stderr(), "perform_mergedata now... please wait ...\n")
        perform_mergedata(g_datarootpath(),
                          datafilename1 = input$file_data1$datapath,
                          datafilename2 = input$file_data2$datapath,
                          postfix = input$outputname,
                          inshiny = TRUE)
       })

      output$htmlhelp <- renderUI({
        # if (showhtml()){
          includeMarkdown("./documentation/mergedata_markdown.md")
        # }
      })
    }
  )
}


