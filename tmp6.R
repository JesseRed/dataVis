# library(shiny)
#
# ui <- fluidPage(
#
#   fluidRow(
#
#     tags$head(
#       tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
#     ),
#
#     textInput(inputId = "txtInp", label = "Label:"),
#     numericInput(inputId = "numInp", label = "Label:", value = 0)
#   )
# )
#
# server <- function(input, output){}
#
#
# shinyApp(ui, server)

#
# library(shiny)
#
# ui <- fluidPage(
#   tags$head(
#     tags$style(type="text/css",
#                "label.control-label, .selectize-control.single {
#          display: table-cell;
#          text-align: center;
#          vertical-align: middle;
#       }
#       label.control-label {
#         padding-right: 10px;
#       }
#       .form-group {
#         display: table-row;
#       }
#       .selectize-control.single div.item {
#         padding-right: 15px;
#       }")
#   ),
#   selectInput("variable", "Variable", c("Cylinders", "Transmission", "Gears"))
# )
#
# server <- function(input, output, server) {}
# shinyApp(ui, server)



# libraries ---------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)


# module UI ---------------------------------------------------------------

moduleUI <- function(id) {

  ns <- NS(id)

  fluidPage(
    DT::dataTableOutput(ns("moduleOutput")),
    tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                               Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                               })")),
    DT::dataTableOutput(ns("moduleWithRatingOutput"))
  ) # close the page

}


# module server -----------------------------------------------------------

mtcarsModule <- function(input, output, session) {


  # helper function to add interactive elements to rows of a table
  shinyInputOther <- function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  # helper function to extract interactive elements from rows of a table
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }


  # reactive data
  mtcarsReactive <- reactive({


    head(mtcars)

  }) # close reactive


  # reactive data output - allows adding a rating to each row/car
  output$moduleOutput <- DT::renderDataTable({

    mtcarsReactive() %>%
      mutate(
        rating = shinyInputOther(FUN = selectInput,
                                 len = nrow(mtcarsReactive()),
                                 id = 'rating_',
                                 choices=c("high", "med", "low"),
                                 width = "60px")
      )
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F,
                 preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
                 drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
  ),
  rownames = F) # close output


  # reactive data with added rating for each row/car
  mtcarsWithRating <- reactive({

    mtcarsReactive() %>%
      mutate(
        rating = shinyValue("rating_",
                            nrow(mtcarsReactive())
        )
      )

  }) # close reactive


  # reactive data output - includes rating for each row/car
  output$moduleWithRatingOutput <- DT::renderDataTable({

    mtcarsWithRating()

  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F
  ),
  rownames = F) # close output

} # close module


# app UI ------------------------------------------------------------------

ui <- navbarPage("mtcars",

                 # app tab
                 tabPanel("app",
                          fluidPage(
                            DT::dataTableOutput("mtcarsOutput"),
                            tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                               Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                               })")),
                            DT::dataTableOutput("mtcarsWithRatingOutput")
                          ) # close the page
                 ), # close tab


                 # module tab
                 tabPanel("module",
                          moduleUI("mtcarsModule")
                 ) # close tab

) # close navbarPage


# app server --------------------------------------------------------------

server <- function(input, output) {


  # helper function to add interactive elements to rows of a table
  shinyInputOther <- function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  # helper function to extract interactive elements from rows of a table
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }
