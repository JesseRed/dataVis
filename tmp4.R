library(shiny)

ui <- fluidPage(
  title = 'Creating a UI from a loop',

  sidebarLayout(
    sidebarPanel(

      lapply(1:5, function(i) {
        selectInput(paste0('a', i), paste0('SelectA', i),
                    choices = sample(LETTERS, 5))
      })
    ),

    mainPanel(
      verbatimTextOutput('a_out'),

      br(),
      fluidRow(
        column(2,),
        column(10,h4("A"),h4("B"), h4("C"),h4("D")),
      ),

      uiOutput("fluid"),
      br(),


      actionButton("yesall","YES ALL SELECT"),
      actionButton("noall","NO ALL SELECT"),

    )
  )
)


server <- function(input, output, session) {

  output$a_out <- renderPrint({
    res <- lapply(1:5, function(i) input[[paste0('a', i)]])
    str(setNames(res, paste0('a', 1:5)))
  })
  regions = c("parietal_links", "central", "parietal", "sadfasdf")

  lapply(1:10, function(i) {
    output[[paste0('b', i)]] <- renderUI({
      strong(paste0('Hi, this is output B#', i))
    })
  })

  output$fluid <- renderUI({
    lapply(1:5, function(i) {
      fluidRow(
        tags$head(
          tags$style(type="text/css",
                     "label.control-label, .selectize-control.single {
         display: table-cell;
         text-align: center;
         vertical-align: middle;
         background-color: #fcfcfc;
         border-top: 2px solid gray;
      }
      label.control-label {
        padding-right: 10px;
      }
      .form-group {
        display: table-row;
      }
      .selectize-control.single div.item {
        padding-right: 5px;
      }")
        ),
        # tags$head(
        #   tags$style(type="text/css", "label{ display: table-cell; text-align: left; vertical-align: middle; } .form-group { display: table-row;}")
        # ),

         column(2,

           h4(regions[i])

        ),
        column(10,

             radioButtons(paste0('c', i),
                          label = NULL,
                          choices = 1:10,
                          selected = character(1),
                          inline = T)

        )
      )
    })

  })




  observeEvent(input$yesall,{
    lapply(1:5, function(i) {
      updateRadioButtons(session,paste0('c', i), label = "",choices = list("Yes" = "yes", "No" = "may be"),selected = "yes",inline = T)
    })
  })

  observeEvent(input$noall,{
    lapply(1:5, function(i) {
      updateRadioButtons(session,paste0('c', i), label = "",choices = list("Yes" = "yes", "No" = "may be"),selected = "may be",inline = T)
    })
  })
}

shinyApp(ui,server)
