library(shiny)
library(DT)

module_ui = function(id, label) {

  ns = NS(id)

  tagList(
    DT::dataTableOutput(ns('foo')),
    verbatimTextOutput(ns('sel'))
  )

}

module_server = function(input, output, session){

  ns = session$ns
  regions = c("a","B","C","D","E")
  data <- head(iris, 5)
  m = matrix(1:15, nrow = 3, ncol = 5)
  for (i in seq_len(nrow(m))){
    m[i,] = sprintf(
      '<div id="%s">\n <input type="radio" name="%s" value="%s"/>',
      ns(paste0(regions[i])), ns(paste0(regions[i])), m[i, ]
    )
  }
  # m = matrix(1:15, nrow = 3, ncol = 5)
  # for (i in seq_len(nrow(m))){
  #   m[i,] = sprintf(
  #     '<input type="radio" name="%s" value="%s"/>',
  #     ns(paste0("sel",i)), m[i, ]
  #   )
  # }
 # m = matrix(1:4, nrow = 2, ncol = 2)
#  m[1,] <- as.character((radioButtons(ns("myname1"), label = NULL, choices = 1:3, selected = 1, inline = T)))
#  m[2,] <- as.character((radioButtons(ns("myname2"), label = NULL, choices = 1:3, selected = 1, inline = T)))
  #m[3,] <- as.character((radioButtons(ns("myname3"), label = NULL, choices = 1:3, selected = 1, inline = T)))

    # for (i in seq_len(nrow(m))){
  #   for (j in 1:length(ncol(m))){
  #     m[i,j] <- as.character(radioButtons(ns("myname",label = NULL, choices = 1:5)))
  #   m[i,] = sprintf(
  #     '<input type="radio" name="%s" value="%s"/>',
  #     ns(paste0("sel",i)), m[i, ]
  #   )
  #   }
  # }
#  data <- data.frame(m)

  # for (i in 1:nrow(data)) {
  #   data$X1[i] <- as.character(selectInput(ns(paste0("sel", i)), "", choices = unique(iris$Species), width = "100px"))
  # }
  gm<<-m
  output$foo = DT::renderDataTable(
    m, escape = FALSE, selection = 'none', server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE),
    callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
  )
  # output$foo = DT::renderDataTable(
  #   data, escape = FALSE, selection = 'none', server = FALSE,
  #   options = list(dom = 't', paging = FALSE, ordering = FALSE),
  #   callback = JS("table.rows().every(function(i, tab, row) {
  #       var $this = $(this.node());
  #       $this.attr('id', this.data()[0]);
  #       $this.addClass('shiny-input-radiogroup');
  #     });
  #     Shiny.unbindAll(table.table().node());
  #     Shiny.bindAll(table.table().node());")
  # )

  output$sel = renderPrint({
    str(sapply(1:nrow(data), function(i) input[[paste0(regions[i])]]))

    str(input$myname1)
    str(input$myname2)
    str(input$myname3)
    str(input$B)
    x <- paste0("tabl-c")
    str(input[["tabl-B"]])
  })
}

ui <- fluidPage(
  title = 'Selectinput column in a table',
  h3("Source:", tags$a("Yihui Xie", href = "https://yihui.shinyapps.io/DT-radio/")),
  module_ui("tabl")
)

server <- function(input, output, session) {
  callModule(module_server, "tabl")
}

shinyApp(ui, server)

#
# library(shiny)
# library(DT)
#
# module_ui = function(id, label) {
#
#   ns = NS(id)
#
#   tagList(
#     DT::dataTableOutput(ns('foo')),
#     verbatimTextOutput(ns('sel'))
#   )
#
# }
#
# module_server = function(input, output, session){
#
#   ns = session$ns
#
#   data <- head(iris, 5)
#
#   for (i in 1:nrow(data)) {
#     data$species_selector[i] <- as.character(selectInput(ns(paste0("sel", i)), "", choices = unique(iris$Species), width = "100px"))
#   }
#
#   output$foo = DT::renderDataTable(
#     data, escape = FALSE, selection = 'none', server = FALSE,
#     options = list(dom = 't', paging = FALSE, ordering = FALSE),
#     callback = JS("table.rows().every(function(i, tab, row) {
#         var $this = $(this.node());
#         $this.attr('id', this.data()[0]);
#         $this.addClass('shiny-input-container');
#       });
#       Shiny.unbindAll(table.table().node());
#       Shiny.bindAll(table.table().node());")
#   )
#
#   output$sel = renderPrint({
#     str(sapply(1:nrow(data), function(i) input[[paste0("sel", i)]]))
#   })
# }
#
# ui <- fluidPage(
#   title = 'Selectinput column in a table',
#   h3("Source:", tags$a("Yihui Xie", href = "https://yihui.shinyapps.io/DT-radio/")),
#   module_ui("tabl")
# )
#
# server <- function(input, output, session) {
#   callModule(module_server, "tabl")
# }
#
# shinyApp(ui, server)
