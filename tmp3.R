library(shiny)
library(DT)


  myUI <- function(id){
    ns<-NS(id)
  tagList(
    uiOutput(ns("fo"))
  )
  }

  myserver <- function(id){
    moduleServer(id,
      function(input, output, session) {
    ns<-session$ns
    output$fo <-renderUI({

      fluidPage(
        title = 'Radio buttons in a table',
        DT::dataTableOutput('foo'),
        verbatimTextOutput('sel')
      )

    })
    m = matrix(
      as.character(1:5), nrow = 12, ncol = 5, byrow = TRUE,
      dimnames = list(month.abb, LETTERS[1:5])
    )
    for (i in seq_len(nrow(m))) {
      m[i, ] = sprintf(
        '<input type="radio" name="%s" value="%s"/>',
        month.abb[i], m[i, ]
      )
    }
    m
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
    output$sel = renderPrint({
      str(sapply(month.abb, function(i) input[[i]]))
    })
  }

)
}


  shinyApp(
  )
