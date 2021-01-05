library(shiny)
library(markdown)


options_mod_orderUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("fluidRow_oben")),
  )
}

options_mod_orderServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns


      output$fluidRow_oben <- renderUI({
        fluidPage(

          h1("Order of Regions"), hr(),
          #h5("for the Circle Plot, the first regions is positioned at the 12 o clock position"),
          #h5("all other regions are then spaced clockwise "),

          fluidRow(
            orderInput(inputId = ns('regions'),
                       label = '',
                       items = g_regions(),
                       class = "btn-group-vertical"),
            #verbatimTextOutput(ns('orderregions'))
          ),
          fluidRow(
            actionButton(ns("btn"),"save"),
          ),
          fluidRow(
          ),
          fluidRow(
            column(12,
                   box(title = "Options Regions ..........expand for help (options_regions.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_options_regions"))),
            )
          ),

          )})



      observeEvent(input$regions,{
        cat(file = stderr(), "regions clicked")
      })
      # output$ui_regions <- renderUI({
      #   orderInput(inputId = 'regions', 'Regions start from 12 o clock', items = g_regions())
      # })

      #output$orderregions <- renderPrint({ print(input$regions_order) })


      observeEvent(input$btn,{
        # Render the UI for the orderInput widgets again
        # save here to a global veriable
        #cat(file=stderr(), paste0("out=",input$regions_order,"\n"))
        #cat(file=stderr(), paste0("class(input$regions_order)=",class(input$regions_order),"\n"))
        #cat(file=stderr(), paste0("g_act_data_dir()=",g_act_data_dir(),"\n"))
        # lade die orginalen Daten
        uregion_list_org <<- readRDS(file.path("../data",g_act_data_dir(),"uregion_list.Rda"))
        mdatc_org = readRDS(file.path("../data", g_act_data_dir(), "tbl_data.Rda"))
        # estimate the changes
        uregion_list <<- input$regions_order

        #cat(file=stderr(), paste0("uregion_list_org=",uregion_list_org,"\n"))
        #cat(file=stderr(), paste0("uregion_list=",uregion_list,"\n"))
        #cat(file=stderr(), paste0("class(uregion_list_org)=",class(uregion_list_org),"\n"))
        #cat(file=stderr(), paste0("class(uregion_list)=",class(uregion_list),"\n"))

        cx <<- match(uregion_list, uregion_list_org)
        #cat(file=stderr(), paste0("changes=",cx,"\n"))
        mdat = mdatc_org[,cx,cx,,]
#        x = match(urn, uro)
#        n = m(,x,x,,)
        saveRDS(uregion_list, file = file.path("../data", g_act_data_dir(),"uregion_list.Rda"))
        saveRDS(mdat,         file = file.path("../data", g_act_data_dir(), "tbl_data.Rda"))
        g_reload_rVal(g_reload_rVal()+1)
        #        saveRDS(uregion_list, file = file.path("./data",g_act_method(),"uregion_list.Rda"))
#        saveRDS(mdat,         file = file.path("./data", g_act_method(), "tbl_data.Rda"))
        # speichere nun wieder alles

      })

      output$htmlhelp_options_regions <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/options_regions_markdown.md"))
        # }
      })

    }
  )
}



























