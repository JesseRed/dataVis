library(shiny)
library(markdown)


optionsUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("fluidRow_oben")),
  )
}

optionsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns


      output$fluidRow_oben <- renderUI({
        fluidPage(
          fluidRow(column(6,
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
          ),

          ),
          column(6,




          h1("Naming of Regions"), hr(),
          #h5("for the Circle Plot, the first regions is positioned at the 12 o clock position"),
          #h5("all other regions are then spaced clockwise "),

          fluidRow(
            column(5,
                   selectInput(ns("selename"), h5("Select Region", align = "center"),
                               choices = g_regions())
            ),
            column(5,
                   textInput(ns("textname"), h5("enter new name", align = "center"))
            ),
            column(2,

            )
          ),

)
        ),
fluidRow(
  style = "padding-top:20px",
  column(6,
         fluidRow(column(3),column(2, actionButton(ns("btn"),"save")))),
  column(6,
         fluidRow(column(4),column(2, actionButton(ns("btn_naming"),"save")))
        )
  ),
fluidRow(
  style = "padding-top:20px",
  column(12,
         box(title = "Options Regions ..........expand for help (options_regions.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_options_regions_names"))),
  )
)


)
      })





      observeEvent(input$btn_naming,{

        D <- readRDS(file = file.path(g_act_data_dir(),"D.Rda"))
        uregion_list <<- g_regions()

        uregion_list[match(input$selename,uregion_list)]<-input$textname
        D$uregion_list <- uregion_list


        # cat(file=stderr(), paste0("new uregion_list=",uregion_list,"\n"))
        saveRDS(D, file = file.path(g_act_data_dir(),"D.Rda"))
        #saveRDS(uregion_list, file = file.path("../data", g_act_data_dir(),"uregion_list.Rda"))
        g_reload_rVal(g_reload_rVal()+1)




      })








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


        D <- readRDS(file = file.path(g_act_data_dir(),"D.Rda"))
        uregion_list_org <- D$uregion_list

        mdatc_org <- D$mdat
        #cat(file = stderr(), paste0("D$mdat[1,3,4,1,1]=",D$mdat[1,3,4,1,1],"\n"))
        #cat(file = stderr(), paste0("D$mdat[1,3,5,1,1]=",D$mdat[1,3,5,1,1],"\n"))
        #cat(file = stderr(), paste0("D$mdat[1,3,6,1,1]=",D$mdat[1,3,6,1,1],"\n"))
        #cat(file = stderr(), paste0("D$mdat[1,3,7,1,1]=",D$mdat[1,3,7,1,1],"\n"))

        uregion_list <- input$regions_order
        #cat(file = stderr(), paste0("uregion_list_org = ", uregion_list_org,"\n"))
        #cat(file = stderr(), paste0("uregion_list = ", uregion_list,"\n"))
        mysaveuregion_list<- uregion_list
        cx <- match(uregion_list, uregion_list_org)
        mdat = mdatc_org[,cx,cx,,]
        D$mdat = mdat
        D$uregion_list = uregion_list
        uregion_list_named = list()
        uregion_list_named[uregion_list] = 1:length(uregion_list)
        D$uregion_list_named = uregion_list_named
        # cat(file = stderr(), paste0("D$uregion_list = ", D$uregion_list,"\n"))
        # cat(file = stderr(), paste0("D$uregion_list_named = ", D$uregion_list_named,"\n"))
        # cat(file = stderr(), paste0("D$mdat[1,3,4,1,1]=",D$mdat[1,3,4,1,1],"\n"))
        # cat(file = stderr(), paste0("D$mdat[1,3,5,1,1]=",D$mdat[1,3,5,1,1],"\n"))
        # cat(file = stderr(), paste0("D$mdat[1,3,6,1,1]=",D$mdat[1,3,6,1,1],"\n"))
        # cat(file = stderr(), paste0("D$mdat[1,3,7,1,1]=",D$mdat[1,3,7,1,1],"\n"))

        saveRDS(D, file = file.path(g_act_data_dir(),"D.Rda"))
#        saveRDS(mdat,         file = file.path("../data", g_act_data_dir(), "tbl_data.Rda"))
        g_reload_rVal(g_reload_rVal()+1)

      })

      output$htmlhelp_options_regions <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/options_regions_markdown.md"))
        # }
      })

    }
  )
}



























