library(shiny)
library(markdown)


options_mod_nameUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("fluidRow_oben")),
  )
}

options_mod_nameServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns
      #new_val <-g_reload_rVal() +1
      #g_reload_rVal(new_val)

      output$fluidRow_oben <- renderUI({
        fluidPage(

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
                     actionButton(ns("btn"),"save")
                     )
          ),
          fluidRow(
            column(12,
                   box(title = "Options Regions ..........expand for help (options_regions.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_options_regions_names"))),
            )
          ),

          )
          })





      observeEvent(input$btn,{
        # cat(file = stderr(), "namelist = \n")
        # cat(file = stderr(), g_regions())
        # cat(file = stderr(), "\n")
        # # Render the UI for the orderInput widgets again
        # save here to a global veriable
        #cat(file=stderr(), paste0("out=",input$regions_order,"\n"))
        #cat(file=stderr(), paste0("class(input$regions_order)=",class(input$regions_order),"\n"))
        #cat(file=stderr(), paste0("g_act_data_dir()=",g_act_data_dir(),"\n"))
        # lade die orginalen Daten

        # uregion_list_org <- readRDS(file.path("./data",g_act_data_dir(),"uregion_list.Rda"))
        #mdatc_org = readRDS(file.path("./data", g_act_data_dir(), "tbl_data.Rda"))
        # estimate the changes
        uregion_list <<- g_regions()
        # cat(file=stderr(), paste0("uregion_list_org=",uregion_list_org,"\n"))
        # cat(file=stderr(), paste0("uregion_list=",uregion_list,"\n"))
        # cat(file=stderr(), paste0("input$selename=",input$selename,"\n"))
        # cat(file=stderr(), paste0("input$textname=",input$textname,"\n"))

        uregion_list[match(input$selename,uregion_list)]<-input$textname


        # cat(file=stderr(), paste0("new uregion_list=",uregion_list,"\n"))
        saveRDS(uregion_list, file = file.path("../data", g_act_data_dir(),"uregion_list.Rda"))
        g_reload_rVal(g_reload_rVal()+1)






#        cat(file=stderr(), paste0("uregion_list=",uregion_list,"\n"))
        #cat(file=stderr(), paste0("class(uregion_list_org)=",class(uregion_list_org),"\n"))
        #cat(file=stderr(), paste0("class(uregion_list)=",class(uregion_list),"\n"))

 #       cx <<- match(uregion_list, uregion_list_org)
        #cat(file=stderr(), paste0("changes=",cx,"\n"))
  #      mdat = mdatc_org[,cx,cx,,]
#        x = match(urn, uro)
#        n = m(,x,x,,)
        #saveRDS(uregion_list, file = file.path("./data", g_act_data_dir(),"uregion_list.Rda"))
        #saveRDS(mdat,         file = file.path("./data", g_act_data_dir(), "tbl_data.Rda"))
#        saveRDS(uregion_list, file = file.path("./data",g_act_method(),"uregion_list.Rda"))
#        saveRDS(mdat,         file = file.path("./data", g_act_method(), "tbl_data.Rda"))
        # speichere nun wieder alles

      })

      output$htmlhelp_options_regions <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/options_regions_names_markdown.md"))
        # }
      })

    }
  )
}



























