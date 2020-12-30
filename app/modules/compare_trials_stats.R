library(shiny)
compareTrialsStatsUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    uiOutput(ns("fluidRow_oben")),
    #uiOutput(ns("uiCompareTrialsStats")),


  )
}

compareTrialsStatsServer<- function(id, input_glob_sig, freq) {
  moduleServer(
    id,
    function(input, output, session) {
      ns<-session$ns
      #f_utrial_list_all <- reactive({ c("all", f_utrial_list()) })
      output$fluidRow_oben <- renderUI({
        cat(file = stderr(), "\n\n")
        cat(file = stderr(), paste0("dim(data)="))
        cat(file = stderr(), dim(g_data()))
        cat(file = stderr(), paste0("input_glob_sig=", input_glob_sig(), "\n"))
        cat(file = stderr(), paste0("freq=", freq(), "\n"))
        fluidPage(
        fluidRow(
          column(3,
                 style = "background-color: #fcfcfc;",
                 #style = 'border-bottom: 2px solid gray',
                 style = "border-right: 2px solid black",
                 h4("Trial comparison", align = "center"),
                 selectInput(ns("group"), h5("Select Group", align = "center"),
                                      choices = g_groups(), selected = g_groups()[2])
                ),
          column(9,

                 )
        ),
        fluidRow(
    column(12,
           box(title = "Stats..........expand for help (comp_trials_stats_markdown.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_Comp_Trials")))
    )
  ),
        fluidRow(
          verbatimTextOutput(ns("textCompareTrialsStats"))
        )
        )
        })


  output$htmlhelp_Comp_Trials <- renderUI({
    # if (showhtml()){
    includeMarkdown(rmarkdown::render("./documentation/comp_trials_stats_markdown.md"))
    # }
  })




      output$textCompareTrialsStats <- renderPrint({
      #output$textCompareTrialsStats <- renderText({
         req(input$group)

        cat(paste0("sig     = ", input_glob_sig(), "\n"))
        cat(paste0("group   = ", input$group, "\n"))
        cat(paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))
        # cat(file = stderr(), paste0("\n\nXXXXsig     = ", input_glob_sig(), "\n"))
        # cat(file = stderr(), paste0("group   = ", input$group, "\n"))
        # cat(file = stderr(), paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))
        f_data_group_freqmean <- reactive({get_data_group_freqmean(g_data(), input$group, freq())})
        data1 = f_data_group_freqmean()
        data2 = f_data_group_freqmean()

        for (trial1_id in 1:length(g_trials())){
          trial2_id = trial1_id + 1
          while (trial2_id<=length(g_trials())){
            cat(paste0(g_trials()[trial1_id]," vs. ", g_trials()[trial2_id],"\n"))
            for (region1_id in 1:length(g_regions())){
              region2_id = region1_id + 1
              while (region2_id <= length(g_regions())){

                #cat(file = stderr(), paste0("dim(data1) = ", dim(data1), " \n"))
                #cat(file = stderr(), paste0("dim(data2) = ", dim(data2), " \n"))
                #cat(file = stderr(), paste0("region1 = ", region1_id, "  region2_id = ", region2_id, "\n"))
                #cat(file = stderr(), paste0("trial1 = ", trial1_id, "  trial2_id = ", trial2_id, "\n"))

                x = data1[,region1_id,region2_id,trial1_id]
                y = data2[,region1_id,region2_id,trial2_id]
                # cat(file = stderr(), "x = ")
                # cat(file = stderr(), x)
                # cat(file = stderr(), "\n")
                # cat(file = stderr(), "y = ")
                # cat(file = stderr(), y)
                # cat(file = stderr(), "\n")
                z = t.test(x,y, paired = TRUE)
                t = z$statistic
                p = z$p.value
                if (p<input_glob_sig()){

                  cat(paste0(g_regions()[region1_id], " vs ", g_regions()[region2_id],"\n"))
                  print(z)

                }

                region2_id = region2_id + 1
              }
            }
            trial2_id = trial2_id + 1
          }
        }
      })
    }
  )
}
