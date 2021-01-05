library(shiny)


compareGroupsStatsUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    uiOutput(ns("uiCompareGroupsStats")),

  )
}

compareGroupsStatsServer <- function(id, input_glob_sig, freq) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns
      f_utrial_list_all <- reactive({c("all", g_trials())})
      output$uiCompareGroupsStats <- renderUI({
        fluidPage(
        fluidRow(
          column(3,
                 selectInput(ns("mod_group1"), h4("Select Group1"),
                             choices = g_groups(), selected = g_groups()[2])
          ),
          column(3,
                 selectInput(ns("mod_group2"), h4("Select Group 2"),
                             choices = g_groups(), selected = g_groups()[3])
          ),
          column(3,
                 selectInput(ns("trial"), h4("Select Trial"),
                             choices = f_utrial_list_all(), selected = f_utrial_list_all()[1])
          ),
          column(3,
                 #selectInput(ns("mod_method"), h4("visualize method"),
                 #            choices = c("Corrplot", "Heatmap", "Circle", "Pheatmap"), selected = 1)
          )
        ),
        fluidRow(
        column(12,
               box(title = "Stats..........expand for help (comp_groups_stats_markdown.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_Comp_Groups")))
        )
      ),
        fluidRow(
          verbatimTextOutput(ns("textCompareGroupsStats"))
        )
        )
      })


      output$htmlhelp_Comp_Groups <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/comp_groups_stats_markdown.md"))
        # }
      })

      # filter data by group
      data_group <- reactive({
        get_data_group(g_data(),input$mod_group1)
      })


      output$textCompareGroupsStats <- renderPrint({
        req(input$mod_group1)
        req(input$mod_group2)
        req(input$trial)
        data1 = reactive({get_data_group_freqmean(g_data(), input$mod_group1, freq())})
        data2 = reactive({get_data_group_freqmean(g_data(), input$mod_group2, freq())})
        #cat(file=stderr(), paste0("dim(data1)= ", length(dim(data1)),"\n"))
        #cat(file = stderr(), paste0("input_sig = ", input_glob_sig(), "\n"))
        #cat(file = stderr(), paste0("input_sig = ",  input_glob_sig(), "\n"))
        #cat(file = stderr(), paste0("group1 = ", input$mod_group1, "\n"))
        #cat(file = stderr(), paste0("group2 = ", session$ns("mod_group2"), "\n"))
        #cat(file = stderr(), paste0("group2 = ", input$mod_group2, "\n"))
        my_utrial_list <- reactive({
          if (input$trial == "all"){
            return(g_trials())
          }else{
            return(c(g_trials()[match(input$trial, g_trials())]))
          }
        })

        # if (input$trial != "all"){
        #   current_trial_list = g_trials()
        # }else{
        #   g_trials() = c(g_trials()[match(input$trial, g_trials())])
        # }
        #cat(file = stderr(), "current_trial_list = ", current_trial_list, "\n")
        #at(file = stderr(), "selected = ", input$trial, "\n")
        #cat(file = stderr(), class(input$trial), "\n")
        #cat(file = stderr(), class(g_trials()[2]), "\n")
        #xx = match(c(input$trial),g_trials())
        #xx = which(input$trial == v)[[1]]
        #cat(file = stderr(), class(xx))
        #cat(file = stderr(), "selected_num = ", match(input$trial, g_trials()), "\n")

        cat(paste0("sig     = ", input_glob_sig(), "\n"))
        cat(paste0("group   = ", input$mod_group1, " vs. ", input$mod_group2, "\n"))
        cat(paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))


        for (trial1_id in 1:length(my_utrial_list())){
          #for (trial1_id in 1:length(my_utrial_list)){
          cat(paste0("#################\n################\n###TRIAL ",my_utrial_list()[trial1_id],"\n"))

            significant_result_found = FALSE
            #data1 = get_data_group_trial_freqmean(data, input$mod_group1, trial1_id, freq())
            #data2 = get_data_group_trial_freqmean(data, input$mod_group2, trial1_id, freq())
            for (region1_id in 1:length(g_regions())){
              region2_id = region1_id + 1
              while (region2_id <= length(g_regions())){

                #cat(file = stderr(), paste0("length(dim(data1)) = ", length(dim(data1)), " \n"))
                #cat(file = stderr(), paste0("dim(data1) = ", dim(data1), " \n"))
                #cat(file = stderr(), paste0("dim(data2) = ", dim(data2), " \n"))
                #cat(file = stderr(), paste0("trial= ", trial1_id, "region1 = ", region1_id, "  region2_id = ", region2_id, "\n"))

                x = data1()[,region1_id,region2_id,trial1_id]
                y = data2()[,region1_id,region2_id,trial1_id]
                z = t.test(x,y, paired = FALSE)
                t = z$statistic
                p = z$p.value
                if (p<input_glob_sig()){

                  cat(paste0(g_regions()[region1_id], " vs ", g_regions()[region2_id],"\n"))
                  print(z)
                  significant_result_found = TRUE
                }

                region2_id = region2_id + 1
              }
            }
            if (!(significant_result_found)){
              cat("no significant results found \n")
            }
          }




      })
    }
  )
}
