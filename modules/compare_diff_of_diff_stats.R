library(shiny)


compareDiffOfDiffStatsUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    uiOutput(ns("uiCompareGroupsStats"))

    #selectInput(ns("var"), "Variable", choices = c("one", "two"), selected = "two"),
    #actionButton(ns("do1"),"Click me")
  )
}

compareDiffOfDiffStatsServer <- function(id, input_glob_sig, freq) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns
      f_utrial_list_all <- reactive({ c("all", g_trials()) })
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
                 selectInput(ns("mod_method"), h4("method"),
                             choices = c("Stats"), selected = 1)
          )
        ),
        fluidRow(
        column(12,
               box(title = "Stats..........expand for help (comp_diff_stats_markdown.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_Comp_Diff")))
        )
      ),
        fluidRow(
          verbatimTextOutput(ns("textCompareGroupsStats"))
        )
        )
      })



      output$htmlhelp_Comp_Diff <- renderUI({
        # if (showhtml()){
        cat(file = stderr(), "markdown")
        includeMarkdown(rmarkdown::render("./documentation/comp_diff_stats_markdown.md"))
        # }
      })
      # filter data by group
      data_group <- reactive({
        get_data_group(g_data(),input$mod_group1)
      })
      output$textCompareGroupsStats <- renderPrint({
        req(input$mod_group1)
        req(input$mod_group2)
        data1 = reactive({get_data_group_freqmean(g_data(), input$mod_group1, freq())})
        data2 = reactive({get_data_group_freqmean(g_data(), input$mod_group2, freq())})


        cat(paste0("sig     = ", input_glob_sig(), "\n"))
        cat(paste0("group   = ", input$mod_group1, " vs. ", input$mod_group2, "\n"))
        cat(paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))


        if (input$trial == "all"){


          for (trial1_id1 in 1:(length(g_trials())-1)){
            for (trial2_id in (trial1_id1+1):length(g_trials())){
              cat(paste0("#############################\n###TRIAL ",
                         g_trials()[trial1_id1]," vs. ",
                         g_trials()[trial2_id],"###\n#############################\n"))

              significant_result_found = FALSE

              for (region1_id in 1:length(g_regions())){
                region2_id = region1_id + 1
                while (region2_id <= length(g_regions())){


                  x = data1()[,region1_id,region2_id,trial1_id1]-data1()[,region1_id,region2_id,trial2_id]
                  y = data2()[,region1_id,region2_id,trial1_id1]-data2()[,region1_id,region2_id,trial2_id]
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

          }

        }else{
          selected_trial <- reactive({ c(g_trials()[match(input$trial, g_trials())])})
          trial1_id <- reactive({ match(input$trial, g_trials()) })
          other_trials <- reactive({g_trials()[g_trials() != selected_trial()]})

            for (trial2_id in 1:length(g_trials())){

              if ( trial2_id==trial1_id()){
                next
              }

              cat(paste0("#############################\n###TRIAL ",
                         g_trials()[trial1_id()]," vs. ",
                         g_trials()[trial2_id],"###\n#############################\n"))

              significant_result_found = FALSE

              for (region1_id in 1:length(g_regions())){
                region2_id = region1_id + 1
                while (region2_id <= length(g_regions())){


                  x = data1()[,region1_id,region2_id,trial1_id()]-data1()[,region1_id,region2_id,trial2_id]
                  y = data2()[,region1_id,region2_id,trial1_id()]-data2()[,region1_id,region2_id,trial2_id]
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

          }






      })
    }
  )
}

mylocalfunc<-function(){
  cat(file = stderr(), "my locale function")
}

